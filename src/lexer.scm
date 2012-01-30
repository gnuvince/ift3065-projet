;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

;; make-stream :: string -> stream
;;
;; Takes a string and returns a "stream" on that string.  The stream
;; supports two operations:
;; - next: returns the character under the cursor, without moving the cursor.
;;         Reading past the end of the stream returns the nul character.
;; - advance: moves to the cursor to the next character.


(load "token.scm")

(define (make-stream str)
  (let ((current 0)
        (line 1)
        (col 1)
        (size (string-length str)))
    (define (dispatch msg)
      (case msg
        ((next) (if (>= current size)
                    #\nul
                    (string-ref str current)))
        ((advance)
         (begin
           (if (char=? (dispatch 'next) #\newline)
               (begin
                 (set! col 1)
                 (set! line (+ line 1)))
               (set! col (+ col 1)))
           (set! current (+ current 1))))
        ((col) col)
        ((line) line)))
    dispatch))


(define (make-stream-from-port port)
  (let ((line 1)
        (col 1))
    (define (dispatch msg)
      (case msg
        ((next) (let ((c (peek-char port)))
                  (if (eof-object? c)
                      #\nul
                      c)))
        ((advance) (begin
                     (if (char=? (dispatch 'next) #\newline)
                         (begin
                           (set! col 1)
                           (set! line (+ line 1)))
                         (set! col (+ col 1)))
                     (read-char port)))
        ((col) col)
        ((line) line)))
    dispatch))


;; Definition of lexical elements used in Scheme.  The extended
;; characters are taken from R5RS:
;; http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-5.html#%_sec_2.1
(define digit (string->list "0123456789"))
(define alpha (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define extended (string->list "!$%&*+-./:<=>?@^_~"))

;; char-identifier? :: char -> bool
;;
;; Is `c` a character used in an identifier?
(define (char-identifier? c)
  (or (member c digit)
      (member c alpha)
      (member c extended)))


(define (char-delimiter? c)
  (or (member c '(#\( #\) #\" #\;))
      (char-whitespace? c)))


(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (consume-number stream)
  (define (loop n)
    (let ((c (stream 'next)))
      (cond ((char-numeric? c)
             (stream 'advance)
             (loop (+ (char->digit c) (* 10 n))))
            ((char-delimiter? c) n)
            ((char=? c #\nul) n)
            (else #f))))
  (let ((n (loop 0)))
    (if n
        (cons 'number n)
        #f)))

;; consume-eof :: stream -> symbol
(define (consume-eof stream)
  (stream 'advance)
  '(eof . #f))

;; consume-open-paren :: stream -> symbol
(define (consume-open-paren stream)
  (stream 'advance)
  '(punctuation . open-paren))

;; consume-close-paren :: stream -> symbol
(define (consume-close-paren stream)
  (stream 'advance)
  '(punctuation . close-paren))

;; consume-quote :: stream -> symbol
;; TODO: Should we return quote-symbol or quote like the keyword?
(define (consume-quote stream)
  (stream 'advance)
  '(punctuation . quote-symbol))


;; consume-backquote :: stream -> symbol
(define (consume-backquote stream)
  (stream 'advance)
  '(punctuation . backquote))


;; consume-comma :: stream -> symbol
(define (consume-comma stream)
  (stream 'advance)
  (if (char=? (stream 'next) #\@)
      (begin
        (stream 'advance)
        '(punctuation . comma-at))
      '(punctuation . comma)))

;; consume-dot :: stream -> symbol
(define (consume-dot stream)
  (stream 'advance)
  '(punctuation . dot))

;; consume-identifier :: stream -> symbol
;;
;; Read characters, concatenating them into a string, until a
;; non-identifier character is read.
(define (consume-identifier stream)
  (define (loop)
    (cond ((char-identifier? (stream 'next))
           (let ((c (stream 'next)))
             (stream 'advance)
             (cons c (loop))))
          (else '())))
  (string->symbol (list->string (loop))))


;; consume-whitespace :: stream -> ()
;;
;; skip whitespace
(define (consume-whitespace stream)
  (if (char-whitespace? (stream 'next))
      (begin
        (stream 'advance)
        (consume-whitespace stream))))



;; lexeme-keyword :: string -> symbol|bool
;;
;; Keywords taken from R5RS, Section 7.1.1
(define (lexeme-keyword lexeme)
  (let ((p (member lexeme '(define
                             else
                             unquote
                             unquote-splicing
                             quote
                             lambda
                             if
                             set!
                             begin
                             cond
                             and
                             or
                             case
                             let
                             let*
                             letrec
                             do
                             delay
                             quasiquote))))
    (and p
         (cons 'keyword (car p)))))

;; consume-string :: stream -> symbol
(define (consume-string stream)
  (define (loop escaped?)
    (cond
     ((and (not escaped?) (char=? (stream 'next) #\"))
      (begin
        (stream 'advance) ; consume closing double-quote
        '()))

     ((and (not escaped?) (char=? (stream 'next) #\\))
      (begin
        (stream 'advance)
        (loop #t)))

     ((and (not escaped?) (char=? (stream 'next) #\nul)) #f)

     ((not escaped?)
      (let ((c (stream 'next)))
        (stream 'advance)
        (cons c (loop #f))))

     (escaped?
      (let ((c (stream 'next)))
        (stream 'advance)
        (cons (cond
               ((char=? c #\") #\")
               ((char=? c #\n) #\newline)
               ((char=? c #\t) #\tab)
               ((char=? c #\\) #\\))
              (loop #f))))))

  (stream 'advance) ; consume opening double-quote.
  (let ((chars (loop #f)))
    (and (list? chars)
         (cons 'string (list->string chars)))))





;; consume-comment :: stream -> ()
;;
;; Starting with the semi-colon, discard characters until the
;; newline character or the end of the stream.
(define (consume-comment stream)
  (cond
   ((or (char=? #\newline (stream 'next))
        (char=? #\nul (stream 'next)))
    (stream 'advance))
   (else (begin
           (stream 'advance)
           (consume-comment stream)))))



;; consume-hash :: stream -> symbol
;;
;; consume the hash character.  If the next character is t or f,
;; return the corresponding boolean value.  If the next character is a
;; \, consume a character.
(define (consume-hash stream)
  (stream 'advance)                     ; consume the #
  (let ((token (cond
                ((char=? (stream 'next) #\t) (begin (stream 'advance) '(boolean . true)))
                ((char=? (stream 'next) #\f) (begin (stream 'advance) '(boolean . false)))
                ((char=? (stream 'next) #\\) (consume-char stream))
                (else #f))))
    token))



;; consume-char :: stream -> symbol
;;
;; If the character after the \ is non-alphabetic, return the
;; character's ascii code.  Otherwise, try to read a character name.
;; If the character name is a single letter, return that letter's
;; ascii code.  If the character name is a word, return the associated
;; character code.
(define (consume-char stream)
  (define (read-char-name stream)
    (let ((c (stream 'next)))
      (cond ((char-alphabetic? c) (stream 'advance) (cons c (read-char-name stream)))
            (else '()))))

  (stream 'advance)                     ; consume the \
  (let* ((c (stream 'next))
         (char-code
          (cond
           ((char-alphabetic? c)
            (let ((chars (read-char-name stream)))
              (cond ((null? chars) #f)
                    ((= 1 (length chars)) (char->integer (car chars)))
                    ((string=? (list->string chars) "nul") (char->integer #\nul))
                    ((string=? (list->string chars) "tab") (char->integer #\tab))
                    ((string=? (list->string chars) "newline") (char->integer #\newline))
                    ((string=? (list->string chars) "space") (char->integer #\space))
                    (else #f))))
           (else
            (stream 'advance)
            (char->integer c)))))
    (if char-code
        (cons 'char char-code)
        #f)))




(define (skip-whitespace-and-comments stream)
  (cond ((char-whitespace? (stream 'next))
         (consume-whitespace stream)
         (skip-whitespace-and-comments stream))
        ((char=? (stream 'next) #\;)
         (consume-comment stream)
         (skip-whitespace-and-comments stream))))


;; get-token :: stream -> token
;;
;; Consume characters from the stream and return the next token.
;; Invalid characters return #f.
(define (get-token stream)
  (skip-whitespace-and-comments stream)
  (let* ((line (stream 'line))
         (col (stream 'col))
         (symbol
          (cond
           ((char=? (stream 'next) #\nul) (consume-eof stream))

           ((char=? (stream 'next) #\()   (consume-open-paren stream))

           ((char=? (stream 'next) #\))   (consume-close-paren stream))

           ((char=? (stream 'next) #\')   (consume-quote stream))

           ((char=? (stream 'next) #\`)   (consume-backquote stream))

           ((char=? (stream 'next) #\,)   (consume-comma stream))

           ((char=? (stream 'next) #\.)   (consume-dot stream))

           ((char=? (stream 'next) #\")   (consume-string stream))

           ((char=? (stream 'next) #\#)   (consume-hash stream))

           ((char-numeric? (stream 'next)) (consume-number stream))

           ;; Keywords, numbers and identifiers are all read the same way:
           ;; 1. Characters are read until we no longer have a char-identifier;
           ;; 2. If the characters read represent a keyword, return a keyword token;
           ;; 3. If the characters read represent a number, return a number token;
           ;; 4. Otherwise, return an identifier token.
           ((char-identifier? (stream 'next))
            (let* ((ident (consume-identifier stream))
                   (identifier (or (lexeme-keyword ident)
                                   (cons 'ident ident))))
              identifier))
           (else
            (stream 'advance)
            #f))))
    (make-token symbol line col)))



;; lex :: stream -> [token]
;;
;; Call `get-token` until EOF, accumulating tokens into a list.
(define (lex stream)
  (define (loop)
    (let ((token (get-token stream)))
      (cond
       ((eq? (token-type token) 'eof) '())
       (else (cons token (loop))))))
  (loop))


;; lex-from-string :: string -> [token]
;;
;; Construct a stream from a string and tokenize it.
(define (lex-from-string str)
  (let ((stream (make-stream str)))
    (lex stream)))


;; lex-from-file :: filename -> [token]
;;
;; Construct a stream from an input port and tokenize it.
(define (lex-from-file filename)
  (call-with-input-file filename
    (lambda (p)
      (lex (make-stream-from-port p)))))
