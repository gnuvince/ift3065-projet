;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

;; make-stream : string -> stream
;;
;; Takes a string and returns a "stream" on that string.  The stream
;; supports two operations:
;; - next: returns the character under the cursor, without moving the cursor.
;;         Reading past the end of the stream returns the nul character.
;; - advance: moves to the cursor to the next character.
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



;; Definition of lexical elements used in Scheme.  The extended
;; characters are taken from R5RS:
;; http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-5.html#%_sec_2.1
(define digit (string->list "0123456789"))
(define alpha (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define extended (string->list "!$%&*+-./:<=>?@^_~"))

;; char-identifier? : char -> bool
;;
;; Is `c` a character used in an identifier?
(define (char-identifier? c)
  (or (member c digit)
      (member c alpha)
      (member c extended)))


(define (end-of-token? c)
  (or (char-whitespace? c)
      (char=? c #\nul)))


;; consume-eof : stream -> symbol
(define (consume-eof stream)
  (stream 'advance)
  'eof)

;; consume-open-paren : stream -> symbol
(define (consume-open-paren stream)
  (stream 'advance)
  'open-paren)

;; consume-close-paren : stream -> symbol
(define (consume-close-paren stream)
  (stream 'advance)
  'close-paren)

;; consume-quote : stream -> symbol
;; TODO: Should we return quote-symbol or quote like the keyword?
(define (consume-quote stream)
  (stream 'advance)
  'quote-symbol)


;; consume-backquote : stream -> symbol
(define (consume-backquote stream)
  (stream 'advance)
  'backquote)


;; consume-comma : stream -> symbol
(define (consume-comma stream)
  (stream 'advance)
  (if (char=? (stream 'next) #\@)
      (begin
        (stream 'advance)
        'comma-at)
      'comma))

;; consume-identifier : stream -> symbol
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
  (list->string (loop)))


;; consume-whitespace : stream -> ()
;;
;; skip whitespace
(define (consume-whitespace stream)
  (if (char-whitespace? (stream 'next))
      (begin
        (stream 'advance)
        (consume-whitespace stream))))



;; lexeme-keyword : string -> symbol|bool
;;
;; Keywords taken from R5RS, Section 7.1.1
(define (lexeme-keyword lexeme)
  (let ((p (assoc lexeme '(("define"           . define)
                           ("else"             . else)
                           ("unquote"          . unquote)
                           ("unquote-splicing" . unquote-splicing)
                           ("quote"            . quote)
                           ("lambda"           . lambda)
                           ("if"               . if)
                           ("set!"             . set!)
                           ("begin"            . begin)
                           ("cond"             . cond)
                           ("and"              . and)
                           ("or"               . or)
                           ("case"             . case)
                           ("let"              . let)
                           ("let*"             . let-star)
                           ("letrec"           . letrec)
                           ("do"               . do)
                           ("delay"            . delay)
                           ("quasiquote"       . quasiquote)))))
    (if p (cdr p) #f)))

;; lexeme-numeric : string -> symbol|bool
(define (lexeme-numeric lexeme)
  (let ((n (string->number lexeme)))
    (if n
        (cons 'number n)
        #f)))



;; consume-string : stream -> symbol
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
  (cons 'string (list->string (loop #f))))





;; consume-comment : stream -> ()
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



;; consume-hash : stream -> symbol
;;
;; consume the hash character.  If the next character is t or f,
;; return the corresponding boolean value.  If the next character is a
;; \, consume a character.
(define (consume-hash stream)
  (stream 'advance)                     ; consume the #
  (let ((token (cond
                ((char=? (stream 'next) #\t) (begin (stream 'advance) 'true))
                ((char=? (stream 'next) #\f) (begin (stream 'advance) 'false))
                ((char=? (stream 'next) #\\)
                 (let ((char (consume-char stream)))
                   (and char (cons 'char char))))
                (else #f))))
    token))



;; consume-char : stream -> symbol
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
  (let ((c (stream 'next)))
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



(define (skip-whitespace-and-comments stream)
  (cond ((char-whitespace? (stream 'next))
         (consume-whitespace stream)
         (skip-whitespace-and-comments stream))
        ((char=? (stream 'next) #\;)
         (consume-comment stream)
         (skip-whitespace-and-comments stream))))


;; get-token : stream -> token
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

           ((char=? (stream 'next) #\")   (consume-string stream))

           ((char=? (stream 'next) #\#)   (consume-hash stream))

           ;; Keywords, numbers and identifiers are all read the same way:
           ;; 1. Characters are read until we no longer have a char-identifier;
           ;; 2. If the characters read represent a keyword, return a keyword token;
           ;; 3. If the characters read represent a number, return a number token;
           ;; 4. Otherwise, return an identifier token.
           ((char-identifier? (stream 'next))
            (let* ((ident (consume-identifier stream))
                   (identifier (or (lexeme-keyword ident)
                                   (lexeme-numeric ident)
                                   (cons 'ident ident))))
              identifier))
           (else
            (stream 'advance)
            #f))))
    (make-token symbol line col)))



;; lex : string -> [token]
;;
;; Call `get-token` until EOF, accumulating tokens into a list.
(define (lex str)
  (let ((stream (make-stream str)))
    (define (loop)
      (let ((token (get-token stream)))
        (cond
         ((eq? (token-type token) 'eof) '())
         (else (cons token (loop))))))
    (loop)))



;; make-token : type+value -> int -> int -> token
(define (make-token tv line col)
  (if tv
      (list 'token tv line col)
      (list 'token-error line col)))


;; token? : any -> bool
(define (token? t)
  (and (list? t)
       (= (length t) 4)
       (eq? (car t) 'token)))

(define (token-error? t)
  (and (list? t)
       (eq? (car t) 'token-error)))

;; token-type : token -> type
;;
;; Accessor used to get the type of a token
(define (token-type t)
  (let ((type-value (token-symbol t)))
    (if (pair? type-value)
        (car type-value)
        type-value)))

;; token-value : token -> value|#f
;;
;; Accesor used to get the value of a token.
;; If the token has no associated value (e.g. keywords),
;; #f is returned.
(define (token-value t)
  (let ((type-value (token-symbol t)))
    (and (pair? type-value) (cdr type-value))))

;; token-symbol : token -> type+value
(define (token-symbol t)
  (and (token? t) (list-ref t 1)))

;; token-line : token -> int
(define (token-line t)
  (and (token? t) (list-ref t 2)))

;; token-col : token -> int
(define (token-col t)
  (and (token? t) (list-ref t 3)))





;;;; Tests
(define (symbols string) (map token-symbol (lex string)))

(define (test-eof)
  (and
   (eq? (token-type (get-token (make-stream ""))) 'eof)
   (null? (lex ""))))

(define (test-open-paren)
  (equal? (symbols "(") '(open-paren)))

(define (test-close-paren)
  (equal? (symbols ")") '(close-paren)))

(define (test-quote)
  (and
   (equal? (symbols "'") '(quote-symbol))
   (equal? (symbols "'x") '(quote-symbol (ident . "x")))))

(define (test-backquote)
  (and
   (equal? (symbols "`") '(backquote))
   (equal? (symbols "`x") '(backquote (ident . "x")))))

(define (test-comma)
  (and
   (equal? (symbols ",") '(comma))
   (equal? (symbols ",x") '(comma (ident . "x")))))

(define (test-arobas)
  (and
   (equal? (symbols "@") '((ident . "@")))
   (equal? (symbols ",@") '(comma-at))))

(define (test-true)
  (equal? (symbols "#t") '(true)))

(define (test-false)
  (equal? (symbols "#f") '(false)))

(define (test-keywords)
  (and
   (equal? (symbols "define")           '(define))
   (equal? (symbols "else")             '(else))
   (equal? (symbols "unquote")          '(unquote))
   (equal? (symbols "unquote-splicing") '(unquote-splicing))
   (equal? (symbols "quote")            '(quote))
   (equal? (symbols "lambda")           '(lambda))
   (equal? (symbols "if")               '(if))
   (equal? (symbols "set!")             '(set!))
   (equal? (symbols "begin")            '(begin))
   (equal? (symbols "cond")             '(cond))
   (equal? (symbols "and")              '(and))
   (equal? (symbols "or")               '(or))
   (equal? (symbols "case")             '(case))
   (equal? (symbols "let")              '(let))
   (equal? (symbols "let*")             '(let-star))
   (equal? (symbols "letrec")           '(letrec))
   (equal? (symbols "do")               '(do))
   (equal? (symbols "delay")            '(delay))
   (equal? (symbols "quasiquote")       '(quasiquote))
   ))

(define (test-whitespace)
  (and
   (equal? (symbols "") '())
   (equal? (symbols "  x  ") '((ident . "x")))
   (equal? (symbols "\tx\n\ny ") '((ident . "x") (ident . "y")))
   ))

(define (test-comment)
  (and
   (equal? (symbols "; hello") '())
   (equal? (symbols "x ; comment \n y") '((ident . "x") (ident . "y")))))

(define (test-lex)
  (and
   (equal? (symbols "(let ((x 10)) (* x 2)) ; 10 * 2")
           '(open-paren
             let
             open-paren
             open-paren
             (ident . "x")
             (number . 10)
             close-paren
             close-paren
             open-paren
             (ident . "*")
             (ident . "x")
             (number . 2)
             close-paren
             close-paren))
   ))


(define (test-make-token)
  (let ((a (make-token '(type . value) 1 1))
        (b (make-token 'type 1 1))
        (c (make-token #f 1 1)))
    (and (token? a)
         (token? b)
         (token-error? c))))

(define (test-token-accessors)
  (let ((a (make-token '(type . value) 1 1))
        (b (make-token 'type 1 1))
        (c (make-token #f 1 1)))
    (and
     (equal? (token-type a) 'type)
     (equal? (token-value a) 'value)
     (equal? (token-type b) 'type)
     (equal? (token-value b) #f)
     (equal? (token-type c) #f)
     (equal? (token-value c) #f))))

(define (run-tests)
  (for-each (lambda (t)
              (display t)
              (display ": ")
              (display (if (t) "OK" "FAIL"))
              (newline))
            (list test-eof
                  test-open-paren
                  test-close-paren
                  test-quote
                  test-backquote
                  test-comma
                  test-arobas
                  test-true
                  test-false
                  test-keywords
                  test-whitespace
                  test-make-token
                  test-token-accessors
                  test-lex)))

(define (self-lex)
  (define (loop p s)
    (let ((line (read-line p)))
      (if (eq? line #!eof)
          s
          (loop p (string-append s "\n" line)))))
  (let* ((port (open-input-file "lexer.scm"))
         (tokens (lex (loop port ""))))
    (close-input-port port)
    tokens))
