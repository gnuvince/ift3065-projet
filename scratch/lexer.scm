;; make-stream : string -> stream
;;
;; Takes a string and returns a "stream" on that string.  The stream
;; supports two operations:
;; - next: returns the character under the cursor, without moving the cursor.
;;         Reading past the end of the stream returns the nul character.
;; - advance: moves to the cursor to the next character.
(define (make-stream str)
  (let ((current 0)
        (size (string-length str)))
    (lambda (msg)
      (case msg
        ((next) (if (>= current size)
                    #\nul
                    (string-ref str current)))
        ((advance) (set! current (+ current 1)))))))


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



;; consume-eof : stream -> token
(define (consume-eof stream)
  (stream 'advance)
  'eof)

;; consume-open-paren : stream -> token
(define (consume-open-paren stream)
  (stream 'advance)
  'open-paren)

;; consume-close-paren : stream -> token
(define (consume-close-paren stream)
  (stream 'advance)
  'close-paren)

;; consume-quote : stream -> token
;; TODO: Should we return quote-symbol or quote like the keyword?
(define (consume-quote stream)
  (stream 'advance)
  'quote-symbol)


;; consume-backquote : stream -> token
(define (consume-backquote stream)
  (stream 'advance)
  'backquote)


;; consume-comma : stream -> token
(define (consume-comma stream)
  (stream 'advance)
  (if (char=? (stream 'next) #\@)
      (begin
        (stream 'advance)
        'comma-at)
      'comma))

;; consume-identifier : stream -> token
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



;; lexeme-keyword : string -> token|bool
;;
;; Keywords taken from R5RS, Section 7.1.1
(define (lexeme-keyword lexeme)
  (let ((p (assoc lexeme '(("define" . define)
                           ("else" . else)
                           ("unquote" . unquote)
                           ("unquote-splicing" . unquote-splicing)
                           ("quote" . quote)
                           ("lambda" . lambda)
                           ("if" . if)
                           ("set!" . set!)
                           ("begin" . begin)
                           ("cond" . cond)
                           ("and" . and)
                           ("or" . or)
                           ("case" . case)
                           ("let" . let)
                           ("let*" . let-star)
                           ("letrec" . letrec)
                           ("do" . do)
                           ("delay" . delay)
                           ("quasiquote" . quasiquote)))))
    (if p (cdr p) #f)))

;; lexeme-numeric : string -> token|bool
(define (lexeme-numeric lexeme)
  (let ((n (string->number lexeme)))
    (if n
        (cons 'number n)
        #f)))



;; consume-string : stream -> token
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




(define (consume-hash stream)
  (stream 'advance)                     ; consume the #
  (let ((token (cond
                ((char=? (stream 'next) #\t) 'true)
                ((char=? (stream 'next) #\f) 'false)
                ((char=? (stream 'next) #\\) (consume-char stream))
                (else #f))))
    (stream 'advance)
    token))





;; tokenize : stream -> token
;;
;; Consume characters from the stream and return the next token.
;; Invalid characters return #f.
(define (tokenize stream)
  (let ((token
         (cond
           ((char=? (stream 'next) #\nul) (consume-eof stream))

           ((char=? (stream 'next) #\()   (consume-open-paren stream))

           ((char=? (stream 'next) #\))   (consume-close-paren stream))

           ((char=? (stream 'next) #\')   (consume-quote stream))

           ((char=? (stream 'next) #\`)   (consume-backquote stream))

           ((char=? (stream 'next) #\,)   (consume-comma stream))

           ((char=? (stream 'next) #\")   (consume-string stream))

           ((char=? (stream 'next) #\#)   (consume-hash stream))

           ((char=? (stream 'next) #\;)
            (begin
              (consume-comment stream)
              (tokenize stream)))

           ;; We'll skip all white space, then call `tokenize`
           ;; recursively to return the next token.
           ((char-whitespace? (stream 'next))
            (begin
              (consume-whitespace stream)
              (tokenize stream)))

           ;; Keywords, numbers and identifiers are all read the same way:
           ;; 1. Characters are read until we no longer have a char-identifier;
           ;; 2. If the characters read represent a keyword, return a keyword token;
           ;; 3. If the characters read represent a number, return a number token;
           ;; 4. Otherwise, return an identifier token.
           ((char-identifier? (stream 'next))
            (let ((ident (consume-identifier stream)))
               (or (lexeme-keyword ident)
                   (lexeme-numeric ident)
                   (cons 'ident ident))))

           (else #f))))
    token))


;; lex : string -> [token]
;;
;; Call `tokenize` until EOF, accumulating tokens into a list.
(define (lex str)
  (let ((stream (make-stream str)))
    (define (loop)
      (let ((token (tokenize stream)))
        (cond
         ((eq? token 'eof) '())
         (else (cons token (loop))))))
    (loop)))






;;;; Tests

(define (test-null)
  (and
   (eq? 'eof (tokenize (make-stream "")))
   (null? (lex ""))))

(define (test-open-paren)
  (equal? (lex "(") '(open-paren)))

(define (test-close-paren)
  (equal? (lex ")") '(close-paren)))

(define (test-quote)
  (and
   (equal? (lex "'") '(quote-symbol))
   (equal? (lex "'x") '(quote-symbol (ident . "x")))))

(define (test-backquote)
  (and
   (equal? (lex "`") '(backquote))
   (equal? (lex "`x") '(backquote (ident . "x")))))

(define (test-comma)
  (and
   (equal? (lex ",") '(comma))
   (equal? (lex ",x") '(comma (ident . "x")))))

(define (test-arobas)
  (and
   (equal? (lex "@") '((ident . "@")))
   (equal? (lex ",@") '(comma-at))))

(define (test-true)
  (equal? (lex "#t") '(true)))

(define (test-false)
  (equal? (lex "#f") '(false)))

(define (test-keywords)
  (and
   (equal? (lex "define") '(define))
   (equal? (lex "else") '(else))
   (equal? (lex "unquote") '(unquote))
   (equal? (lex "unquote-splicing") '(unquote-splicing))
   (equal? (lex "quote") '(quote))
   (equal? (lex "lambda") '(lambda))
   (equal? (lex "if") '(if))
   (equal? (lex "set!") '(set!))
   (equal? (lex "begin") '(begin))
   (equal? (lex "cond") '(cond))
   (equal? (lex "and") '(and))
   (equal? (lex "or") '(or))
   (equal? (lex "case") '(case))
   (equal? (lex "let") '(let))
   (equal? (lex "let*") '(let-star))
   (equal? (lex "letrec") '(letrec))
   (equal? (lex "do") '(do))
   (equal? (lex "delay") '(delay))
   (equal? (lex "quasiquote") '(quasiquote))
   ))

(define (test-whitespace)
  (and
   (equal? (lex "") '())
   (equal? (lex "  x  ") '((ident . "x")))
   (equal? (lex "\tx\n\ny ") '((ident . "x") (ident . "y")))
   ))

(define (test-comment)
  (and
   (equal? (lex "; hello") '())
   (equal? (lex "x ; comment \n y") '((ident . "x") (ident . "y")))))

(define (test-lex)
  (and
   (equal? (lex "(let ((x 10)) (* x 2)) ; 10 * 2")
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


(define (run-tests)
  (and (test-null)
       (test-open-paren)
       (test-close-paren)
       (test-quote)
       (test-backquote)
       (test-comma)
       (test-arobas)
       (test-true)
       (test-false)
       (test-keywords)
       (test-whitespace)
       (test-lex)
       ))
