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
  (let* ((line (stream 'line))
         (col (stream 'col))
         (token
          (cond
           ((char=? (stream 'next) #\nul) (make-token (consume-eof stream) line col))

           ((char=? (stream 'next) #\()   (make-token (consume-open-paren stream) line col))

           ((char=? (stream 'next) #\))   (make-token (consume-close-paren stream) line col))

           ((char=? (stream 'next) #\')   (make-token (consume-quote stream) line col))

           ((char=? (stream 'next) #\`)   (make-token (consume-backquote stream) line col))

           ((char=? (stream 'next) #\,)   (make-token (consume-comma stream) line col))

           ((char=? (stream 'next) #\")   (make-token (consume-string stream) line col))

           ((char=? (stream 'next) #\#)   (make-token (consume-hash stream) line col))

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
            (let* ((ident (consume-identifier stream))
                   (symbol (or (lexeme-keyword ident)
                               (lexeme-numeric ident)
                               (cons 'ident ident))))
              (make-token symbol line col)))

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
         ((eq? (token-type token) 'eof) '())
         (else (cons token (loop))))))
    (loop)))



;; make-token : type+value -> int -> int -> token
(define (make-token t line col)
  (list 'token t line col))


;; token? : any -> bool
(define (token? t)
  (and (list? t)
       (= (length t) 4)
       (eq? (car t) 'token)))

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
    (if (pair? type-value)
        (cdr type-value)
        type-value)))

;; token-symbol : token -> type+value
(define (token-symbol t)
  (list-ref t 1))

;; token-line : token -> int
(define (token-line t)
  (list-ref t 2))

;; token-col : token -> int
(define (token-col t)
  (list-ref t 3))





;;;; Tests
(define (symbols string) (map token-symbol (lex string)))

(define (test-eof)
  (and
   (eq? (token-type (tokenize (make-stream ""))) 'eof)
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
   (equal? (symbols "define") '(define))
   (equal? (symbols "else") '(else))
   (equal? (symbols "unquote") '(unquote))
   (equal? (symbols "unquote-splicing") '(unquote-splicing))
   (equal? (symbols "quote") '(quote))
   (equal? (symbols "lambda") '(lambda))
   (equal? (symbols "if") '(if))
   (equal? (symbols "set!") '(set!))
   (equal? (symbols "begin") '(begin))
   (equal? (symbols "cond") '(cond))
   (equal? (symbols "and") '(and))
   (equal? (symbols "or") '(or))
   (equal? (symbols "case") '(case))
   (equal? (symbols "let") '(let))
   (equal? (symbols "let*") '(let-star))
   (equal? (symbols "letrec") '(letrec))
   (equal? (symbols "do") '(do))
   (equal? (symbols "delay") '(delay))
   (equal? (symbols "quasiquote") '(quasiquote))
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
                  test-lex)))
