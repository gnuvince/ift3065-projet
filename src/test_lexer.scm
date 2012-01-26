;;;; Tests
(define (symbols string) (map token-symbol (lex string)))

(define (test-eof)
  (and
   (eq? (token-type (get-token (make-stream ""))) 'eof)
   (null? (lex ""))))

(define (test-open-paren)
  (equal? (symbols "(") '((punctuation . open-paren))))

(define (test-close-paren)
  (equal? (symbols ")") '((punctuation . close-paren))))

(define (test-quote)
  (and
   (equal? (symbols "'") '((punctuation . quote-symbol)))
   (equal? (symbols "'x") '((punctuation . quote-symbol) (ident . "x")))))

(define (test-backquote)
  (and
   (equal? (symbols "`") '((punctuation . backquote)))
   (equal? (symbols "`x") '((punctuation . backquote) (ident . "x")))))

(define (test-comma)
  (and
   (equal? (symbols ",") '((punctuation . comma)))
   (equal? (symbols ",x") '((punctuation . comma) (ident . "x")))))

(define (test-dot)
  (and
   (equal? (symbols ".") '((punctuation . dot)))
   (equal? (symbols ".foo") '((punctuation . dot) (ident . "foo")))
   (equal? (symbols "f.oo") '((ident . "f.oo")))
   ))

(define (test-comma-at)
  (and
   (equal? (symbols "@") '((ident . "@")))
   (equal? (symbols ",@") '((punctuation . comma-at)))))

(define (test-true)
  (equal? (symbols "#t") '((boolean . true))))

(define (test-false)
  (equal? (symbols "#f") '((boolean . false))))

(define (test-keywords)
  (and
   (equal? (symbols "define")           '((keyword . define)))
   (equal? (symbols "else")             '((keyword . else)))
   (equal? (symbols "unquote")          '((keyword . unquote)))
   (equal? (symbols "unquote-splicing") '((keyword . unquote-splicing)))
   (equal? (symbols "quote")            '((keyword . quote)))
   (equal? (symbols "lambda")           '((keyword . lambda)))
   (equal? (symbols "if")               '((keyword . if)))
   (equal? (symbols "set!")             '((keyword . set!)))
   (equal? (symbols "begin")            '((keyword . begin)))
   (equal? (symbols "cond")             '((keyword . cond)))
   (equal? (symbols "and")              '((keyword . and)))
   (equal? (symbols "or")               '((keyword . or)))
   (equal? (symbols "case")             '((keyword . case)))
   (equal? (symbols "let")              '((keyword . let)))
   (equal? (symbols "let*")             '((keyword . let-star)))
   (equal? (symbols "letrec")           '((keyword . letrec)))
   (equal? (symbols "do")               '((keyword . do)))
   (equal? (symbols "delay")            '((keyword . delay)))
   (equal? (symbols "quasiquote")       '((keyword . quasiquote)))
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
           '((punctuation . open-paren)
             (keyword . let)
             (punctuation . open-paren)
             (punctuation . open-paren)
             (ident . "x")
             (number . 10)
             (punctuation . close-paren)
             (punctuation . close-paren)
             (punctuation . open-paren)
             (ident . "*")
             (ident . "x")
             (number . 2)
             (punctuation . close-paren)
             (punctuation . close-paren)))
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
        (c (make-token #f 1 1)))
    (and
     (equal? (token-type a) 'type)
     (equal? (token-value a) 'value)
     (equal? (token-type c) #f)
     (equal? (token-value c) #f))))

(define (test-invalid-tokens)
  (and
   (token-error? (car (lex "[")))
   (token-error? (car (lex "]")))
   (token-error? (car (lex "{")))
   (token-error? (car (lex "}")))
   (token-error? (car (lex "#\\foo")))
   ))

(define (test-string)
  (and
   (equal? (symbols "\"foo\"") '((string . "foo")))
   (token-error? (car (lex "\"foo")))
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
                  test-dot
                  test-comma-at
                  test-true
                  test-false
                  test-keywords
                  test-whitespace
                  test-make-token
                  test-token-accessors
                  test-invalid-tokens
                  test-string
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
