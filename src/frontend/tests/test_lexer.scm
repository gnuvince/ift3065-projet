(load "token.scm")
(load "lexer.scm")
(load "tests/tests.scm")

;;;; Tests
(define (symbols string) (map token-symbol (lex-from-string string)))

(define (test-eof)
  (and
   (eq? (token-type (get-token (make-stream ""))) 'eof)
   (null? (lex-from-string ""))))

(define (test-open-paren)
  (equal? (symbols "(") '((punctuation . open-paren))))

(define (test-close-paren)
  (equal? (symbols ")") '((punctuation . close-paren))))

(define (test-quote)
  (and
   (equal? (symbols "'") '((punctuation . quote-prefix)))
   (equal? (symbols "'x") '((punctuation . quote-prefix) (ident . x)))))

(define (test-backquote)
  (and
   (equal? (symbols "`") '((punctuation . quasiquote-prefix)))
   (equal? (symbols "`x") '((punctuation . quasiquote-prefix) (ident . x)))))

(define (test-comma)
  (and
   (equal? (symbols ",") '((punctuation . unquote-prefix)))
   (equal? (symbols ",x") '((punctuation . unquote-prefix) (ident . x)))))

(define (test-dot)
  (and
   (equal? (symbols ".") '((punctuation . dot)))
   (equal? (symbols ".foo") '((punctuation . dot) (ident . foo)))
   (equal? (symbols "f.oo") '((ident . f.oo)))
   ))

(define (test-comma-at)
  (and
   (equal? (symbols "@") '((ident . @)))
   (equal? (symbols ",@") '((punctuation . unquote-splicing-prefix)))))

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
   (equal? (symbols "let*")             '((keyword . let*)))
   (equal? (symbols "letrec")           '((keyword . letrec)))
   (equal? (symbols "do")               '((keyword . do)))
   (equal? (symbols "delay")            '((keyword . delay)))
   (equal? (symbols "quasiquote")       '((keyword . quasiquote)))
   ))

(define (test-whitespace)
  (and
   (equal? (symbols "") '())
   (equal? (symbols "  x  ") '((ident . x)))
   (equal? (symbols "\tx\n\ny ") '((ident . x) (ident . y)))
   ))

(define (test-comment)
  (and
   (equal? (symbols "; hello") '())
   (equal? (symbols "x ; comment \n y") '((ident . x) (ident . y)))))

(define (test-lex)
  (and
   (equal? (symbols "(let ((x 10)) (* x 2)) ; 10 * 2")
           '((punctuation . open-paren)
             (keyword . let)
             (punctuation . open-paren)
             (punctuation . open-paren)
             (ident . x)
             (number . 10)
             (punctuation . close-paren)
             (punctuation . close-paren)
             (punctuation . open-paren)
             (ident . *)
             (ident . x)
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
   (token-error? (car (lex-from-string "[")))
   (token-error? (car (lex-from-string "]")))
   (token-error? (car (lex-from-string "{")))
   (token-error? (car (lex-from-string "}")))
   (token-error? (car (lex-from-string "#\\foo")))
   ))

(define (test-string)
  (and
   (equal? (symbols "\"foo\"") '((string . "foo")))
   (token-error? (car (lex-from-string "\"foo")))
  ))

(define (self-lex)
  (list (lex-from-file "lexer.scm")
        (lex-from-file "test_lexer.scm")))

(define (test-interactif)
  (let ((port (open-output-string)))
    (write (read) port)
    (lex (get-output-string port))))


(define (main)
  (let ((result (run-tests (list test-eof
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
                                 test-lex))))
    (if result
        (exit 0)
        (exit 1))))
