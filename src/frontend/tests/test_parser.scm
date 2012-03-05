(include "../parser.scm")
(include "tests.scm")

(define tests '())

(define (e->string e)
  (with-output-to-string "" (lambda () (write e))))

(define (test-parse e-in e-out)
  (equal? (parse (lex-from-string (e->string e-in)))
          (list e-out)))

(define-macro (make-failure-test name in)
  (let ((n (string->symbol (string-append "test-"
                                          (symbol->string name)))))
    `(begin
       (define (,n)
         (with-exception-catcher
          (lambda (e) #t)
          (lambda ()
            (parse (lex-from-string (e->string e-in)))
            #f)))
       (set! tests (cons ,n tests)))))

(define-macro (make-test name in out)
  (let ((n (string->symbol (string-append "test-"
                                          (symbol->string name)))))
    `(begin (define (,n)
              (test-parse ,in ,out))
            (set! tests (cons ,n tests)))))

(make-test number 42 42)
(make-test char #\a #\a)
(make-test string-non-null "allo" "allo")
(make-test string-null "" "")
(make-test string-escapes "\\\\ \\n \\t \\0" "\\\\ \\n \\t \\0")
(make-test symbol 'sym 'sym)
(make-test binop0 '(+) '(+))
(make-test binop1 '(+ 1) '(+ 1))
(make-test binop2 '(+ 1 2) '(+ 1 2))
(make-test binop3 '(+ 1 2 3) '(+ 1 2 3))
(make-test binop4 '(+ 1 2 3 a) '(+ 1 2 3 a))
(make-test define-var '(define a) '(define a))
(make-test define-var-expr1 '(define var (+ 1 2 3)) '(define var (+ 1 2 3)))
(make-test define-var-expr2 '(define var 42) '(define var 42))
(make-test define-var-formals1 '(define (var x y) (+ x y)) '(define (var x y) (+ x y)))
(make-test define-var-formals2 '(define (var) (+ x y)) '(define (var) (+ x y)))
(make-test begin-null '(begin) '(begin))
(make-test begin-definition* '(begin (define a 1) (define b 2)) '(begin (define a 1) (define b 2)))
(make-test false '#f 'false)
(make-test true '#t 'true)
(make-test quote-prefix ''a '(quote-prefix a))
(make-test quasiquote-prefix '`a '(quasiquote-prefix a))
(make-test unquote-prefix ',a '(unquote-prefix a))
(make-test unquote-splicing-prefix ',@a '(unquote-splicing-prefix a))
(make-test lambda '(lambda (x) (+ x 2)) '(lambda (x) (+ x 2)))
(make-test lambda-mult '(lambda (x y) (+ x y)) '(lambda (x y) (+ x y)))
(make-test lambda-null '(lambda () (+ 1 2)) '(lambda () (+ 1 2)))
(make-test lambda-list '(lambda L (+ 1 2)) '(lambda L (+ 1 2)))
(make-test set!-atom '(set! a 42) '(set! a 42))
(make-test set!-expr '(set! a (+ 1 2)) '(set! a (+ 1 2)))

(make-failure-test fail-define0 '(define))
(make-failure-test fail-define1 '(define (f x)))
(make-failure-test fail-define2 '(define a b))
(make-failure-test fail-define3 '(define x 1 2))



(run-tests (reverse tests))
