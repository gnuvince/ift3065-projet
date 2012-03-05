(include "../parser.scm")
(include "tests.scm")

(define tests '())

(define (e->string e)
  (with-output-to-string "" (lambda () (write e))))

(define (test-parse e-in e-out)
  (equal? (parse (lex-from-string (e->string e-in)))
          (list e-out)))

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

(run-tests (reverse tests))
