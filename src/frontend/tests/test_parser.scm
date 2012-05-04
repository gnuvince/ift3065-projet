;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(include "../parser.scm")
(include "tests.scm")

(define tests '())

(define (e->string e)
  (with-output-to-string "" (lambda () (write e))))

(define (test-parse e-in e-out)
  (equal? (parse (lex-from-string (e->string e-in)))
          (list 'begin e-out)))

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
(make-test false '#f #f)
(make-test true '#t #t)
(make-test quote-prefix ''a ''a)
(make-test quasiquote-prefix '`a '`a)
(make-test unquote-prefix ',a ',a)
(make-test unquote-splicing-prefix ',@a ',@a)
(make-test lambda0 '(lambda (x) x) '(lambda (x) x))
(make-test lambda1 '(lambda (x) (+ x 2)) '(lambda (x) (+ x 2)))
(make-test lambda2 '(lambda (x) (+ x 2) (* x 3)) '(lambda (x) (+ x 2) (* x 3)))
(make-test lambda-mult '(lambda (x y) (+ x y)) '(lambda (x y) (+ x y)))
(make-test lambda-null '(lambda () (+ 1 2)) '(lambda () (+ 1 2)))
(make-test lambda-list '(lambda L (+ 1 2)) '(lambda L (+ 1 2)))
(make-test set!-atom '(set! a 42) '(set! a 42))
(make-test set!-expr '(set! a (+ 1 2)) '(set! a (+ 1 2)))

(make-failure-test fail-define0 '(define))
(make-failure-test fail-define1 '(define (f x)))
(make-failure-test fail-define3 '(define x 1 2))

(make-failure-test fail-lambda0 '(lambda))
(make-failure-test fail-lambda1 '(lambda x))
(make-failure-test fail-lambda2 '(lambda (x)))
(make-failure-test fail-lambda3 '(lambda ((x 3)) (+ 1 2)))
(make-failure-test fail-lambda4 '(lambda (1 2) (+ 1 2)))
(make-failure-test fail-lambda5 '(lambda 1 (+ 1 2)))
(make-failure-test fail-lambda6 '(lambda "a" (+ 1 2)))
(make-failure-test fail-lambda7 '(lambda #t (+ 1 2)))
(make-failure-test fail-lambda8 '(lambda #\c (+ 1 2)))

(make-failure-test fail-set!0 '(set!))
(make-failure-test fail-set!1 '(set! x))
(make-failure-test fail-set!2 '(set! 3))
(make-failure-test fail-set!3 '(set! "a"))
(make-failure-test fail-set!4 '(set! #\c))
(make-failure-test fail-set!5 '(set! (+ 1 2) 3))
(make-failure-test fail-set!6 '(set! (x) 3))

(make-test cond-1 '(cond (else x)) '(cond (else x)))
(make-test cond-2 '(cond (a 1) (b 2) (else 3)) '(cond (a 1) (b 2) (else 3)))
(make-test cond-3 '(cond ((eq? a 1) #t)) '(cond ((eq? a 1) #t)))
(make-test cond-4 '(cond ((eq? 1 1) => not)) '(cond ((eq? 1 1) => not)))
(make-failure-test cond-5 '(cond))
(make-failure-test cond-6 '(cond (else 3) (#f 1)))


(make-test and-1 '(and) '(and))
(make-test and-2 '(and x) '(and x))
(make-test and-3 '(and x y) '(and x y))
(make-test and-4 '(and x y z) '(and x y z))
(make-test and-5 '(and (and x y) z) '(and (and x y) z))

(make-test or-1 '(or) '(or))
(make-test or-2 '(or x) '(or x))
(make-test or-3 '(or x y) '(or x y))
(make-test or-4 '(or x y z) '(or x y z))
(make-test or-5 '(or (or x y) z) '(or (or x y) z))


(make-test let-1 '(let () 3) '(let () 3))
(make-test let-2 '(let () 3 4) '(let () 3 4))
(make-test let-3 '(let ((x 3)) x) '(let ((x 3)) x))
(make-test let-4 '(let ((x 3) (y 4)) (cons x y)) '(let ((x 3) (y 4)) (cons x y)))
(make-test let-5 '(let L () 3) '(let L () 3))
(make-test let-6 '(let L ((x 3)) (L (- x 1))) '(let L ((x 3)) (L (- x 1))))
(make-failure-test let-7 '(let))
(make-failure-test let-8 '(let L))
(make-failure-test let-9 '(let L 3))
(make-failure-test let-10 '(let ()))
(make-failure-test let-11 '(let (a)))
(make-failure-test let-12 '(let (a) a))
(make-failure-test let-13 '(let (a 1) a))
(make-failure-test let-14 '(let ((a 1))))
(make-failure-test let-15 '(let L ((a 1))))
(make-failure-test let-16 '(let L K ((a 1)) a))


(make-test let*-1 '(let* () 3) '(let* () 3))
(make-test let*-2 '(let* () 3 4) '(let* () 3 4))
(make-test let*-3 '(let* ((x 3)) x) '(let* ((x 3)) x))
(make-failure-test let*-4 '(let* ((x 3) (y 4)) (cons x y)))
(make-failure-test let*-5 '(let* L () 3))
(make-failure-test let*-6 '(let* L ((x 3)) (L (- x 1))))
(make-failure-test let*-7 '(let*))
(make-failure-test let*-8 '(let* L))
(make-failure-test let*-9 '(let* L 3))
(make-failure-test let*-10 '(let* ()))
(make-failure-test let*-11 '(let* (a)))
(make-failure-test let*-12 '(let* (a) a))
(make-failure-test let*-13 '(let* (a 1) a))
(make-failure-test let*-14 '(let* ((a 1))))
(make-failure-test let*-15 '(let* L ((a 1))))
(make-failure-test let*-16 '(let* L K ((a 1)) a))

(make-test letrec-1 '(letrec () 3) '(letrec () 3))
(make-test letrec-2 '(letrec () 3 4) '(letrec () 3 4))
(make-test letrec-3 '(letrec ((x 3)) x) '(letrec ((x 3)) x))
(make-failure-test letrec-4 '(letrec ((x 3) (y 4)) (cons x y)))
(make-failure-test letrec-5 '(letrec L () 3))
(make-failure-test letrec-6 '(letrec L ((x 3)) (L (- x 1))))
(make-failure-test letrec-7 '(letrec))
(make-failure-test letrec-8 '(letrec L))
(make-failure-test letrec-9 '(letrec L 3))
(make-failure-test letrec-10 '(letrec ()))
(make-failure-test letrec-11 '(letrec (a)))
(make-failure-test letrec-12 '(letrec (a) a))
(make-failure-test letrec-13 '(letrec (a 1) a))
(make-failure-test letrec-14 '(letrec ((a 1))))
(make-failure-test letrec-15 '(letrec L ((a 1))))
(make-failure-test letrec-16 '(letrec L K ((a 1)) a))

(make-test begin-1 '(begin) '(begin))
(make-test begin-2 '(begin a) '(begin a))
(make-test begin-3 '(begin a b) '(begin a b))


(make-test case-1 '(case x (else x)) '(case x (else x)))
(make-test case-2 '(case x ((x) x) (else y)) '(case x ((x) x) (else y)))
(make-test case-3 '(case x ((x1 x2 x3) x) (else y)) '(case x ((x1 x2 x3) x) (else y)))
(make-failure-test case-4 '(case))
(make-failure-test case-5 '(case x))
(make-failure-test case-6 '(case x ()))
(make-failure-test case-7 '(case x (3 4)))
(make-failure-test case-8 '(case x (L 4)))
(make-failure-test case-9 '(case x (else a) ((b) b)))
(make-failure-test case-10 '(case ()))
(make-failure-test case-11 '(case (3 4)))
(make-failure-test case-12 '(case (L 4)))
(make-failure-test case-13 '(case (else a) ((b) b)))


(define (main)
  (display "\nPARSER TESTS\n")
  (if (run-tests (reverse tests))
      (exit 0)
      (exit 1)))
