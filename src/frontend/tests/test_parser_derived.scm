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



(make-test cond-1 '(cond (else x)) '(cond (else x)))
(make-test cond-2 '(cond (a 1) (b 2) (else 3)) '(cond (a 1) (b 2) (else 3)))
(make-test cond-3 '(cond ((eq? a 1) #t)) '(cond ((eq? a 1) true)))
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
