;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(include "../simplify.scm")
(include "../../utils/tests.scm")

(define tests '())

(define-macro (make-test name in pattern)
  (let ((n (string->symbol (string-append "test-"
                                          (symbol->string name)))))
    `(begin (define (,n)
              (match (simplify ,in)
                (,pattern #t)
                (,',_ #f)))
            (set! tests (cons ,n tests)))))



(make-test begin-0 '(begin) ())
(make-test begin-1 '(begin a) a)
(make-test begin-2 '(begin a b)
           ((lambda (,_) b) a))
(make-test begin-3 '(begin a b c)
           ((lambda (,_1)
              ((lambda (,_2)
                 c)
               b))
            a))

(make-test let-0 '(let () a) a)
(make-test let-1 '(let ((a 1)) a) ((lambda (a) a) 1))
(make-test let-2 '(let ((a 1) (b 2)) (a b))
           ((lambda (a b)
                 (a b))
            1 2))
(make-test let-3 '(let () a b) ((lambda (,_) b) a))
(make-test let-4 '(let ((a 1)) a b)
           ((lambda (a)
              ((lambda (,_)
                 b)
               a))
            1))


(make-test let*-0 '(let* () a) a)
(make-test let*-1 '(let* ((a 1)) a) ((lambda (a) a) 1))
(make-test let*-2 '(let* ((a 1) (b a)) (a b))
           ((lambda (a)
              ((lambda (b)
                 (a b))
               a))
            1))


(make-test letrec-0 '(letrec () a) a)
(make-test letrec-1 '(letrec ((a 1)) a)
           ((lambda (a)
              ((lambda (,_)
                 a)
              (set! a 1)))
            #f))

(make-test letrec-2 '(letrec ((a 1) (b 2)) (a b))
           ((lambda (a b)
              ((lambda (,_1)
                 ((lambda (,_2)
                    (a b))
                  (set! b 2)))
               (set! a 1)))
            #f #f))

(make-test labeled-let-0 '(let a () a)
           ((lambda (a)
              ((lambda (,_)
                 (a))
               (set! a (lambda () a))))
            #f))

(make-test labeled-let-1 '(let a ((b 1)) a)
           ((lambda (a)
              ((lambda (,_)
                 (a 1))
               (set! a (lambda (b) a))))
            #f))

(run-tests (reverse tests))

(define (main)
  (display "\nSIMPLIFY TESTS\n")
  (if (run-tests (reverse tests))
      (exit 0)
      (exit 1)))
