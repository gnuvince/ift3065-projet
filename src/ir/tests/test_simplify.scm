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
(make-test begin-2 '(begin a b) (let ((,_ a)) b))
(make-test begin-3 '(begin a b c)
           (let ((,_1 a)) (let ((,_2 b)) c)))

(make-test let-0 '(let () a) a)
(make-test let-1 '(let ((a 1)) a) (let ((a 1)) a))
(make-test let-2 '(let ((a 1) (b 2)) (a b))
           (let ((a 1) (b 2)) (a b)))

(make-test let*-0 '(let* () a) a)
(make-test let*-1 '(let* ((a 1)) a) (let ((a 1)) a))
(make-test let*-2 '(let* ((a 1) (b a)) (a b))
           (let ((a 1)) (let ((b a)) (a b))))

(make-test letrec-0 '(letrec () a) a)
(make-test letrec-1 '(letrec ((a 1)) a)
           (let ((a #f)) (let ((,_ (set! a 1))) a)))
(make-test letrec-2 '(letrec ((a 1) (b 2)) (a b))
           (let ((a #f) (b #f))
             (let ((,_1 (set! a 1)))
               (let ((,_2 (set! b 2)))
                 (a b)))))

(make-test labeled-let-0 '(let a () a)
           (let ((a #f))
             (let ((,_ (set! a (lambda () a))))
               (a))))
(make-test labeled-let-1 '(let a ((b 1)) a)
           (let ((a #f))
             (let ((,_ (set! a (lambda (b) a))))
               (a 1))))

(define (main)
  (display "\nSIMPLIFY TESTS\n")
  (if (run-tests (reverse tests))
      (exit 0)
      (exit 1)))
