(define (make-adder x)
  (lambda (y)
    (+ x y)))

(let ((add-3 (make-adder 3)))
  (add-3 4))
