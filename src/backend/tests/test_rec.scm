(define (f x)
  (if (= x 0)
      42
      (f (- x 1))))
(f 3)
