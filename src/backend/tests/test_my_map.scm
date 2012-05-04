(define (my-map f xs)
  (cond ((pair? xs) (cons (f (car xs))
                          (my-map f (cdr xs))))
        (else (list))))

(define (my-sum xs)
  (cond ((pair? xs) (+ (car xs)
                       (my-sum (cdr xs))))
        (else 0)))

(my-sum (my-map (lambda (x) (* x 2)) (list 1 2 3)))
