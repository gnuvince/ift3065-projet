;; Set operations.

(define (union . ss)
  (let loop ((lst ss) (result '()))
    (if (null? lst)
        result
        (loop (cdr lst)
              (union2 result (car lst))))))

(define (union2 s1 s2)
  (cond ((null? s1)
         s2)
        ((member (car s1) s2)
         (union2 (cdr s1) s2))
        (else
         (cons (car s1)
               (union2 (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (cons (car s1)
               (intersect (cdr s1) s2)))
        (else
         (intersect (cdr s1) s2))))

(define (difference s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (difference (cdr s1) s2))
        (else
         (cons (car s1)
               (difference (cdr s1) s2)))))

(define (set-equal? s1 s2)
  (and (null? (difference s1 s2))
       (null? (difference s2 s1))))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))
