(letrec ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
         (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
  (odd 13))
