(let ((l (list 1 2 3 4)))
  (+ (car l)
     (+ (car (cdr l))
        (+ (car (cdr (cdr l)))
           (car (cdr (cdr (cdr l))))))))
