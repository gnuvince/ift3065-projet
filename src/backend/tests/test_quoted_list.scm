(let ((a '(1 2 3)))
  (+ (car a)
     (+ (car (cdr a))
        (car (cdr (cdr a))))))
