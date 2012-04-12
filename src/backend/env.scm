(define (make-env)
  (list 0))

(define (env-fs env)
  (car env))

(define (env-symbols env)
  (cdr env))

(define (env-inc-fs env)
  (cons (+ 1 (env-fs env))
        (env-symbols env)))

(define (env-dec-fs env)
  (cons (- (env-fs env) 1)
        (env-symbols env)))

(define (env-add-symbol env s)
  (cons (+ (env-fs env) 1)
        (cons s (env-symbols env))))
