(include "../utils/utilities.scm")

(define (binding? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (pair? (cdr expr))
       (null? (cddr expr))))

(define (binding-list? expr)
  (all? binding? expr))
