(include "match.scm")
(include "expand.scm")
(include "alpha-conv.scm")
(include "assign-conv.scm")
(include "closure-conv.scm")
(include "var-analysis.scm")
(include "set.scm")
(include "simplify.scm")

(define (primitive? f)
  (memq f '(cons car cdr set-car! set-cdr!)))

(define make-closure vector)
(define (closure-code c) (vector-ref c 0))
(define (closure-ref c i) (vector-ref c (+ i 1)))
