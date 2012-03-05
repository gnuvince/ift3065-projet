;; Free-variable and mutable-variable analyses.

(define (fv expr)
  (match expr

    (,c when (constant? c)
     '())

    ((quote ,x)
     `())

    (,v when (symbol? v)
     (list v))

    ((set! ,v ,E1)
     (union (list v) (fv E1)))

    ((define ,v ,E1)
     (union (list v) (fv E1)))

    ((lambda ,params ,E)
     (difference (fv E) params))

    ((let ,bindings ,E)
     (union (apply union (map (lambda (b) (fv (cadr b))) bindings))
            (difference (fv E) (map car bindings))))

    ((if ,E1 ,E2)
     (union (fv E1) (fv E2)))
    ((if ,E1 ,E2 ,E3)
     (union (fv E1) (fv E2) (fv E3)))

    ((,E0 . ,Es)
     (union (if (primitive? E0)
                '()
                (fv E0))
            (apply union (map fv Es))))

    (,_
     (error "unknown expression" expr))))

(define (mv expr)
  (match expr

    (,c when (constant? c)
     '())

    ((quote ,x)
     `())

    (,v when (symbol? v)
     '())

    ((set! ,v ,E1)
     (union (list v) (mv E1)))

    ((define ,v ,E1)
     (union (list v) (mv E1)))

    ((lambda ,params ,E)
     (mv E))

    ((let ,bindings ,E)
     (union (apply union (map (lambda (b) (mv (cadr b))) bindings))
            (mv E)))

    ((if ,E1 ,E2)
     (union (mv E1) (mv E2)))
    ((if ,E1 ,E2 ,E3)
     (union (mv E1) (mv E2) (mv E3)))

    ((,E0 . ,Es)
     (union (if (primitive? E0) '() (mv E0))
            (apply union (map mv Es))))

    (,_
     (error "unknown expression" expr))))
