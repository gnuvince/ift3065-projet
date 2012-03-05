;; Alpha-conversion.

(define (alpha-conv expr)
  (alphac expr '()))

(define (alphac expr env)

  (define (ac e)
    (alphac e env))

  (define (rename v)
    (cond ((assq v env) => cdr)
          (else v)))

  (match expr

    (,c when (constant? c)
     expr)

    ((quote ,x)
     expr)

    (,v when (symbol? v)
     (rename v))

    ((set! ,v ,E1)
     `(set! ,(rename v) ,(ac E1)))

    ((define ,v ,E1)
     `(define ,(rename v) ,(ac E1)))

    ((lambda ,params ,E)
     (let* ((fresh-params
             (map (lambda (p) (cons p (gensym)))
                  params))
            (new-env
             (append fresh-params env)))
       `(lambda ,(map cdr fresh-params)
          ,(alphac E new-env))))

    ((let ,bindings ,E)
     (let* ((fresh-vars
             (map (lambda (b) (cons (car b) (gensym)))
                  bindings))
            (new-env
             (append fresh-vars env)))
       `(let ,(map (lambda (v e) `(,(cdr v) ,(ac (cadr e))))
                   fresh-vars
                   bindings)
          ,(alphac E new-env))))

    ((if ,E1 ,E2)
     `(if ,(ac E1) ,(ac E2)))
    ((if ,E1 ,E2 ,E3)
     `(if ,(ac E1) ,(ac E2) ,(ac E3)))

    ((,E0 . ,Es)
     `(,(if (primitive? E0) E0 (ac E0))
       ,@(map ac Es)))

    (,_
     (error "unknown expression" expr))))
