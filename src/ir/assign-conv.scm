;; Assignment-conversion.

(define (assign-conv expr)
  (let ((globals (fv expr)))
    (assignc expr (difference (mv expr) globals))))

(define (assignc expr mut-vars)

  (define (ac e)
    (assignc e mut-vars))

  (define (mutable? v)
    (memq v mut-vars))

  (match expr

    (,c when (constant? c)
     expr)

    ((quote ,x)
     expr)

    (,v when (symbol? v)
     (if (mutable? v) `(car ,v) v))

    ((set! ,v ,E1)
     (if (mutable? v)
         `(set-car! ,v ,(ac E1))
         `(set! ,v ,(ac E1))))

    ((define ,v ,E1)
     `(define ,v ,(ac E1)))

    ((lambda ,params ,E)
     (let* ((mut-params
             (map (lambda (p) (cons p (gensym)))
                  (keep mutable? params)))
            (params2
             (map (lambda (p)
                    (if (mutable? p)
                        (cdr (assq p mut-params))
                        p))
                  params)))
       `(lambda ,params2
          ,(if (null? mut-params)
               (ac E)
               `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) '())))
                           mut-params)
                   ,(ac E))))))

    ((let ,bindings ,E)
     (let* ((vars
             (map car bindings))
            (mut-vars
             (map (lambda (v) (cons v (gensym)))
                  (keep mutable? vars)))
            (vars2
             (map (lambda (v)
                    (if (mutable? v)
                        (cdr (assq v mut-vars))
                        v))
                  vars)))
       `(let ,(map (lambda (v e) `(,v ,(ac (cadr e))))
                   vars2
                   bindings)
          ,(if (null? mut-vars)
               (ac E)
               `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) '())))
                           mut-vars)
                   ,(ac E))))))

    ((if ,E1 ,E2)
     `(if ,(ac E1) ,(ac E2)))
    ((if ,E1 ,E2 ,E3)
     `(if ,(ac E1) ,(ac E2) ,(ac E3)))

    ((,E0 . ,Es)
     `(,(if (primitive? E0) E0 (ac E0))
       ,@(map ac Es)))

    (,_
     (error "unknown expression" expr))))
