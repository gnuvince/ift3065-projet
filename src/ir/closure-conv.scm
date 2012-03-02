;; Closure-conversion.

(define (closure-conv expr)
  (let ((globals (fv expr)))
    (closurec expr '() globals)))

(define (closurec expr cenv globals)

  (define (cc e)
    (closurec e cenv globals))

  (define (pos id)
    (let ((x (memq id cenv)))
      (and x
           (- (length cenv)
              (length x)))))

  (match expr

    (,c when (constant? c)
     expr)

    ((quote ,x)
     expr)

    (,v when (symbol? v)
     (let ((p (pos v)))
       (if p
           `(closure-ref $this ,p)
           v)))

    ((set! ,v ,E1)
     `(set! ,v ,(cc E1)))

    ((define ,v ,E1)
     `(define ,v ,(cc E1)))

    ((lambda ,params ,E)
     (let ((new-cenv (difference (fv expr) globals)))
       `(make-closure
         (lambda ($this ,@params)
           ,(closurec E new-cenv globals))
         ,@(map cc new-cenv))))

    ((let ,bindings ,E)
     `(let ,(map (lambda (b) `(,(car b) ,(cc (cadr b)))) bindings)
        ,(cc E)))

    ((if ,E1 ,E2)
     `(if ,(cc E1) ,(cc E2)))
    ((if ,E1 ,E2 ,E3)
     `(if ,(cc E1) ,(cc E2) ,(cc E3)))

    ((,E0 . ,Es)
     (if (primitive? E0)
         `(,E0 ,@(map cc Es))
         `(let (($clo ,(cc E0)))
            ((closure-code $clo)
             $clo
             ,@(map cc Es)))))

    (,_
     (error "unknown expression" expr))))
