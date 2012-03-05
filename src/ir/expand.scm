;; The macro expander.

(define (expand expr)
  (match expr

    (,c when (constant? c)
     c)

    (,v when (symbol? v)
     v)

    ((quote ,x)
     `(quote ,x))

    ((set! ,v ,E1)
     `(set! ,v ,(expand E1)))
    ((set! . ,rest)
     (error "improper set!"))

    ((define (,v . ,params) . ,Es)
     `(define ,v ,(expand `(lambda ,params ,@Es))))
    ((define ,v ,E1)
     `(define ,v ,(expand E1)))
    ((define . ,rest)
     (error "improper define"))

    ((if ,E1 ,E2)
     `(if ,(expand E1) ,(expand E2) #f))
    ((if ,E1 ,E2 ,E3)
     `(if ,(expand E1) ,(expand E2) ,(expand E3)))
    ((if . ,rest)
     (error "improper if"))

    ((begin ,E1)
     (expand E1))
    ((begin ,E1 . ,Es)
     (expand
      (let ((v (gensym)))
        `(let ((,v ,E1)) (begin ,@Es)))))
    ((begin . ,Es)
     (error "improper begin"))

    ((lambda ,params . ,Es)
     `(lambda ,params ,(expand `(begin ,@Es))))
    ((lambda . ,rest)
     (error "improper lambda"))

    ((let ,name ,bindings . ,Es) when (symbol? name)
     (expand
      `((letrec ((,name
                  (lambda ,(map car bindings) ,@Es)))
          ,name)
        ,@(map cadr bindings))))
;; don't convert let to lambda... let is considered a primitive form
;;    ((let ,bindings . ,Es)
;;     (expand
;;      `((lambda ,(map car bindings) ,@Es)
;;        ,@(map cadr bindings))))
    ((let ,bindings . ,Es)
     `(let ,(map (lambda (b) (list (car b) (expand (cadr b)))) bindings)
        ,(expand `(begin ,@Es))))
    ((let . ,rest)
     (error "improper let"))

    ((letrec ,bindings . ,Es)
     (expand
      `(let ,(map (lambda (b) `(,(car b) #f)) bindings)
         ,@(map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)
         ,@Es)))
    ((letrec . ,rest)
     (error "improper letrec"))

    ((cond)
     `#f)
    ((cond (else ,E1 . ,Es))
     (expand
      `(begin ,E1 ,@Es)))
    ((cond (else . ,Es) . ,rest)
     (error "improper else clause"))
    ((cond (,test) . ,rest)
     (expand
      `(or ,test (cond ,@rest))))
    ((cond (,test => ,fn) . ,rest)
     (expand
      (let ((v (gensym)))
        `(let ((,v ,test))
           (if ,v
               (,fn ,v)
               (cond ,@rest))))))
    ((cond (,test => . ,Es) . ,rest)
     (error "improper => clause"))
    ((cond (,test ,E1 . ,Es) . ,rest)
     (expand
      `(if ,test
           (begin ,E1 ,@Es)
           (cond ,@rest))))
    ((cond . ,rest)
     (error "improper cond"))

    ((or)
     `#f)
    ((or ,E1)
     (expand E1))
    ((or ,E1 ,E2 . ,Es)
     (expand
      (let ((v (gensym)))
        `(let ((,v ,E1))
           (if ,v ,v (or ,E2 ,@Es))))))

    ((and)
     `#t)
    ((and ,E1)
     (expand E1))
    ((and ,E1 ,E2 . ,Es)
     (expand
      `(if ,E1 (and ,E2 ,@Es) #f)))

    ((,op . ,Es) when (primitive? op)
     `(,op ,@(map expand Es)))

    ((,E0 . ,Es)
     (map expand (cons E0 Es)))

    (,_
     (error "unknown expression" expr))))

(define (constant? c)
  (match c
    ((quote ,x)
     #t)
    (,x
     (or (number? x)
         (string? x)
         (boolean? x)
         (char? x)))))

(define primitives '(+ - * / eq? = < > <= >=
                     null? pair? cons car cdr set-car! set-cdr! display))

(define (primitive? op)
  (and (symbol? op)
       (memq op primitives)))
