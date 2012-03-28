(include "../ir/match.scm")
(include "../utils/utilities.scm")


(define (reduce f init xs)
  (if (null? xs)
      init
      (f (car xs) (reduce f init (cdr xs)))))

(define (set-union s1 s2)
  (if (null? s1)
      s2
      (if (member (car s1) s2)
          (set-union (cdr s1) s2)
          (cons (car s1)
                (set-union (cdr s1) s2)))))

(define (union . ss)
  (reduce set-union '() ss))


(define (remove x xs)
  (if (null? xs)
      '()
      (if (eqv? x (car xs))
          (cdr xs)
          (cons (car xs)
                (remove x (cdr xs))))))

(define (set-diff s1 s2)
  (if (null? s2)
      s1
      (set-diff (remove (car s2) s1)
                (cdr s2))))

(define (freevar-analysis ast)
  (match ast
    (() (list))

    (,c when (constant? c) (list))

    (,v when (variable? v) (list v))

    ((lambda ,params ,body)
     (set-diff (freevar-analysis body) params))

    ((if ,e1 ,e2) (set-union (freevar-analysis e1)
                             (freevar-analysis e2)))

    ((if ,e1 ,e2 ,e3) (set-union (freevar-analysis e1)
                                 (set-union (freevar-analysis e2)
                                            (freevar-analysis e3))))

    ((,e1 . ,es)
     (set-union (freevar-analysis e1)
                (freevar-analysis es)))

    (,_ (error "match failed: " ast))))


(define (alpha-conv expr)
  (alphac expr '()))

(define (alphac expr env)
  (define (rename v)
    (cond ((assq v env) => cdr)
          (else v)))

  (match expr
    (,const when (constant? const)
            expr)
    (,var when (variable? var)
          (rename var))
    ((set! ,var ,E1)
     `(set! ,(rename var)
            ,(alphac E1 env)))

    ((lambda ,params ,E)
     (let* ((fresh-params
             (map (lambda (p) (cons p (gensym)))
                  params))
            (new-env
             (append fresh-params env)))
       `(lambda ,(map cdr fresh-params)
          ,(alphac E new-env))))

    ((if ,E1 ,E2)
     `(if ,(alphac E1 env) ,(alphac E2 env)))

    ((if ,E1 ,E2 ,E3)
     `(if ,(alphac E1 env)
          ,(alphac E2 env)
          ,(alphac E3 env)))

    ((,E0 . ,Es)
     `(,(if (primitive? E0) E0 (alphac E0 env))
       ,@(map (lambda (e) (alphac e env))
              Es)))
    (,_
     (error "unknown expression" expr))))



(define (mv expr)
  (match expr
    (,const when (constant? const)
            `())
    (,var when (variable? var)
          `())
    ((set! ,var ,E1)
     (set-union (list var) (mv E1)))
    ((lambda ,params ,E)
     (mv E))
    ((if ,E1 ,E2)
     (set-union (mv E1) (mv E2)))
    ((if ,E1 ,E2 ,E3)
     (set-union (mv E1) (set-union (mv E2) (mv E3))))
    ((,E0 . ,Es)
     (set-union (if (primitive? E0) '() (mv E0))
            (apply union (map mv Es))))
    (,_
     (error "unknown expression" expr))))



(define (assign-conv expr)
  (assignc expr
           (set-union (mv expr) (fv expr))))

(define (assignc expr mut-vars)
  (define (mutable? v) (memq v mut-vars))
  (match expr
    (,const when (constant? const)
            expr)
    (,var when (variable? var)
          (if (mutable? var)
              `(car ,var)
               var))
    ((set! ,var ,E1)
     `(set-car! ,var
                ,(assignc E1 mut-vars)))

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
               (assignc E mut-vars)
               `((lambda ,(map car mut-params)
                   ,(assignc E mut-vars))
                 ,@(map (lambda (x) `(list ,(cdr x)))
                        mut-params))))))))
