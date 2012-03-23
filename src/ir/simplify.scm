;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)


(include "predicates.scm")
(include "match.scm")

(define (simplify expr)
  (match expr
    ;; Expand begin into nested lets.
    ((begin) '())
    ((begin ,E) (simplify E))
    ((begin ,E1 . ,Es)
     (let ((g (gensym)))
       (simplify
        `(let ((,g ,(simplify E1)))
           (begin ,@Es)))))


    ((lambda ,Bs ,E1) `(lambda ,Bs ,(simplify E1)))
    ((lambda ,Bs . ,Es) `(lambda ,Bs ,(simplify `(begin ,@Es))))
    ((lambda ,Bs) (error "ill-formed lambda"))

    ;; Labeled let.
    ((let ,label ,Bs . ,Es) when (and (symbol? label)
                                      (binding-list? Bs))
     (let ((vars (map car Bs))
           (exprs (map cadr Bs)))
       (simplify
        `(letrec ((,label (lambda (,@vars) ,@(simplify Es))))
           (,label ,@exprs)))))
    ;; Regular let.
    ((let () . ,Es) (simplify `(begin ,@Es)))
    ((let ,Bs . ,Es) when (binding-list? Bs)
     (let ((vars (map car Bs))
           (exprs (map (lambda (binding) (simplify (cadr binding))) Bs)))
       `((lambda (,@vars)
           ,(simplify
             `(begin
                ,@Es)))
         ,@exprs)))
    ((let . ,_) (error "ill-formed let expression"))



    ;; Expand let* into nested lets.
    ((let* () . ,Es) (simplify `(let () ,@Es)))
    ((let* (,B1 . ,Bs) . ,Es) when (binding? B1)
     (simplify
      `(let ((,(car B1) ,(simplify (cadr B1))))
         ,(simplify
           `(let* (,@Bs)
              ,@Es)))))
    ((let* . ,_) (error "ill-formed let* expression"))


    ;; Expand letrec into a let + mutations.
    ((letrec () . ,Es) (simplify `(let () ,@Es)))
    ((letrec ,Bs . ,Es) when (binding-list? Bs)
     (let ((vars (map car Bs))
           (exprs (map cadr Bs)))
       (simplify
        `(let (,@(map (lambda (v) (list v #f)) vars))
           ,@(map (lambda (v e) `(set! ,v ,e)) vars exprs)
           ,@(map simplify Es)))))
    ((letrec . ,_) (error "ill-formed letrec expression"))




    ;; Cond expressions simplify to a series of nested ifs.
    ((cond (else . ,Es)) (simplify `(begin ,@Es)))

    ((cond (,condition . ,actions))
     when (not (eq? condition 'else))
     `(if ,(simplify condition)
          ,(simplify `(begin ,@actions))))

    ((cond (,condition . ,actions) . ,other-clauses)
     when (not (eq? condition 'else))
     `(if ,(simplify condition)
          ,(simplify `(begin ,@actions))
          ,(simplify `(cond ,@other-clauses))))

    ((cond . ,_) (error "ill-formed cond expression"))



    ;; Case expressions simplify to an if+memv.
    ((case ,expr (else . ,actions)) (simplify `(begin ,@actions)))
    ((case ,expr (,cases . ,actions) . ,rest)
     when (list? cases)
     (let ((e (gensym)))
       (simplify
        `(let ((,e ,(simplify expr)))
           (if (memv ,e ,cases)
               ,(simplify `(begin ,@actions))
               ,(simplify `(case ,e ,@rest)))))))
    ((case . ,_) (error "ill-formed case expression"))

    ;; or is expanded into a series of nested ifs.
    ((or) #f)
    ((or ,e) (simplify e))
    ((or ,e1 ,e2 . ,es)
     (let ((g (gensym)))
       (simplify
        `(let ((,g ,(simplify e1)))
           (if ,g
               ,g
               ,(simplify `(or ,e2 ,@es)))))))


    ;; and is expanded into a series of nested ifs.
    ((and) #t)
    ((and ,e1) (simplify e1))
    ((and ,e1 ,e2 . ,es)
     `(if ,e1
          ,(simplify `(and ,e2 ,@es))
          #f))


    ((,arith-op . ,rest) when (member arith-op '(+ - * /))
     (simplify-arithmetic-op `(,arith-op ,@rest)))

    ((,compare-op . ,rest) when (member compare-op '(< <= = >= >))
     (simplify-compare-op `(,compare-op ,@rest)))

    ;; Function calls
    ((,fn . ,args) `(,(simplify fn) ,@(map simplify args)))

    ;; Return all other expressions as is.
    (,_ expr)))


;; Comparison operators (<, <=, >=, >, =) can accept an arbitrary number
;; of arguments and are simplified into a cascade of if expressions.
(define (simplify-compare-op form)
  (match form
    ((,op) #t)
    ((,op ,a) #t)
    ((,op ,a ,b . ,rest) `(if (,op ,(simplify a) ,(simplify b))
                              ,(simplify `(,op ,b ,@rest))
                              #f))))


;; Arithmetic operators (+, -, *, /) can accept an arbitrary number
;; of arguments and are simplified into a cascade of operations.
(define (simplify-arithmetic-op form)
  (match form
    ((+) 0)
    ((+ ,n) (simplify n))

    ((-) (error "invalid form for subtraction"))
    ((- ,n) (- 0 (simplify n)))

    ((*) 1)
    ((* ,n) (simplify n))

    ((/) (error "invalid form for division"))
    ((/ ,n) 0)

    ((,op ,a ,b . ,rest)
     `(,op ,(simplify a)
         ,(simplify `(,op ,b ,@rest))))))
