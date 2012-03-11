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


    ;; Function calls
    ((,fn . ,args) `(,(simplify fn) ,@(map simplify args)))

    ;; Return all other expressions as is.
    (,_ expr)))
