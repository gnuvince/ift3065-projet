;; Dans les fonctions d'expansion suivantes, "menv" est
;; l'environnement de macro-expansion (une liste d'association qui
;; associe pour chaque macro, son nom à sa fonction d'expansion).

(define (expand expr menv)

  ;; Elimine tous les appels de macro de l'expression "expr", en
  ;; tenant compte que cette expression n'est pas au niveau
  ;; supérieur du programme.

  ;; "expr" est une expression qui peut contenir des appels de macro

  (expand-core (expand-macros expr menv)
               menv))

(define (expand-macros expr menv)

  ;; Elimine les appels de macro à la racine de l'expression "expr".

  ;; "expr" est une expression qui peut contenir des appels de macro
  ;; incluant à sa racine

  (match expr

    ((,E0 . ,Es) when (and (symbol? E0) (assq E0 menv))
     (expand-macros (apply (cdr (assq E0 menv)) ;; appeler la fn d'expansion
                           (cdr expr))
                    menv))

    (,_
     expr)))

(define (expand-core expr menv)

  ;; Fait l'expansion des formes spéciales de base (les "core" forms).

  ;; "expr" est l'expression à expanser (elle ne contient pas d'appel
  ;; de macro à sa racine)

  (match expr

    ((define . ,rest)
     (error "internal define is not supported"))

    ((define-macro . ,rest)
     (error "internal define-macro is not supported"))

    (,c when (constant? c)
     c)

    (,v when (symbol? v)
     v)

    ((quote ,x)
     `(quote ,x))

    ((set! ,v ,E1)
     `(set! ,v ,(expand E1 menv)))
    ((set! . ,rest)
     (error "improper set!"))

    ((if ,E1 ,E2)
     `(if ,(expand E1 menv) ,(expand E2 menv) #f))
    ((if ,E1 ,E2 ,E3)
     `(if ,(expand E1 menv) ,(expand E2 menv) ,(expand E3 menv)))
    ((if . ,rest)
     (error "improper if"))

    ((lambda ,params . ,Es)
     `(lambda ,params ,(expand `(begin ,@Es) menv)))
    ((lambda . ,rest)
     (error "improper lambda"))

    ((begin ,E1)
     (expand E1 menv))
    ((begin ,E1 . ,Es)
     `(begin ,(expand E1 menv)
             ,(expand `(begin ,@Es) menv)))
    ((begin . ,Es)
     (error "improper begin"))

    ((,E0 . ,Es)
     (map (lambda (e) (expand e menv))
          (cons E0 Es)))

    (,_
     (error "unknown expression" expr))))

(define (expand-top expr menv)

  ;; Fait l'expansion des macros et formes spéciales de base pour
  ;; une expression au niveau supérieur du programme source.

  ;; "expr" est l'expression à expanser

  ;; Normalement "expr" est une forme (begin ...) qui contient
  ;; comme sous-expressions, toutes les expressions du programme
  ;; incluant les definitions de macro.

  (match expr

    ((begin)
     `#f)

    ((begin ,E . ,tail)
     (let ((x (expand-macros E menv)))
       (match x

         ((define-macro (,id . ,params) . ,Es) when (symbol? id)
          (expand-top
          `(begin (define-macro ,id (lambda ,params ,@Es))
                  ,@tail)
          menv))
         ((define-macro ,id ,E1) when (symbol? id)
          (expand-top `(begin ,@tail)
                      (cons (cons id (eval E1)) ;; ajout de la macro à menv
                            menv)))
         ((define-macro . ,rest)
          (error "improper define-macro"))

         ((define (,id . ,params) . ,Es) when (symbol? id)
          (expand-top
          `(begin (define ,id (lambda ,params ,@Es))
                  ,@tail)
          menv))
         ((define ,id ,E1) when (symbol? id)
          `(begin (define ,id ,(expand E1 menv))
                  ,(expand-top `(begin ,@tail)
                               menv)))
         ((define . ,rest)
          (error "improper define"))

         (,_
          `(begin ,(expand-core x menv)
                  ,(expand-top `(begin ,@tail) menv))))))

    (,_
     (expand expr menv))))

(define (constant? c)
  (or (number? c)
      (string? c)
      (boolean? c)
      (char? c)))

(define top-menv
  (list

   (cons 'let
         (lambda rest
           (match (cons 'let rest)
             ((let ,name ,bindings . ,Es) when (symbol? name)
              `((letrec ((,name
                          (lambda ,(map car bindings) ,@Es)))
                  ,name)
                ,@(map cadr bindings)))
             ((let ,bindings . ,Es)
              `((lambda ,(map car bindings) ,@Es)
                ,@(map cadr bindings)))
             (,_
              (error "improper let")))))

   (cons 'let*
         (lambda rest
           (match (cons 'let* rest)
             ((let* () . ,Es)
              `(let () ,@Es))
             ((let* ((,v ,E) . ,bindings) . ,Es)
              `(let ((,v ,E))
                 (let* ,bindings ,@Es)))
             (,_
              (error "improper let*")))))

   (cons 'letrec
         (lambda rest
           (match (cons 'letrec rest)
             ((letrec ,bindings . ,Es)
              `(let ,(map (lambda (b) `(,(car b) #f)) bindings)
                 ,@(map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)
                 ,@Es))
             (,_
              (error "improper letrec")))))

   (cons 'cond
         (lambda rest
           (match (cons 'cond rest)
             ((cond)
              `#f)
             ((cond (else ,E1 . ,Es))
              `(begin ,E1 ,@Es))
             ((cond (else . ,Es) . ,rest)
              (error "improper else clause"))
             ((cond (,test) . ,rest)
              `(or ,test (cond ,@rest)))
             ((cond (,test => ,fn) . ,rest)
              (let ((v (gensym)))
                `(let ((,v ,test))
                   (if ,v
                       (,fn ,v)
                       (cond ,@rest)))))
             ((cond (,test => . ,Es) . ,rest)
              (error "improper => clause"))
             ((cond (,test ,E1 . ,Es) . ,rest)
              `(if ,test
                   (begin ,E1 ,@Es)
                   (cond ,@rest)))
             (,_
              (error "improper cond")))))

   (cons 'or
         (lambda rest
           (match (cons 'or rest)
             ((or)
              `#f)
             ((or ,E1)
              E1)
             ((or ,E1 ,E2 . ,Es)
              (let ((v (gensym)))
                `(let ((,v ,E1))
                   (if ,v ,v (or ,E2 ,@Es)))))
             (,_
              (error "improper or")))))

   (cons 'and
         (lambda rest
           (match (cons 'and rest)
             ((and)
              `#t)
             ((and ,E1)
              E1)
             ((and ,E1 ,E2 . ,Es)
              `(if ,E1 (and ,E2 ,@Es) #f))
             (,_
              (error "improper and")))))

   (cons 'quasiquote
         (lambda (form)
           (quasiquotation form 1)))

   ;; Pour traiter toutes les formes de Scheme il faudrait aussi
   ;; définir des macros pour :
   ;;
   ;; - case
   ;; - do
   ;; - delay

   ))

(define (quasiquotation form level)
  (cond ((= level 0)
         form)
        ((pair? form)
         (cond ((eq? (car form) 'quasiquote)
                (quasiquotation-list form (+ level 1)))
               ((eq? (car form) 'unquote)
                (if (= level 1)
                    (cadr form)
                    (quasiquotation-list form (- level 1))))
               ((eq? (car form) 'unquote-splicing)
                (if (= level 1)
                    (error "improper unquote-splicing")
                    (quasiquotation-list form (- level 1))))
               (else
                (quasiquotation-list form level))))
        (else
         (list 'quote form))))

(define (quasiquotation-list lst level)
  (if (pair? lst)
      (let ((first (car lst)))
        (if (and (= level 1)
                 (pair? first)
                 (eq? (car first) 'unquote-splicing))
            (list 'append
                  (cadr first)
                  (quasiquotation (cdr lst) 1))
            (list 'cons
                  (quasiquotation first level)
                  (quasiquotation (cdr lst) level))))
      (quasiquotation lst level)))

(define (expand-program expr)
  (expand-top expr top-menv))

;;;----------------------------------------------------------------------------

(define (eval expr)

  ;; Cette fonction d'évaluation est basée sur un interprète rapide
  ;; qui traite uniquement les formes spéciales de base (les "core"
  ;; forms).  Le même expanseur de macro est utilisé pour transformer
  ;; l'expression "expr" en une expression qui ne contient que des
  ;; formes spéciales de base.

  (eval-rapide (expand expr top-menv)))

(define (eval-rapide expr)

  ;; Interprète rapide qui traite seulement les formes spéciales de
  ;; base de Scheme (les "core" forms).

  ((ev expr gcte) grte))

(define gcte (list 'list 'cons 'car 'cdr 'cadr 'cddr 'caddr 'cadddr 'append
                   'length 'memq 'pair? 'null? 'eq? 'eqv? 'map '+ '- '=
                   'symbol? 'char? 'boolean?  'number? 'gensym 'error 'pp))

(define grte (list  list  cons  car  cdr  cadr  cddr  caddr  cadddr  append
                    length  memq  pair?  null?  eq?  eqv?  map  +  -  =
                    symbol?  char?  boolean?   number?  gensym  error  pp))

(define (cte-extend cte vars) (append vars cte))
(define (rte-extend rte vals) (append vals rte))

(define (cte-lookup cte var)
  (let ((x (memq var cte)))
    (if x (- (length cte) (length x)) (error "unbound" var))))

(define (rte-ref rte pos)
  (car (list-tail rte pos)))

(define (rte-set! rte pos val)
  (set-car! (list-tail rte pos) val))

(define (ev expr cte) ;; interprète rapide
  (match expr

    (,c when (constant? c)
     (lambda (rte) c))

    (,v when (symbol? v)
     (let ((pos (cte-lookup cte v)))
       (lambda (rte) (rte-ref rte pos))))

    ((quote ,x)
     (lambda (rte) x))

    ((set! ,v ,E1)
     (let ((pos (cte-lookup cte v))
           (e (ev E1 cte)))
       (lambda (rte) (rte-set! rte pos (e rte)))))

    ((if ,E1 ,E2 ,E3)
     (let ((e1 (ev E1 cte))
           (e2 (ev E2 cte))
           (e3 (ev E3 cte)))
       (lambda (rte) (if (e1 rte) (e2 rte) (e3 rte)))))

    ((lambda ,params ,E)
     (let* ((ps (parameter-list params))
            (e (ev E (cte-extend cte ps))))
       (if (equal? ps params) ;; pas de paramètre reste?

           (lambda (rte)
             (lambda args (e (rte-extend rte args))))

           (let ((n (length ps)))
             (lambda (rte)
               (lambda args (e (rte-extend rte (arg-list-with-rest-param args n)))))))))

    ((begin ,E1 ,E2)
     (let ((e1 (ev E1 cte))
           (e2 (ev E2 cte)))
       (lambda (rte) (begin (e1 rte) (e2 rte)))))

    ((,E0 . ,Es)
     (let ((e0 (ev E0 cte))
           (es (map (lambda (x) (ev x cte)) Es)))
       (lambda (rte)
         (apply (e0 rte)
                (map (lambda (e) (e rte)) es)))))))

(define (parameter-list params)
  (cond ((null? params)
         '())
        ((pair? params)
         (cons (car params) (parameter-list (cdr params))))
        (else
         (list params)))) ;; paramètre reste

(define (arg-list-with-rest-param args n)
  (if (= n 1)
      (list args)
      (cons (car args)
            (arg-list-with-rest-param (cdr args) (- n 1)))))

