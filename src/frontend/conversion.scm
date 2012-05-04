(define (improper->proper xs)
  (cond
   ((pair? (cdr xs)) (cons (car xs) (improper->proper (cdr xs))))
   (else (list (car xs) (cdr xs)))))

(define (proper->improper xs)
  (cond
   ((null? (cdr xs)) (car xs))
   (else (cons (car xs)
               (proper->improper (cdr xs))))))



;;;----------------------------------------------------------------------------

;; Define some special forms to define macros that are usable
;; in macro definitions.

(define-macro (for-macro-expansion . body)
  (eval `(begin ,@body))
  #f)

(define-macro (for-here-and-macro-expansion . body)
  `(begin
     (for-macro-expansion ,@body)
     ,@body))

;;;----------------------------------------------------------------------------

;; Define the match special form.

(for-here-and-macro-expansion

  (define-macro (match sujet . clauses)

    (define (if-equal? var gab oui non)
      (cond ((and (pair? gab)
                  (eq? (car gab) 'unquote)
                  (pair? (cdr gab))
                  (null? (cddr gab)))
             `(let ((,(cadr gab) ,var))
                ,oui))
            ((null? gab)
             `(if (null? ,var) ,oui ,non))
            ((symbol? gab)
             `(if (eq? ,var ',gab) ,oui ,non))
            ((or (boolean? gab)
                 (char? gab))
             `(if (eq? ,var ,gab) ,oui ,non))
            ((number? gab)
             `(if (eqv? ,var ,gab) ,oui ,non))
            ((pair? gab)
             (let ((carvar (gensym))
                   (cdrvar (gensym)))
               `(if (pair? ,var)
                    (let ((,carvar (car ,var)))
                      ,(if-equal?
                        carvar
                        (car gab)
                        `(let ((,cdrvar (cdr ,var)))
                           ,(if-equal?
                             cdrvar
                             (cdr gab)
                             oui
                             non))
                        non))
                    ,non)))
            (else
             (error "unknown pattern"))))

    (let* ((var
            (gensym))
           (fns
            (map (lambda (x) (gensym))
                 clauses))
           (err
            (gensym)))
      `(let ((,var ,sujet))
         ,@(map (lambda (fn1 fn2 clause)
                  `(define (,fn1)
                     ,(if-equal? var
                                 (car clause)
                                 (if (and (eq? (cadr clause) 'when)
                                          (pair? (cddr clause)))
                                     `(if ,(caddr clause)
                                          ,(cadddr clause)
                                          (,fn2))
                                     (cadr clause))
                                 `(,fn2))))
                fns
                (append (cdr fns) (list err))
                clauses)
         (define (,err) (error "match failed"))
         (,(car fns)))))

  (define gensym ;; a version of gensym that creates easier to read symbols
    (let ((count 0))
      (lambda ()
        (set! count (+ count 1))
        (string->symbol (string-append "g" (number->string count))))))

)

;;;----------------------------------------------------------------------------

;; The macro expander.

(define (expand expr)
  (match expr

    (,c when (constant? c)
     c)

    (,v when (symbol? v)
     v)

    ;; (,s when (string? s)
    ;;     `(string ,@(string->list s)))

    ((quote ,x)
     `(quote ,x))

    ((set! ,v ,E1)
     `(set! ,v ,(expand E1)))
    ((set! . ,rest)
     (error "improper set!"))

    ((define (,v . ,params) . ,Es)
     (expand `(define ,v (lambda ,params ,@Es))))
    ((define ,v ,E1)
     ;`(define ,v ,(expand E1)))
     (expand `(set! ,v ,E1)))
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

    ((begin ,E1 . ,Es) `(begin ,@(map expand (cons E1 Es))))

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

    ((let* ,bindings . ,Es)
     (match bindings
       (() error "improper let*")
       (((,var ,val)) `(let ((,var ,(expand val))) ,@(expand Es)))
       (((,var ,val) . ,rest) (expand
                               `(let ((,var ,val))
                                  (let* (,@rest) ,@Es))))))

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

(define primitives '(%* %+ %- %< %<= %= %> %>= %car %cdr %cons %eq?
                        %null? %number? %pair? %quotient %remainder
                        %set-car! %set-cdr! %string? %write-char
                        %display %make-string %string-set! %string-ref))

(define (primitive? op)
  (and (symbol? op)
       (memq op primitives)))


;;;----------------------------------------------------------------------------

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

    ((lambda ,params ,E) when (symbol? params)
     (let* ((g (gensym))
            (new-env (cons (cons params g) env)))
     `(lambda ,g ,(alphac E new-env))))

    ((lambda ,params ,E)
     (let* ((params-proper (if (list? params) params (improper->proper params)))
            (fresh-params
             (map (lambda (p) (cons p (gensym)))
                  params-proper))
            (new-env
             (append fresh-params env)))
       `(lambda ,(if (list? params)
                     (map cdr fresh-params)
                     (proper->improper (map cdr fresh-params)))
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

;;;----------------------------------------------------------------------------

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


    ((lambda ,params ,E) when (symbol? params)
     (let* ((mut-params
             (map (lambda (p) (cons p (gensym)))
                  (keep mutable? (list params))))
            (params2
             (map (lambda (p)
                    (if (mutable? p)
                        (cdr (assq p mut-params))
                        p))
                  (list params))))
       `(lambda ,@params2
          ,(if (null? mut-params)
               (ac E)
               `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) (list))))
                           mut-params)
                   ,(ac E))))))


    ((lambda ,params ,E)
     (let* ((proper-params (if (list? params) params (improper->proper params)))
            (mut-params
             (map (lambda (p) (cons p (gensym)))
                  (keep mutable? proper-params)))
            (params2
             (map (lambda (p)
                    (if (mutable? p)
                        (cdr (assq p mut-params))
                        p))
                  proper-params)))
       `(lambda ,(if (list? params)
                     params2
                     (proper->improper params2))
          ,(if (null? mut-params)
               (ac E)
               `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) (list))))
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
               `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) (list))))
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

;;;----------------------------------------------------------------------------

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
         (lambda ($this $num ,@params)
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
             ,(length Es)
             ,@(map cc Es)))))

    (,_
     (error "unknown expression" expr))))

;;;----------------------------------------------------------------------------

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
     (cond
      ((symbol? params) (difference (fv E) (list params)))
      ((list? params) (difference (fv E) params))
      (else (difference (fv E) (improper->proper params)))))

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

    (() '())

    (,_
     (error "[fv] unknown expression" expr))))

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

    (() '())

    (,_
     (error "[mv] unknown expression" expr))))

;;;----------------------------------------------------------------------------

;; Set operations.

(define (union . ss)
  (let loop ((lst ss) (result '()))
    (if (null? lst)
        result
        (loop (cdr lst)
              (union2 result (car lst))))))

(define (union2 s1 s2)
  (cond ((null? s1)
         s2)
        ((member (car s1) s2)
         (union2 (cdr s1) s2))
        (else
         (cons (car s1)
               (union2 (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (cons (car s1)
               (intersect (cdr s1) s2)))
        (else
         (intersect (cdr s1) s2))))

(define (difference s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (difference (cdr s1) s2))
        (else
         (cons (car s1)
               (difference (cdr s1) s2)))))

(define (set-equal? s1 s2)
  (and (null? (difference s1 s2))
       (null? (difference s2 s1))))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))




(define (-> datum . fns)
  (define (loop fns)
    (match fns
      (() (error "must provide at least one function in the pipeline"))
      ((,f) (f datum))
      ((,f ,g . ,rest) (f (loop (cons g rest))))))
  (loop (reverse fns)))


(define (conv expr)
  (-> expr
      expand
      alpha-conv
      assign-conv
      closure-conv
      ))
