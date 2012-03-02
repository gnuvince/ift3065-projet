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
