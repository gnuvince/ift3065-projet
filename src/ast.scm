;; AST: (head, child*)
;;   - head: (tok prop)
;;   - prop: assoc-list
;;   - tok: token
;;   - child: AST
(define (ast-add-node tok prop)
  (list (list tok prop)))

(define (make-ast) '())

(define (make-ast-node tok prop

(define (make-ast-prop-list) ... )

(define prop? alist?)
  

;; return (tok prop)
(define ast-head car)

;; return tok
(define ast-tok caar)

;; return 
(define ast-prop cadar)

;; return child list
(define ast-childs cdr)

;; return nth child of the ast
(define (ast-nth-child ast n)
  (list-ref (ast-childs ast)
            n))

;; add prop pair to ast
(define (ast-add-prop ast key value)
  (cond ((assq key (ast-prop ast))
         (error (string-append "Key already exists: " key)))
        (else
         (make-ast (ast-head ast)
                   (cons (list key value)
                         (ast-prop ast))))))

;; return
;;   - (key value) of property 'key' of ast if it exists
;;   - #f otherwise
;; uses eq? to test key equality
(define (ast-get-prop ast key)
  (assq key (ast-prop ast)))


(define (ast-get-prop-value ast key)
  (cadr (ast-get-prop ast key)))

(define (ast-set-prop-value ast key)
  (cond 
