;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)



;; AST = ('ast-node, <assoc list of attributes>, <list of children>)

;; make-ast :: list -> list -> ast
(define (make-ast attrs children)
  (list 'ast-node
        attrs
        children))


;; token->ast :: token -> ast
(define (token->ast tok)
  (make-ast
   (list (cons 'type (token-type tok))
         (cons 'value (token-value tok))
         (cons 'line (token-line tok))
         (cons 'col (token-col tok)))
   '()))


;; ast-get-attr :: ast -> key -> (key . value)|#f
(define (ast-get-attr ast key)
  (assoc key (ast-get-attrs ast)))


;; ast-get-attr :: ast -> key -> value|#f
(define (ast-get-attr-value ast key)
  (let ((x (ast-get-attr ast key)))
    (and x (cdr x))))


;; ast-get-attrs :: ast -> [(key . value)]
(define (ast-get-attrs ast)
  (list-ref ast 1))


;; ast-get-children :: ast -> [ast]
(define (ast-get-children ast)
  (list-ref ast 2))


;; ast-node? :: any -> bool
(define (ast-node? ast)
  (and (list? ast)
       (= (length ast) 3)
       (eq? (car ast) 'ast-node)))


;; ast-add-child :: ast -> ast -> ast
(define (ast-add-child ast child)
  (make-ast (ast-get-attrs ast)
            (append (ast-get-children ast) (list child))))


;; ast-put-attr :: ast -> key -> value -> ast
(define (ast-put-attr ast key value)
  (make-ast (cons (cons key value) (ast-get-attrs ast))
            (ast-get-children ast)))

;;
;; utilities
;;

(define (ast-first-child ast)
  (car (ast-get-children ast)))

(define (ast-second-child ast)
  (cadr (ast-get-childrem ast)))

(define (ast-third-child ast)
  (caddr (ast-get-children ast)))

(define (ast-get-value ast)
  (ast-get-attr-value ast 'value))

(define (ast-get-type ast)
  (ast-get-attr-value ast 'type))
