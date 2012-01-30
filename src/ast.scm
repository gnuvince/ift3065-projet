;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(define (token->ast tok)
    (list (cond ((ident-token? tok)
                 (string->symbol (token-value tok)))
                (else
                 (token-value tok)))
          (list (list 'type (token-type tok))
                (list 'line (token-line tok))
                (list 'col  (token-col  tok)))
          '()))

(define ast-get-value car)
(define ast-get-alist cadr)
(define ast-get-childs caddr)

(define (ast-get-prop ast key)
  (assoc key (ast-get-alist ast)))

(define (ast-get-prop-value ast key)
  (let ((prop (ast-get-prop ast key)))
    (and prop
         (cadr prop))))

(define (ast-node? ast)
  (and (= (length ast) 3)
       (not (pair? (car ast)))
       (alist? (ast-get-alist ast))))
           
