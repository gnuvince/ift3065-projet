;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)


(load "token.scm")
(load "utilities.scm")
(load "test_lexer.scm")
(load "lexer.scm")
(load "ast.scm")

(define stream #f)

(define (parse token-list)
  (<program> (normalize (ASTify token-list))))

(define (ASTify token-list)

  (define (consume-pair tree)
    (cond ((stream 'empty)
           (error "Early EOF"))
          ((open-paren-token? (stream 'next))
           (begin
             (consume-lparen)
             (consume-pair (cons (consume-pair '())
                                 tree))))
          ((close-paren-token? (stream 'next))
           (begin
             (consume-rparen)
             (reverse tree)))
          (else
           (consume-pair (cons (stream 'pop)
                               tree)))))  
  (define (ASTify-aux tree)
    (cond ((stream 'empty)
           (reverse tree))
          ((open-paren-token? (stream 'next))
           (begin
             (consume-lparen)
             (ASTify-aux (cons (consume-pair '())
                               tree))))
          ((close-paren-token? (stream 'next))
           (error "Datum or EOF expected"))
          (else
           (ASTify-aux (cons (stream 'pop)
                             tree)))))

  (begin
    (set! stream (make-token-stream token-list))
    (reverse (ASTify-aux '()))))

;;
;; ( token                      ( <val>
;;   ( <type> . <val> )  ====>    ( ( type <type> )
;;   <line>                         ( line <line> )
;;   <col> )                        ( col  <col>  ) )
;;                                ( <child>* ) )
;;
(define (normalize ast)  
  (define (normalize-aux ast nast)
    (cond ((null? ast)
           (reverse nast))
          ((token? (car ast))
           (normalize-aux (cdr ast)
                          (cons (token->ast (car ast))
                                nast)))
          (else
           (normalize-aux (cdr ast)
                          (cons (normalize-aux (car ast)
                                               '())
                                nast)))))
           
  (cond ((null? ast)
         ast)
        (else
         (normalize-aux ast '()))))

(define (<program> ast)
  (cond ((null? ast)
         ast)
        (else
         (list 'implicit-begin
               '()
               (reverse (<command-or-definition+> ast))))))

(define (<command-or-definition+> ast)
  (define (<command-or-definition> ast)
    (or (<command> ast)
        (<definition> ast)
        (and (eq? (car ast) 'begin)
             (<command-or-definition+> (caddr ast)))))

  (map <command-or-definition> ast))

(define (<definition> ast)
  #f)

(define (<expression> ast)
  (or (<variable> ast)
      (<literal>  ast)
      (<procedure-call> ast)
      (<lambda-expression> ast)
      (<conditional> ast)
      (<assignment> ast)
      (<derived-expression> ast)
      (<macro-use> ast)
      (<macro-block> ast)))

(define <command> <expression>)

(define (<variable> ast)
  (and (ast-node? ast)
       (ident? ast)
       ast))
;;  #f)
;;  (and (ident? ast)
;;       ast))

(define (<literal> ast)
  (or (<quotation> ast)
      (<self-evaluating> ast)))

(define (<quotation> ast)
  #f)

(define (<self-evaluating> ast)
  (or (<boolean> ast)
      (<number> ast)
      (<character> ast)
      (<string> ast)))

(define (<boolean> ast)
  (and (ast-node? ast)
       (let ((type (cadr (assoc 'type (cadr ast)))))
         (and (eq? type 'boolean)
              (null? (caddr ast))
              ast))))

(define (<number> ast)
  (and (ast-node? ast)
       (let ((type (cadr (assoc 'type (cadr ast)))))
         (and (eq? type 'number)
              (null? (caddr ast))
              ast))))

(define (<character> ast)
  #f)
;;   (let ((type (cadr (assoc 'type (cadr ast)))))
;;    (and (eq? type 'character)
;;         (null? (caddr ast))
;;         ast)))

(define (<string> ast)
  (and (ast-node? ast)
       (let ((type (cadr (assoc 'type (cadr ast)))))
         (and (eq? type 'string)
              (null? (caddr ast))
              ast))))

(define (<procedure-call> ast)
  (and (not (ast-node? ast))
       (or (null? (cdr ast))
           (and (simple-binop? ast)
                (andmap (lambda (node)
                          (or (<number> node)
                              (ident? node))) (cdr ast))))
       (list (ast-get-value (car ast))
             (ast-get-alist (car ast))
             (cdr ast))))

(define (<lambda-expression> ast)
  #f)

(define (<conditional> ast)
  #f)

(define (<assignment> ast)
  #f)

(define (<derived-expression> ast)
  #f)

(define (<macro-use> ast)
  #f)

(define (<macro-block> ast)
  #f)

;;
;; utilities
;;

(define (consume-token-value value msg)
  (let ((tok (stream 'pop)))
    (cond ((is-token-value? tok value)
           tok)
          (else
           (error (make-err-msg msg tok))))))

(define (consume-token-type type msg)
  (let ((tok (stream 'pop)))
    (cond ((is-token-type? tok type)
           tok)
          (else
           (error (make-err-msg msg tok))))))
  
(define (consume-lparen)
  (consume-token-value 'open-paren "Expected start of compound expression: line "))

(define (consume-rparen)
  (begin
    (consume-token-value 'close-paren "Expected end of compound expression: line ")))

(define (consume-keyword keyword)
  (consume-token-value 'keyword (string-append "Expected keyword: "
                                              (symbol->string keyword)
                                              " line ")))

(define (ident? ast)
  (eq? (ast-get-prop-value ast 'type)
       'ident))

(define (simple-binop? ast)
  (member? (ast-get-value (car ast))
           '(+ - * /)))
