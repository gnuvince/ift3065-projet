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
  (<program> (arborify token-list)))

(define (arborify token-list)
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
           (consume-pair (cons (token-value (stream 'pop))
                               tree)))))
  
  (define (arborify-aux tree)
    (cond ((stream 'empty)
           (reverse tree))
          ((open-paren-token? (stream 'next))
           (begin
             (consume-lparen)
             (arborify-aux (cons (consume-pair '())
                                 tree))))
          ((close-paren-token? (stream 'next))
           (error "Datum or EOF expected"))
          (else
           (arborify-aux (cons (token-value (stream 'pop))
                               tree)))))

  (begin
    (set! stream (make-token-stream token-list))
    (arborify-aux '())))


(define (<program> ast)
  (cond ((null? ast)
         ast)
        (else
         (let ((cod (<command-or-definition> (car ast))))
           (if cod
               (cons cod (<program> (cdr ast)))
               (error (string-append "Error in expression:\n"
                                     (with-output-to-string "" (lambda () (write (car ast)))))))))))

(define (<command-or-definition*> ast)
  (cond ((null? ast)
         ast)
        (else
         (<command-or-definition+> ast))))

(define (<command-or-definition+> ast)
  (and (<command-or-definition> (car ast))
       (<command-or-definition*> (cdr ast))
       ast))

(define (<command-or-definition> ast)
  (and
   (or (<command> ast)
       (<definition> ast)
       (begin-com-or-def+ ast))
   ast))

(define (begin-com-or-def+ ast)
  (and (pair? ast)
       (>= (length ast) 2)
       (eq? (car ast) 'begin)
       (<command-or-definition+> (cdr ast))
       ast))

(define (begin-definition* ast)
  (and (pair? ast)
       (eq? (car ast) 'begin)
       (or (= (length ast) 1)
           (<definition*> (cdr ast)))
       ast))

;; ( define <variable>  <expression> )
(define (define-var-expr ast)
  (and (pair? ast)
       (= (length ast) 3)
       (eq? (car ast) 'define)
       (<variable> (cadr ast))
       (<expression> (caddr ast))
       ast))
              
;; ( define ( <variable> <def-formals> ) body )
(define (define-var-formals ast)
  (and (pair? ast)
       (>= (length ast) 3)
       (eq? (car ast) 'define)
       (pair? (cadr ast))
       (<variable> (caadr ast))
       (<def-formals> (cdadr ast))
       (<body> (cddr ast))
       ast))

(define (<def-formals> ast)
  (and (pair? ast)
       (or (null? ast)
           (let ((dotp (member 'dot ast)))
             (and dotp
                  (= (length dotp) 2)
                  (<variable> (cadr dotp))
                  (<variable>* (but-last-n ast 2))))
           (<variable>* ast))
       ast))

(define (<body> ast)
  (and (not (null? ast))
       (or (<expression>+ ast)
           (and (<definition> (car ast))
                (<body> (cdr ast))))
       ast))

(define (<definition> ast)
  (and
   (or (define-var-expr ast)
       (define-var-formals ast)
       (begin-definition* ast))
   ast))

(define (<expression>* ast)
  (and (or (null? ast)
           (<expression>+ ast))
       ast))

(define (<expression>+ ast)
  (and (pair? ast)
       (<expression> (car ast))
       (<expression>* (cdr ast))
       ast))

(define (<expression> ast)
  (and 
   (or (<variable> ast)
       (<literal>  ast)
       (<procedure-call> ast)
       (<lambda-expression> ast)
       (<conditional> ast)
       (<assignment> ast)
       (<derived-expression> ast)
       (<macro-use> ast)
       (<macro-block> ast))
   ast))

(define <command> <expression>)

(define (<variable>* ast)
  (and
   (or (null? ast)
       (and (<variable> (car ast))
            (<variable>* (cdr ast))))
   ast))

(define (<variable> ast)
  (and (symbol? ast)
       ast))

(define (<literal> ast)
  (and 
   (or (<quotation> ast)
       (<self-evaluating> ast))
   ast))

(define (<quotation> ast)
  #f)

(define (<self-evaluating> ast)
  (and
   (or (<boolean> ast)
       (<number> ast)
       (<character> ast)
       (<string> ast))
   ast))

(define (<boolean> ast)
  (and
   (or (eq? ast #t)
       (eq? ast #f))
   ast))

(define (<number> ast)
  (and (number? ast)
       ast))

;;(define (<character> ast)
;;  #f)
(define (<character> ast)
  (and (char? ast)
       ast))

(define (<string> ast)
  (and (string? ast)
       ast))

;;(define (<procedure-call> ast)
;;  #f)
(define (<procedure-call> ast)
  (and (pair? ast)
       (>= (length ast) 1)
       (<operator> (car ast))
       (<operand>* (cdr ast))
       ast))

(define (<operator> ast)
  (and (not (member ast keywords))
       (<expression> ast)
       ast))

(define <operand> <expression>)

(define (<operand>* ast)
  (and (or (null? ast)
           (and (<operand> ast)
                (<operand>* (cdr ast))))
       ast))

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
  (eq? (ast-get-attr-value ast 'value)
       'ident))

(define (simple-binop? ast)
  (member? (ast-get-attr-value ast 'value)
           '(+ - * /)))
