;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(define quote-prefix-symbol (string->symbol "quote-prefix"))
(define unquote-prefix-symbol (string->symbol "unquote-prefix"))
(define unquote-splicing-prefix-symbol (string->symbol "unquote-splicing-prefix"))
(define quasiquote-prefix-symbol (string->symbol "quasiquote-prefix"))


(include "token.scm")
(include "../utils/utilities.scm")
(include "lexer.scm")
(include "reader.scm")

;; (define (parse token-list)
;;   (cons 'begin (parse-aux token-list '())))

;; (define (parse-aux token-list list)
;;   (let ((s (sins-read token-list)))
;;     (if (eq? s
;;         (cons

(define (harmonize-false ast)
  (map (lambda (x)
         (cond ((eq? x false) #f)
               ((pair? x) (harmonize-false x))
               (else x)))
       ast))

(define (parse token-list)
  (harmonize-false (cons 'begin (<program> (sins-read token-list)))))

(define (<program> ast)
  (cond ((null? ast)
         ast)
        (else
         (let ((cod (<command-or-definition> (car ast))))
           (if cod
               (cons cod (<program> (cdr ast)))
               (error (with-output-to-string "Error in expression:\n"
                                             (lambda () (write (car ast))))))))))

(define (<command-or-definition> ast)
  (and (or (<command> ast)
           (<definition> ast)
           (begin-com-or-def ast))
       ast))

(define (<command-or-definition>* ast)
  (cond ((null? ast)
         ast)
        (else
         (<command-or-definition>+ ast))))

(define (<command-or-definition>+ ast)
  (and (<command-or-definition> (car ast))
       (<command-or-definition>* (cdr ast))
       ast))

(define (begin-com-or-def ast)
  (and (list? ast)
       (>= (length ast) 1)
       (eq? (car ast) 'begin)
       (<command-or-definition>* ast)
       ast))

(define (begin-com-or-def* ast)
  (and (or (null? ast)
           (begin-com-or-def+ ast))
       ast))

(define (begin-com-or-def+ ast)
  (and (list? ast)
       (>= (length ast) 1)
       (begin-com-or-def (car ast))
       (begin-com-or-def* (cdr ast))
       ast))

(define (begin-definition* ast)
  (and (list? ast)
       (pair? ast)
       (eq? (car ast) 'begin)
       (or (null? (cdr ast))
           (<definition>* (cdr ast)))
       ast))

(define (define-var ast)
  (and (list? ast)
       (eq? (car ast) 'define)
       (<variable> (cadr ast))
       (null? (cddr ast))
       ast))

;; ( define <variable>  <expression> )
(define (define-var-expr ast)
  (and (list? ast)
       (eq? (car ast) 'define)
       (<variable> (cadr ast))
       (<expression> (caddr ast))
       (null? (cdddr ast))
       ast))

;; ( define ( <variable> <def-formals> ) body )
(define (define-var-formals ast)
  (and (list? ast)
       (pair? ast)
       (>= (length ast) 3)
       (eq? (car ast) 'define)
       (pair? (cadr ast))
       (<variable> (caadr ast))
       (<def-formals> (cdadr ast))
       (<body> (cddr ast))
       ast))

(define (<def-formals> ast)
  (and (or (null? ast)
           (and (improper-list? ast)
                (<variable> (car ast))
                (<def-formals> (cdr ast)))
           (<variable>* ast))
       ast))

(define (<formals> ast)
  (and (or (null? ast)
           (<variable> ast)
           (and (list? ast)
                (<variable> (car ast))
                (<variable>+ (cdr ast)))
           (and (pair? ast)
                (<variable> (car ast))
                (<formals> (cdr ast))))
       ast))

(define (<body> ast)
  (or (and (not (pair? ast))
           ast)
      (if (<definition> (car ast))
          (and (<body> (cdr ast))
               ast)
          (and (<sequence> ast)
               ast))))

(define (<definition>* ast)
  (and (list? ast)
       (or (null? ast)
           (and (<definition> (car ast))
                (<definition>* (cdr ast))))
       ast))

(define (<definition> ast)
  (and (or (define-var ast)
           (define-var-expr ast)
           (define-var-formals ast)
           (begin-definition* ast))
       ast))

(define (<expression> ast)
  (and (or (<variable> ast)
           (<literal>  ast)
           (<lambda-expression> ast)
           (<conditional> ast)
           (<assignment> ast)
           (<derived-expression> ast)
           (<procedure-call> ast)
           (<macro-use> ast)
           (<macro-block> ast))
   ast))

(define (<expression>+ ast)
  (and (pair? ast)
       (<expression> (car ast))
       (<expression>* (cdr ast))
       ast))

(define (<expression>* ast)
  (and (or (null? ast)
           (<expression>+ ast))
       ast))

;; diverge de la grammaire
(define (<sequence> ast)
  (and (list? ast)
       (> (length ast) 0)
       (or (and (list? (car ast))
                (eq? 'begin (caar ast))
                (<expression>* (cdar ast)))
           (<expression>+ ast))
       ast))

(define <command> <expression>)

(define (<variable>* ast)
  (and (or (null? ast)
           (<variable> ast)
           (and (list? ast)
                (<variable> (car ast))
                (<variable>* (cdr ast))))
       ast))

(define (<variable>+ ast)
  (and (list? ast)
       (>= (length ast) 1)
       (<variable> (car ast))
       (<variable>* (cdr ast))
       ast))

(define (<variable> ast)
  (and (symbol? ast)
       ast))

(define (identifier? ast)
  (and (not (number? ast))
       (not (string? ast))
       (not (pair? ast))
       ast))

(define (<literal> ast)
  (and (or (<quotation> ast)
           (<self-evaluating> ast))
       ast))

(define (<quotation> ast)
  (and (list? ast)
       (= (length ast) 2)
       (or
        (eq? (car ast) quote-prefix-symbol)
        (eq? (car ast) unquote-prefix-symbol)
        (eq? (car ast) unquote-splicing-prefix-symbol)
        (eq? (car ast) quasiquote-prefix-symbol))
       ast))

(define (<self-evaluating> ast)
  (or (<boolean> ast)
      (<number> ast)
      (<character> ast)
      (<string> ast)))

(define (<boolean> ast)
  (and (or (eq? ast #t)
           (eq? ast false))
       ast))

(define (<number> ast)
  (and (number? ast)
       ast))

(define (<character> ast)
  (and (char? ast)
       ast))

(define (<string> ast)
  (and (string? ast)
       ast))

(define (<procedure-call> ast)
  (and (list? ast)
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
  (and (list? ast)
       (= (length ast) 3)
       (eq? (car ast) 'lambda)
       (<formals> (cadr ast))
       (<body> (caddr ast))
       ast))

(define (<conditional> ast)
  (and (list? ast)
       (eq? (car ast) 'if)
       (member (length ast) '(3 4))
       (<expression> (cadr ast))
       (<expression> (caddr ast))
       (or (null? (cdddr ast))
           (<expression> (cdddr ast)))
       ast))

(define (<assignment> ast)
  (and (list? ast)
       (eq? (car ast) 'set!)
       (<variable> (cadr ast))
       (<expression> (caddr ast))
       (null? (cdddr ast))
       ast))

(define (<derived-expression> ast)
  (and (pair? ast)
       (or (<quasiquotation> ast)
           (cond-derived-expression ast)
           (and-derived-expression ast)
           (or-derived-expression ast)
           (let-derived-expression ast)
           (let*-derived-expression ast)
           (letrec-derived-expression ast)
           (begin-derived-expression ast)
           (do-derived-expression ast) ;; not implemented
           (delay-derived-expression ast) ;; not implemented
           (case-derived-expression ast))
       ast))

(define (<quasiquotation> ast)
  (and (eq? (car ast) quasiquote-prefix-symbol)
       ast))

(define (cond-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'cond)
       (cond-clauses (cdr ast))
       ast))

(define (cond-clauses ast)
  (and (if (cond-else-clause (car ast))
           (null? (cdr ast))
           (and (<cond-clause> (car ast))
                (or (null? (cdr ast))
                    (cond-clauses (cdr ast)))))
       ast))

(define (cond-else-clause ast)
  (and (pair? ast)
       (eq? (car ast) 'else)
       (<sequence> (cdr ast))
       ast))

(define (<cond-clause> ast)
  (and (pair? ast)
       (<test> (car ast))
       (or (null? (cdr ast))
           (and (eq? (cadr ast) '=>)
                (<recipient> (caddr ast)))
           (<sequence> (cdr ast)))
       ast))

(define <test> <expression>)

(define <recipient> <expression>)

(define (and-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'and)
       (<expression>* (cdr ast))
       ast))

(define (or-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'or)
       (<expression>* (cdr ast))
       ast))

(define (let-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'let)
       (>= (length ast) 3)
       (or (and (<variable> (cadr ast))
                (>= (length (cddr ast)) 2)
                (<binding-spec>* (caddr ast))
                (<body> (cdddr ast)))
           (and (<binding-spec>* (cadr ast))
                (<body> (cddr ast))))
       ast))

(define (let*-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'let*)
       (>= (length ast) 3)
       (<binding-spec>* (cadr ast))
       (<body> (cddr ast))
       ast))

(define (letrec-derived-expression ast)
  (and (pair? ast)
       (eq? (car ast) 'letrec)
       (>= (length ast) 3)
       (<binding-spec>* (cadr ast))
       (<body> (cddr ast))
       ast))

(define (<binding-spec>* ast)
  (and (or (null? ast)
           (and (pair? ast)
                (<binding-spec> (car ast))
                (<binding-spec>* (cdr ast))))
       ast))

(define (<binding-spec> ast)
  (and (pair? ast)
       (= (length ast) 2)
       (<variable> (car ast))
       (<expression> (cadr ast))
       ast))

(define (begin-derived-expression ast)
  (and (list? ast)
       (>= (length ast) 2)
       (eq? (car ast) 'begin)
       (<sequence> (cdr ast))
       ast))

(define (do-derived-expression ast)
  #f)

(define (case-datum ast)
  (and (or (list? ast)
           (symbol? ast)
           (<self-evaluating> ast))
       ast))

(define (case-datum* ast)
  (and (or (null? ast)
           (and (list? ast)
                (case-datum (car ast))
                (case-datum* (cdr ast))))
       ast))

(define (<case-clause> ast)
  (and (list? ast)
       (list? (car ast))
       (case-datum* (car ast))
       (<sequence> (cdr ast))
       ast))

(define (case-body ast)
  (and (list? ast)
       (not (member #f (map list? ast)))
       (>= (length ast) 1)
       (or (and (eq? (caar ast) 'else)
                (<sequence> (cdar ast)))
           (and (<case-clause> (car ast))
                (or (null? (cdr ast))
                    (case-body (cdr ast)))))
       ast))

(define (case-derived-expression ast)
  (and (list? ast)
       (>= (length ast) 3)
       (eq? (car ast) 'case)
       (<expression> (cadr ast))
       (case-body (cddr ast))
       ast))

(define (delay-derived-expression ast)
  #f)

(define (<macro-use> ast)
  #f)

(define (<macro-block> ast)
  #f)
