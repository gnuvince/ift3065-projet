;; Entete
;;

(load "token.scm")
(load "utilities.scm")

(define keywords '(define else unquote unquote-splicing quote lambda
                    if set! begin cond and or case let let-star letrec
                    do delay quasiquote))

(define (keyword? ident)
  (member? keywords ident))

(define stream #f)

(define (parse tokens)
  (begin
    (set! stream (make-token-stream tokens))
    (program)))

(define (program)
  (cond ((stream 'empty?) '())
        (else
         (command_or_definition+))))

;;  (let ((stream ))
;;    (define (loop)
;;      (cond ((stream 'empty?)
;;             '())
;;            (else (cons (parse-expr stream)
;;                        (loop)))))
;;    (loop)))

(define (command_or_definition+)
  (cond ((open-paren-token? (stream 'next)))))

(define (command_or_definition)
  '())

(define (parse-expr stream)
  (let ((tok (stream 'next)))
    (cond ((self-evaluating? tok)
           tok)
          ((is-token-type? tok ident)
           tok)
          ((is-token-type? tok open-paren)
           (parse-compound-expr stream))
          (else
           (error (string-append "Unrecognized token: line "
                                 (token-line tok)
                                 " col "
                                 (token-col tok)
                                 "\n"))))))
  
(define (parse-compound-expr stream)
  (begin
    (consum-open-paren stream)
    (let ((tok (stream 'next)))
      (case (token-type tok)
        ((close-paren) #t)
        ((open-paren)  (parse-compound-expr stream))
        ((lambda)      (parse-lambda stream))
    (consum-close-paren stream)))
      
(define (parse-lambda stream)
  (cons (consum-keyword stream 'lambda)
        (list (parse-formals)
              (parse-body))))

(define (parse-formals stream)
  (let ((tok (stream 'next)))
    (cond ((open-paren-token? tok)
           (parse-variable* stream))
          (else
           (parse-variable stream)))))

(define (parse-variable* stream)
  (begin
    (consum-open-paren stream)
    (cond ((close-paren-token? (stream 'next))
           (begin
             (consum-close-paren stream)
             '()))
          (else
           (let ((formals (cons (parse-variable stream)
                                (parse-variable+ stream)))
             (consum-close-paren stream)
             formals)))))

(define (parse-variable stream)
  (cond ((variable-token? (stream 'next))
         (stream 'pop))
        (else
         (error (make-err-msg "Illegal token for a variable: line " tok)))))

(define (parse-variable+ stream)
  (cond ((close-paren-token? (stream 'next))
         '())
        ((dot-token? (stream 'next))
         (cons (stream 'pop)
               (list (parse-variable stream))))
        (else
         (cons (parse-variable stream)
               (parse-variable+ stream)))))

(define (parse-body stream)
  (display "To Be Completed"))

(define (consum-token-value stream value msg)
  (let ((tok (stream 'pop)))
    (cond ((is-token-value? tok value)
           tok)
          (else
           (error (string-append msg
                                 (token-line tok)
                                 " col "
                                 (token-col tok)
                                 "\n"))))))

(define (consum-token-type stream type msg)
  (let ((tok (stream 'pop)))
    (cond ((is-token-type? tok type)
           tok)
          (else
           (error (string-append msg
                                 (token-line tok)
                                 " col "
                                 (token-col tok)
                                 "\n"))))))
  
(define (consum-open-paren stream)
  (consum-token-type stream open-paren "Expected start of compound expression: line "))

(define (consum-close-paren stream)
  (consum-token-type stream close-paren "Expected end of compound expression: line "))

(define (consum-keyword stream keyword)
  (consum-token-value stream keyword (string-append "Expected keyword: "
                                                    (symbol->string keyword)
                                                    " line ")))
