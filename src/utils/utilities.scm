;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(define (improper-list? e)
  (and (pair? e)
       (not (list? e))))

(define member? member)

(define keywords (quote (define else unquote unquote-splicing quote lambda
                         if set! begin cond and or case let let-star letrec
                         do delay quasiquote)))

(define (keyword? ident)
  (member? ident keywords))

(define (make-token-stream tokens)
  (let ((stream tokens))
    (lambda (msg)
      (case msg
        ((show)    (begin (display stream)(newline)))
        ((empty)   (null? stream))
        ((next)    (cond ((null? stream)
                          (list))
                         (else
                          (car stream))))
        ((2ndnext) (cond ((< (length stream) 2)
                          (list))
                         (else
                          (cadr stream))))
        ((advance) (cond ((null? stream)
                          (error "Advancing beyond last stream element"))
                         (else
                          (set! stream (cdr stream)))))
        ((pop)     (cond ((null? stream)
                          (error "Advancing beyond last stream element"))
                         (else
                          (let ((tok (car stream)))
                            (set! stream (cdr stream))
                            tok))))))))

(define (make-err-msg msg tok)
  (string-append msg
                 (number->string (token-line tok))
                 " col "
                 (number->string (token-col tok))
                 "\n"))

(define (andmap f lst)
  (define (andmap-aux f lst pred)
    (cond ((or (not pred)
               (null? lst))
           pred)
          (else
           (andmap-aux f
                       (cdr lst)
                       (and pred
                            (f (car lst)))))))

  (andmap-aux f lst #t))

(define (but-last-n lst n)
  (reverse (list-tail (reverse lst) n)))

(define (last-n lst n)
  (list-tail lst (- (length lst) n)))

(define (last lst)
  (list-ref lst (- (length lst) 1)))

(define (b4last lst)
  (list-ref lst (- (length lst) 2)))

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

(define (consume-token type value msg)
  (let ((tok (stream 'pop)))
    (cond ((and (is-token-type? tok type)
               (is-token-value? tok value))
           tok)
          (else
           (error (make-err-msg msg tok))))))

;; (define (consume-lparen)
;;   (consume-token-value 'open-paren "Expected start of compound expression: line "))

;; (define (consume-lparen)
;;   (consume-token-value 'open-paren "Expected start of compound expression: line "))

(define (consume-rparen)
  (
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


(define (all? p list)
  (if (null? list)
      #t
      (and (p (car list))
           (all? p (cdr list)))))
