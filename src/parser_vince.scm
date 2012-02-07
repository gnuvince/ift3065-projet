(load "lexer.scm")
(load "ast.scm")

;; whitespace = #\space | #\tab | #\newline
;; alpha = "a..zA..Z"
;; digit = "0..9"
;; extended = "!" | "$" | "%" | "&" | "*" | "+" | "-" | "." | "/" | ":"
;;          | "<" | "=" | ">" | "?" | "@" | "^" | "_" | "~"
;; number = digit { digit }
;; identifier = (alpha | extended) { (alpha | extended | digit) }
;; keyword = "define" | "else" | "unquote" | "unquote-splicing" | "quote"
;;         | "lambda" | "if" | "set!" | "begin" | "cond" | "and" | "or"
;;         | "case" | "let" | "let*" | "letrec" | "do" | "delay" | "quasiquote"
;; string = """" { any character except double-quote } """"
;; character = "#\" ( any character | "nul" | "newline" | "space" | "tab" )
;; boolean = "#t" | "#f"
;;
;; program = { expression }
;; expression = identifier
;;            | literal
;;            | proc-call
;;            | lambda-expr
;;            | conditional
;;            | assignment
;;



(define (make-token-stream str)
  (let ((tokens (lex-from-string str)))
    (lambda (msg)
      (case msg
        ((peek) (and (not (null? tokens)) (car tokens)))
        ((consume)
         (let ((t (and (not (null? tokens)) (car tokens))))
           (if t
               (begin
                 (set! tokens (cdr tokens))
                 t)
               #f)))))))

(define (stream-peek s)
  (s 'peek))

(define (stream-consume s)
  (s 'consume))

(define (stream-consume-type s type)
  (let ((t (stream-peek s)))
    (if (eq? type (token-type t))
        (stream-consume s)
        #f)))

(define (stream-consume-value s value)
  (let ((t (stream-peek s)))
    (if (equal? value (token-value t))
        (stream-consume s)
        #f)))

(define (stream-consume-type-value s type value)
  (let ((t (stream-peek s)))
    (if (and (eq? type (token-type t))
             (equal? value (token-value t)))
        (stream-consume s)
        #f)))

(define (stream-empty? s)
  (not (stream-peek s)))






(define (parse-program stream)
  (let ((exprs
         (let loop ((nodes '())
                    (t (stream-peek stream)))
           (if t
               (loop (cons (parse-expression stream) nodes)
                     (stream-peek stream))
               nodes))))
    (make-ast '((type . program))
              (reverse exprs))))


(define (parse-expression stream)
  (let ((t (stream 'peek)))
    (cond
      ((eq? (token-type t) 'ident) (parse-identifier stream))
      ((eq? (token-type t) 'number) (parse-number stream))
      ((eq? (token-type t) 'string) (parse-string stream))
      ((eq? (token-type t) 'char) (parse-character stream))
      ((eq? (token-value t) 'open-paren) (parse-compound stream))
      ((eq? (token-type t) 'boolean) (parse-boolean stream))
      (else #f))))


(define (parse-compound stream)
  (stream-consume-value stream 'open-paren)
  (let* ((t (stream-peek stream))
         (node (cond
                ((eq? (token-type t) 'keyword) (parse-keyword stream))
                (else (parse-list stream)))))
    (stream-consume-value stream 'close-paren)
    node))


(define (parse-keyword stream)
  (let ((t (stream-peek stream)))
    (cond
     ((eq? (token-value t) 'if) (parse-if stream))
     (else #f))))


(define (parse-list stream)
  (let ((exprs
         (let loop ((tokens (list))
                    (t (stream-peek stream)))
           (cond
            ((eq? (token-value t) 'close-paren) (reverse tokens))
            ((eq? (token-type t) 'eof) (error "syntax error"))
            (else (loop (cons (parse-expression stream) tokens)
                        (stream-peek stream)))))))
    (stream-consume-value stream 'close-paren)
    (make-ast '((type . list))
              exprs)))



(define (parse-number stream)
  (let ((t (stream-consume-type stream 'number)))
    (and t (token->ast t))))

(define (parse-identifier stream)
  (let ((t (stream-consume-type stream 'ident)))
    (and t (token->ast t))))

(define (parse-string stream)
  (let ((t (stream-consume-type stream 'string)))
    (and t (token->ast t))))

(define (parse-boolean stream)
  (let ((t (stream-consume-type stream 'boolean)))
    (and t (token->ast t))))

(define (parse-character stream)
  (let ((t (stream-consume-type stream 'char)))
    (and t (token->ast t))))

(define (parse-if stream)
  (stream-consume-value stream 'if)
  (let* ((condition (parse-expression stream))
         (then-branch (parse-expression stream))
         (else-branch (parse-expression stream)))
    (cond
     ((and condition then-branch else-branch)
      (make-ast '((type . if-then-else))
                (list condition then-branch else-branch)))
     ((and condition then-branch)
      (make-ast '((type . if-then))
                (list condition then-branch)))
     (else #f))))
