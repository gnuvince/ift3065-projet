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
        (error (string-append "expected type: " (symbol->string type) "\n"
                              "found type   : " (symbol->string (token-type t)) "\n"
                              (number->string (token-line t))
                              ":"
                              (number->string (token-col t)))))))

(define (stream-consume-value s value)
  (let ((t (stream-peek s)))
    (if (equal? value (token-value t))
        (stream-consume s)
        (error (string-append "expected value: " (symbol->string value) "\n"
                              "found value   : " (symbol->string (token-value t)) "\n"
                              (number->string (token-line t))
                              ":"
                              (number->string (token-col t)))))))

(define (stream-empty? s)
  (not (stream-peek s)))




(define (accumulate-expressions-until-close-paren stream)
    (let loop ((exprs '())
             (t (stream-peek stream)))
    (if (eq? (token-type t) 'close-paren)
        (reverse exprs)
        (let ((expr (parse-expression stream)))
          (loop (cons expr exprs) (stream-peek stream))))))



;; program = { expression }.
(define (parse-program stream)
  (let ((exprs
         (let loop ((nodes '())
                    (t (stream-peek stream)))
           (if t
               (let ((expr (parse-expression stream)))
                 (if expr
                     (loop (cons expr nodes) (stream-peek stream))
                     #f))
               (reverse nodes)))))
    (make-ast '((type . program))
                exprs)))



;; expression = identifier
;;            | number
;;            | string
;;            | character
;;            | boolean
;;            | "'" expression
;;            | compound-expression
(define (parse-expression stream)
  (let ((t (stream-peek stream)))
    (cond
      ((eq? (token-type t) 'ident) (parse-identifier stream))
      ((eq? (token-type t) 'number) (parse-number stream))
      ((eq? (token-type t) 'string) (parse-string stream))
      ((eq? (token-type t) 'char) (parse-character stream))
      ((eq? (token-type t) 'boolean) (parse-boolean stream))
      ((eq? (token-type t) 'quote-symbol) (parse-quote stream))
      ((eq? (token-type t) 'open-paren) (parse-compound stream))
      (else #f))))


;; compound = begin
;;          | set!
(define (parse-compound stream)
  (stream-consume-type stream 'open-paren)
  (let* ((t (stream-peek stream))
         (node (cond
                ((eq? (token-type t) 'keyword) (parse-keyword stream))
                (else (parse-list stream)))))
    (and (stream-consume-type stream 'close-paren)
         node)))


(define (parse-keyword stream)
  (let ((t (stream-peek stream)))
    (cond
     ((eq? (token-value t) 'if) (parse-if stream))
     ((eq? (token-value t) 'set!) (parse-set! stream))
     ((eq? (token-value t) 'and) (parse-and stream))
     ((eq? (token-value t) 'or) (parse-or stream))
     ((eq? (token-value t) 'begin) (parse-begin stream))
     ((eq? (token-value t) 'lambda) (parse-lambda stream))
     ((eq? (token-value t) 'quote) (parse-quote stream))
     (else #f))))


(define (parse-list stream)
  (let ((exprs
         (let loop ((tokens (list))
                    (t (stream-peek stream)))
           (cond
            ((eq? (token-type t) 'close-paren) (reverse tokens))
            (else (loop (cons (parse-expression stream) tokens)
                        (stream-peek stream)))))))
    (make-ast '((type . list)) exprs)))


;; number = digit { digit }
(define (parse-number stream)
  (let ((t (stream-consume-type stream 'number)))
    (and t (token->ast t))))


;; identifier = (alpha | extended ) { alpha | extended | digit }
(define (parse-identifier stream)
  (let ((t (stream-consume-type stream 'ident)))
    (and t (token->ast t))))


;; string = '"' { <character> } '"'
(define (parse-string stream)
  (let ((t (stream-consume-type stream 'string)))
    (and t (token->ast t))))


;; boolean = "#f" | "#t"
(define (parse-boolean stream)
  (let ((t (stream-consume-type stream 'boolean)))
    (and t (token->ast t))))


;; character = "#\" (<char> | <charname>)
(define (parse-character stream)
  (let ((t (stream-consume-type stream 'char)))
    (and t (token->ast t))))


;; if = "(if" expression expression [expression] ")"
(define (parse-if stream)
  (stream-consume-value stream 'if)
  (let ((condition (parse-expression stream))
        (then-branch (parse-expression stream)))
    (if (eq? (token-type (stream-peek stream)) 'close-paren)
        (make-ast '((type . if-then)) (list condition then-branch))
        (let ((else-branch (parse-expression stream)))
          (make-ast '((type . if-then-else))
                    (list condition then-branch else-branch))))))


;; set! = "(set!" identifier expression ")"
(define (parse-set! stream)
  (stream-consume-value stream 'set!)
  (let ((lvalue (parse-identifier stream))
        (rvalue (parse-expression stream)))
    (make-ast '((type . set!))
              (list lvalue rvalue))))


;; and = "(and" { expression } ")"
(define (parse-and stream)
  (stream-consume-value stream 'and)
  (make-ast '((type . and))
            (accumulate-expressions-until-close-paren stream)))


;; or = "(or" { expression } ")"
(define (parse-or stream)
  (stream-consume-value stream 'or)
  (make-ast '((type . or))
            (accumulate-expressions-until-close-paren stream)))


;; begin = "(begin" {expression} ")"
(define (parse-begin stream)
  (stream-consume-value stream 'begin)
  (make-ast '((type . begin))
            (accumulate-expressions-until-close-paren stream)))


;; lambda = "(lambda" params expression {expression} ")"
;; params = ident
;;        | "()"
;;        | "(" identifier {identifier} ["." identifier] ")"
(define (parse-lambda stream)
  (define (parse-lambda-params stream)
    (let ((t (stream-peek stream)))
      (if (eq? (token-type t) 'open-paren)
          (parse-param-list stream)
          (parse-identifier stream))))

  (define (parse-param-list stream)
    (stream-consume-type stream 'open-paren)
    (let loop ((params '())
               (t (stream-peek stream)))

      (cond ((eq? (token-type t) 'ident)
             (loop (cons (parse-identifier stream) params)
                   (stream-peek stream)))

            ((eq? (token-type t) 'close-paren)
             (stream-consume-type stream 'close-paren)
             (make-ast '((type . param-list)) (reverse params)))

            ((eq? (token-type t) 'dot)
             (stream-consume-type stream 'dot)
             (let ((last-param (parse-identifier stream)))
               (stream-consume-type stream 'close-paren)
               (make-ast '((type . dotted-param-list))
                         (reverse (cons last-param params)))))

            (else (error "parameters of a function should be identifiers")))))

  (stream-consume-value stream 'lambda)
  (let ((params (parse-lambda-params stream))
        (body (accumulate-expressions-until-close-paren stream)))
    (make-ast '((type . lambda))
              (list params body))))


;; quote = "(quote" expression ")"
;;       | "'" expression
(define (parse-quote stream)
  (stream-consume stream)
  (let ((expr (parse-expression stream)))
    (make-ast '((type . quote)) expr)))
