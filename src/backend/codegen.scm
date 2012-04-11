(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")

(define env '())

(define (compile-define expr)
  (match expr
    ((define ,var ,expr)
     (set! env (cons (cons var expr) env)))
    (,_ (error "not a define"))))
