(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
(include "env.scm")

(define *false* 1)

(define delayed-functions '())

(define primitive-funcs '(;symbol         # args primitive name
                          (%*                  2 "__mul")
                          (%+                  2 "__add")
                          (%-                  2 "__sub")
                          (%<                  2 "__lt")
                          (%<=                 2 "__le")
                          (%=                  2 "__eq")
                          (%>                  2 "__gt")
                          (%>=                 2 "__ge")
                          (%car                1 "__getCar")
                          (%cdr                1 "__getCdr")
                          (%cons               2 "__cons")
                          (%eq?                2 "__equal")
                          (%null?              1 "__null_p")
                          (%number?            1 "__number_p")
                          (%pair?              1 "__pair_p")
                          (%quotient           2 "__quotient")
                          (%remainder          2 "__remainder")
                          (%set-car!           2 "__setCar")
                          (%set-cdr!           2 "__setCdr")
                          (%string?            1 "__string_p")
                          (%write-char         1 "__writeChar")
                          ))


(define global-env (make-env))


(define (gen-global-vars expr)
  (map (lambda (var)
         `("glob_" ,(symbol->label var) ": .long 0\n"))
       (fv expr)))


(define (compile expr)
  (let ((asm-code (compile-expr expr global-env))
        (delayed-asm (compile-delayed-lambdas)))
    (list
     ".text\n"
     ".globl main\n"
     "main:\n"
     asm-code
     "ret\n"
     delayed-asm

     "\n\n"
     ".data\n"
     (gen-global-vars expr))))


(define (compile-delayed-lambdas)
  (let loop ((acc '()))
    (match delayed-functions
      (() acc)
      (((,sym ,fn ,env) . ,rest)
       (begin
         (set! delayed-functions rest)
         (loop (cons (compile-lambda sym fn env) acc)))))))


(define (compile-expr expr env)
  (match expr
    (,n when (number? n) (gen-number n))
    (,b when (boolean? b) (gen-bool b))
    (,s when (symbol? s) (gen-variable-access s env))
    ((let ,args ,body) (compile-let expr env))
    ((begin . ,args) (map (lambda (a) (compile-expr a env)) args))
    ((lambda ,args ,body) (delay-lambda expr env))
    ((if ,condition ,then) (compile-if (append expr '(#f)) env))
    ((if ,condition ,then ,else) (compile-if expr env))
    ((set! ,var ,expr) (compile-set! env var expr))
    ((,f . ,args)
     (let ((primitive (assq f primitive-funcs)))
       (if primitive
           (gen-prim-call primitive args env)
           (gen-fun-call f args env))))
    (,_ (error "unknown expression " expr))))


(define (compile-lambda sym fn env)
  (match fn
    ((lambda ,args ,body)
     (list "\n" sym ":\n"
           (compile-expr body
                         ;; We increment fs because the return address is also pushed
                         ;; onto the stack.
                         (env-fs++
                          (env-update env args)))
           "ret\n\n"))))

(define (delay-lambda expr env)
  (match expr
    ((lambda ,args ,body)
     (let ((g (gensym)))
       (set! delayed-functions
             (append delayed-functions (list (list g expr env))))
       (list
        "movl $" g ", %eax  # lambda pointer\n")))))


(define (compile-let expr env)
  (match expr
    ((let ,bindings ,body)
     (let ((new-env (env-update env (map car bindings))))
       (list
        "                            # begin let\n"
        (map (lambda (b)
               (list (compile-expr (cadr b) env) ; Use the old env
                     "pushl %eax\n"))
             bindings)
        (compile-expr body new-env)
        "addl $" (* 4 (length bindings)) ", %esp  # exiting let\n")))))


(define (compile-if expr env)
  (match expr
    ((if ,condition ,then ,else)
     (let ((cond-comp (compile-expr condition env))
           (then-comp (compile-expr then env))
           (else-comp (compile-expr else env))
           (else-label (gensym))
           (endif-label (gensym)))
       (list
        cond-comp
        "cmp   $" *false* ", %eax\n"
        "je    " else-label "\n"
        then-comp
        "jmp   " endif-label "\n"
        else-label ":   # ELSE\n"
        else-comp
        endif-label ":  # ENDIF\n"
        )))))



(define (gen-number n)
 ;; (list "movl $" n ", %eax\n"))
;; Boxed version
  (list
   "movl $" n ", %eax\n"
   "pushl %eax\n"
   "call __boxint\n"
   "addl $4, %esp            # end_number\n"
   ))
;; More efficient version if we don't use the __box primitive.
;; (list
;;  "movl $" n ", %eax\n"
;;  "mull  $4, %eax\n"))


(define (gen-variable-access var env)
  (match (env-lookup env var)
    ((,varname local ,offset) (list "movl " (- (env-fs env) offset word-size) "(%esp), %eax\n"))
    ((,varname global ,label) (list "movl " label ", %eax\n"))))


(define (push-args args env)
  (let loop ((env env) (args (reverse args)) (acc '()))
    (if (null? args)
        acc
        (loop (env-fs++ env)
              (cdr args)
              (append acc
                      (list (compile-expr (car args) env)
                            "pushl %eax\n"))))))


(define (gen-fun-call f args env)
  (list
   (push-args args env)
   (compile-expr f env)
   "call *%eax\n"
   "addl $" (* 4 (length args)) ", %esp # cleaning up function\n"))




(define (gen-prim-call primitive args env)
  (match primitive
    ((,f ,nb-args ,label)
     (list
      (push-args args env)
      "call " label "\n"
      "addl $" (* 4 nb-args) ", %esp\n"))))



(define (gen-bool b)
  (list "movl $"
        (if b 0 *false*)
        " ,%eax\n"))


(define (compile-set! env var expr)
  (match (env-lookup env var)
    ((,v global ,label) (list (compile-expr expr env)
                              "movl %eax, " label "\n"))))
