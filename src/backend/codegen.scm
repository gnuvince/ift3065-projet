(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
(include "env.scm")

(define *false* 1)
(define *true* 2)
(define *null* 5)

(define delayed-functions '())

(define primitive-funcs '(;symbol         # args primitive name
                          (%*                  4 "__mul")
                          (%+                  4 "__add")
                          (%-                  4 "__sub")
                          (%<                  4 "__lt")
                          (%<=                 4 "__le")
                          (%=                  4 "__eq")
                          (%>                  4 "__gt")
                          (%>=                 4 "__ge")
                          (%car                3 "__getCar")
                          (%cdr                3 "__getCdr")
                          (%cons               4 "__cons")
                          (%eq?                4 "__equal")
                          (%null?              3 "__null_p")
                          (%number?            3 "__number_p")
                          (%pair?              3 "__pair_p")
                          (%quotient           4 "__quotient")
                          (%remainder          4 "__remainder")
                          (%set-car!           4 "__setCar")
                          (%set-cdr!           4 "__setCdr")
                          (%string?            3 "__string_p")
                          (%write-char         3 "__writeChar")

                          (%vector             3 "__vector")
                          (%vector-ref         4 "__vectorRef")
                          (%vector-set!        5 "__vectorSet")
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
     "pushl $0\n"
     "pushl $5\n"
     "call __initHeap\n"
     asm-code
     "addl $8, %esp\n"
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
    (() (gen-null))
    ((let ,args ,body) (compile-let expr env))
    ((begin . ,args) (map (lambda (a) (compile-expr a env)) args))
    ((lambda ,args ,body) (delay-lambda expr env))
    ((if ,condition ,then) (compile-if (append expr '(#f)) env))
    ((if ,condition ,then ,else) (compile-if expr env))
    ((set! ,var ,expr) (compile-set! env var expr))

    ((make-closure ,fn . ,captures) (gen-make-closure fn captures env))
    ((closure-code ,clo) (gen-closure-code clo env))
    ((closure-ref ,clo ,i) (gen-closure-ref clo i env))

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
           "pushl %ebp\n"
           "movl  %esp, %ebp\n"
           (compile-expr body
                         ;; We increment fs because the epb and return
                         ;; address are pushed onto the stack.
                         (env-fs+ 2
                          (env-update env args)))
           "addl $4, %esp    # clean ebp, etc.\n"
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
  ;; (list
  ;;  "movl $" n ", %eax\n"
  ;;  "pushl %eax\n"
  ;;  "call __boxint\n"
  ;;  "addl $4, %esp            # end_number\n"
  ;;  ))
;; More efficient version if we don't use the __box primitive.
  (list
   "movl $" n ", %eax\n"
   "imull  $4, %eax\n"))


(define (gen-variable-access var env)
  (match (env-lookup env var)
    ((,varname local ,offset) (list "movl " (- (env-fs env) offset word-size) "(%esp), %eax\n"))
    ((,varname global ,label) (list "movl " label ", %eax\n"))))


(define (push-args args env)
  (list (let loop ((env env) (args (reverse args)) (acc '()))
          (if (null? args)
              acc
              (loop (env-fs++ env)
                    (cdr args)
                    (append acc
                            (list (compile-expr (car args) env)
                                  "pushl %eax\n")))))))

(define (gen-fun-call f args env)
  (list
   (push-args args env)
   (compile-expr f (env-fs+ (length args) env))
   "call *%eax\n"
   "addl $" (* 4 (length args)) ", %esp # cleaning up function\n"))




(define (gen-prim-call primitive args env)
  (match primitive
    ((,f ,nb-args ,label)
     (list
      (push-args args env)
      "pushl $" nb-args "\n"
      "pushl $5\n"
      "call " label "\n"
      "addl $" (* 4 nb-args) ", %esp\n"))))



(define (gen-bool b)
  (list "movl $"
        (if b *true* *false*)
        " ,%eax\n"))


(define (compile-set! env var expr)
  (match (env-lookup env var)
    ((,v global ,label) (list (compile-expr expr env)
                              "movl %eax, " label "\n"))))



(define (gen-null)
  (list "movl $" *null* ", %eax\n"))


(define (gen-make-closure fn captures env)
  (let ((size (+ 1 (length captures))))
    (list
     ;; Create vector
     "\n# make-closure\n"
     (gen-number size)
     "pushl %eax\n"
     "pushl $1\n"                       ; unused; for C compatibility
     "pushl $5\n"                       ; unused; for C compatibility
     "call __vector\n"
     "addl $12, %esp\n"
     "pushl %eax\n"                     ; Push vector addr

     ;; Add function + captured variables
     (let loop ((i 0) (cs (cons fn captures)))
       (if (null? cs)
           '()
           (cons (list (compile-expr (car cs) env)
                       "pushl %eax\n"
                       (gen-number i)
                       "pushl %eax\n"
                       "pushl 8(%esp)\n"
                       "pushl $3\n"     ; unused; for C compatibility
                       "pushl $5\n"     ; unused; for C compatibility
                       "call __vectorSet\n"
                       "addl $20, %esp\n")
                 (loop (+ i 1) (cdr cs)))))
     "movl 0(%esp), %eax\n"
     "addl $4, %esp\n"                  ; Remove vector addr
     "# end of make-closure\n\n")))


(define (gen-closure-code clo env)
  (list
   "pushl $0\n"
   (compile-expr clo (env-fs++ env))
   "pushl %eax\n"
   "pushl $2\n"                         ; unused; for C compatibility
   "pushl $5\n"                         ; unused; for C compatibility
   "call __vectorRef\n"
   "addl $16, %esp\n"
   ))


(define (gen-closure-ref clo i env)
  (list
   (gen-number (+ 1 i))
   "pushl %eax\n"
   (compile-expr clo (env-fs++ env))
   "pushl %eax\n"
   "pushl $2\n"
   "pushl $5\n"
   "call __vectorRef\n"
   "addl $16, %esp\n"
   ))
