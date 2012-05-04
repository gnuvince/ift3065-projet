(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
(include "env.scm")

;; Value of Scheme constants as defined in sins_const.h
(define *false* 1)
(define *true* 5)
(define *null* 2)

;; Queue of functions that will be compiled after the current
;; function is done.
(define delayed-functions '())

;; List of the different primitives
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
                          (%display            3 "__display")
                          (%number?            3 "__number_p")
                          (%pair?              3 "__pair_p")
                          (%quotient           4 "__quotient")
                          (%remainder          4 "__remainder")
                          (%set-car!           4 "__setCar")
                          (%set-cdr!           4 "__setCdr")
                          (%string?            3 "__string_p")
                          (%write-char         3 "__writeChar")
                          (%make-string        3 "__makeString")
                          (%string-ref         3 "__stringRef")
                          (%string-set!        4 "__stringSet")
                          ))


;; The global environment starts empty.
(define global-env (make-env))


;; Global variables are represented by "glob_" followed by their symbol.
(define (gen-global-vars expr)
  (let ((free-vars (fv expr)))
    (list "_TOTAL_VARIABLES_: .long " (length free-vars) "\n"
          (map (lambda (var)
                 `("glob_" ,(symbol->label var) ": .long 0\n"))
               free-vars))))


;; Compile an expression into a program.
(define (compile expr)
  (let ((asm-code (compile-expr expr global-env))
        (delayed-asm (compile-delayed-lambdas)))
    (list
     ".text\n"
     ".globl main\n"
     "main:\n"
     "pushl $0\n"
     "pushl $2\n"
     "call __initHeap\n"
     (push-frame)
     asm-code
     "addl $8, %esp\n"
;     (pop-frame)
     "ret\n"
     delayed-asm

     "\n\n"
     ".data\n"
     (gen-global-vars expr))))


;; Loop through the queue of delayed functions, compiling them.  More lambdas
;; may be added as they are found (lambda inside a lambda).
(define (compile-delayed-lambdas)
  (let loop ((acc '()))
    (match delayed-functions
      (() acc)
      (((,sym ,fn ,env) . ,rest)
       (begin
         (set! delayed-functions rest)
         (loop (cons (compile-lambda sym fn env) acc)))))))


;; Compile an expression.
(define (compile-expr expr env)
  (match expr
    (,n when (number? n) (gen-number n))
    (,b when (boolean? b) (gen-bool b))
    (,c when (char? c) (gen-char c))
    (,s when (symbol? s) (gen-variable-access s env))
    (,s when (string? s) (gen-string s env))
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



;; Compile a lambda into an assembly function with its unique label.
(define (compile-lambda sym fn env)
  (match fn
    ;; Fixed parameters
    ((lambda ,params ,body) when (list? params)
     (list "\n" sym ":\n"
           "pushl %ebp\n"
           "movl  %esp, %ebp\n"
           (compile-expr body
                         ;; We increment fs because the epb and return
                         ;; address are pushed onto the stack.
                         (env-fs+ 2
                          (env-update env params)))
           "popl %ebp\n"
           "ret\n\n"))

    ;; Rest parameter
    ((lambda ,params ,body)
     (let* ((proper-params (improper->proper params))
            (nb-fixed-params (- (length proper-params) 3))
            (new-env (env-fs+ 2 (env-update env proper-params)))
            (loop-label (gensym))
            (end-loop-label (gensym)))
       (list
        "\n" sym ":\n"
        "pushl %ebp\n"
        "movl  %esp, %ebp\n"
        (gen-number nb-fixed-params)
        "movl %eax, %ebx\n"
        (compile-expr '$num new-env)
        "subl %ebx, %eax\n"
        "movl %eax, %ecx\n"
        (gen-null)

        loop-label ":\n"
        "cmp $0, %ecx\n"
        "je " end-loop-label "\n"
        "pushl %ecx\n"
        "pushl %eax\n"
        "movl %esp, %ebx\n"
        "addl %ecx, %ebx\n"
        "addl $20, %ebx\n"
        "pushl (%ebx)\n"
        (gen-number 2)
        "pushl %eax\n"
        (gen-null)
        "pushl %eax\n"
        "call __cons\n"
        "addl $16, %esp\n"
        "popl %ecx\n"
        "subl $4, %ecx\n"
        "jmp " loop-label "\n"
        end-loop-label ":\n"

        "movl %esp, %ebx\n"
        "pushl %eax\n"
        "addl $16, %ebx\n"              ; Skip over epb, RA, $clo and $num.
        (gen-number nb-fixed-params)
        "addl %eax, %ebx\n"
        "popl %eax\n"
        "movl %eax, (%ebx)\n"

        (compile-expr body new-env)
        "popl %ebp\n"
        "ret\n\n")))))




;; When a lambda is encountered, push it to the delayed-functions queue
;; and move its label into %eax.
(define (delay-lambda expr env)
  (match expr
    ((lambda ,args ,body)
     (let ((g (gensym)))
       (set! delayed-functions
             (append delayed-functions (list (list g expr env))))
       (list
        "movl $" g ", %eax  # lambda pointer\n")))))


;; From left to right, compile the expressions with the current environment,
;; push the results onto the stack, then compile the body with the new
;; environment (old + new bindings).
(define (compile-let expr env)
  (match expr
    ((let ,bindings ,body)
     (let ((new-env (env-update env (map car bindings))))
       (list
        "                            # begin let\n"
        (map (lambda (b)
               (list (compile-expr (cadr b) env) ; Use the old env
                     "pushl %eax\n"
                     (push-root)))
             bindings)
        (compile-expr body new-env)
        (map (lambda (x) (pop-root)) bindings)
        "addl $" (* 4 (length bindings)) ", %esp  # exiting let\n")))))


;; Compile a conditional.
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


;; Generate a number.
(define (gen-number n)
  ;; Boxed version
  ;; (list
  ;;  "movl $" n ", %eax\n"
  ;;  "pushl %eax\n"
  ;;  "call __boxint\n"
  ;;  "addl $4, %esp            # end_number\n"
  ;;  ))
  (list
   "movl $" (* 4 n) ", %eax\n"))

;; Generate a boolean.
(define (gen-bool b)
  (list
   "movl $" (if b *true* *false*) " ,%eax\n"))

(define (gen-char c)
  (list
   (gen-number (char->integer c))
   "pushl %eax\n"
   (gen-number 1)
   "pushl %eax\n"
   (gen-null)
   "pushl %eax\n"
   "call __integerToChar\n"
   "addl $12, %esp\n"))


;; Generate null.
(define (gen-null)
  (list "movl $" *null* ", %eax\n"))


(define (gen-string str env)
  3)


;; Accessing a local variable is done through the stack.
;; Accessing a global variable is done with its label in the .data section.
(define (gen-variable-access var env)
  (match (env-lookup env var)
    ((,varname local ,offset) (list "movl " (- (env-fs env) offset word-size) "(%esp), %eax\n"))
    ((,varname global ,label) (list "movl " label ", %eax\n"))))


;; Push (in reverse order) the arguments of a function.
(define (push-args args env)
  (list (let loop ((env env) (args (reverse args)) (acc '()))
          (if (null? args)
              acc
              (loop (env-fs++ env)
                    (cdr args)
                    (append acc
                            (list (compile-expr (car args) env)
                                  "pushl %eax\n"
                                  (push-root))))))))

;; Uncomment these to deactivate all the GC routine calls.
(define (push-root) "")
(define (pop-root) "")
(define (push-frame) "")
(define (pop-frame) "")

;; (define (push-root)
;;   (list
;;    "pushl %eax\n"
;;    "pushl %esp\n"
;;    "call pushRoot\n"
;;    "addl $4, %esp\n"
;;    "popl %eax\n"))

;; (define (pop-root)
;;   (list
;;    "pushl %eax\n"
;;    "call popRoot\n"
;;    "popl %eax\n"))

;; (define (push-frame)
;;   (list
;;    "pushl %eax\n"
;;    "call pushFrame\n"
;;    "popl %eax\n"))

;; (define (pop-frame)
;;   (list
;;    "pushl %eax\n"
;;    "call popFrame\n"
;;    "popl %eax\n"))


;; Generate a call to a Scheme function.
(define (gen-fun-call f args env)
  (list
   (push-frame)
   (push-args args env)
   (compile-expr f (env-fs+ (length args) env))
   "call *%eax\n"
   (map (lambda (x) (pop-root)) args)
   "addl $" (* 4 (length args)) ", %esp # cleaning up function\n"
   (pop-frame)))



;; Generate a call to a primitive (C) function.
(define (gen-prim-call primitive args env)
  (match primitive
    ((,f ,nb-args ,label)
     (list
      (push-frame)
      (push-args args env)
      (gen-number nb-args)
      "pushl %eax\n"
      "pushl $2\n"
      "call " label "\n"
      (map (lambda (x) (pop-root)) args)
      "addl $" (* 4 nb-args) ", %esp\n"
      (pop-frame)))))



;; A set! for a global variable is translated into a move
;; from %eax to the label.
(define (compile-set! env var expr)
  (match (env-lookup env var)
    ((,v global ,label) (list (compile-expr expr env)
                              "movl %eax, " label "\n"))))



;; To make a closure:
;; 1. Create a vector length n+1 (where n is the number of free variables).
;; 2. Copy the lambda pointer into the first cell.
;; 3. Copy the free variables into the remaining cells.
(define (gen-make-closure fn captures env)
  (let ((size (+ 1 (length captures))))
    (list
     ;; Create vector
     "\n# make-closure\n"
     (gen-number size)
     "pushl %eax\n"
     "pushl $1\n"                       ; unused; for C compatibility
     "pushl $2\n"                       ; unused; for C compatibility
     "call __createLambda\n"
     "addl $12, %esp\n"
     "pushl %eax\n"                     ; Push lambda addr

     (compile-expr fn (env-fs++ env))
     "pushl %eax\n"
     "pushl 4(%esp)\n"
     "pushl $3\n"     ; unused; for C compatibility
     "pushl $2\n"     ; unused; for C compatibility
     "call __lambdaSetCode\n"
     "addl $16, %esp\n"

     ;; Add function + captured variables
     (let loop ((i 0) (cs captures))
       (if (null? cs)
           '()
           (cons (list (compile-expr (car cs) (env-fs++ env)) ; don't forget env-fs++, we pushed.
                       "pushl %eax\n"
                       (gen-number i)
                       "pushl %eax\n"
                       "pushl 8(%esp)\n"
                       "pushl $3\n"     ; unused; for C compatibility
                       "pushl $2\n"     ; unused; for C compatibility
                       "call __lambdaSet\n"
                       "addl $20, %esp\n")
                 (loop (+ i 1) (cdr cs)))))
     "movl 0(%esp), %eax\n"
     "addl $4, %esp\n"                  ; Remove vector addr
     "# end of make-closure\n\n")))


;; Accessing the code of a closure is done by indexing the first cell of
;; the closure vector.
(define (gen-closure-code clo env)
  (list
   (compile-expr clo env)
   "pushl %eax\n"
   "pushl $2\n"                         ; unused; for C compatibility
   "pushl $2\n"                         ; unused; for C compatibility
   "call __lambdaGetCode\n"
   "addl $12, %esp\n"
   ))


;; Accessing the i'th variable of a closure is done by indexing the i+1'th
;; cell of the closure vector.
(define (gen-closure-ref clo i env)
  (list
   (gen-number i)
   "pushl %eax\n"
   (compile-expr clo (env-fs++ env))
   "pushl %eax\n"
   "pushl $2\n"
   "pushl $2\n"
   "call __lambdaRef\n"
   "addl $16, %esp\n"
   ))
