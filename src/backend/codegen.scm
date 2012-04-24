(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
(include "env.scm")
;(include "runtime.scm")

(define *false* 1)

(define delayed-functions '())

(define primitive-funcs '(;symbol         # args primitive name
                          (%*                  2 "__mul")
                          (%*-aux              2 "PRIM_MUL_AUX")
                          (%+                  2 "__add")
                          (%+-aux              2 "PRIM_ADD_AUX")
                          (%-                  2 "__sub")
                          (%--aux              2 "PRIM_SUB_AUX")
                          (%<                  2 "__lt")
                          (%<=                 2 "__le")
                          (%=                  2 "__equal")
                          (%>                  2 "__gt")
                          (%>=                 2 "__ge")
                          (%car                1 "PRIM_CAR")
                          (%cdr                1 "PRIM_CDR")
                          (%cons               2 "PRIM_CONS")
                          (%eq?                2 "PRIM_EQ_INTERRO")
                          (%length-aux         1 "PRIM_LENGTH")
                          (%list->string       1 "PRIM_LIST_STRING")
                          (%null?              1 "PRIM_NULL")
                          (%number->string-aux 1 "PRIM_NUMBER_STRING")
                          (%number?            1 "PRIM_NUMBER")
                          (%pair?              1 "PRIM_PAIR")
                          (%quotient           2 "PRIM_QUOTIENT")
                          (%remainder          2 "PRIM_REMAINDER")
                          (%set-car!           2 "PRIM_SET_CAR")
                          (%set-cdr!           2 "PRIM_SET_CDR")
                          (%string->list       1 "PRIM_STRING_LIST")
                          (%string?            1 "PRIM_STRING")
                          (%substring-aux1     2 "PRIM_SUBSTRING1")
                          (%substring-aux2     2 "PRIM_SUBSTRING2")
                          (%symbol?            1 "PRIM_SYMBOL")
                          (%write-char         1 "PRIM_WRITE_CHAR")
                          (%write-list         1 "PRIM_WRITE_LIST")
                          ))


(define (char->label-aux c)
  (case c
    ((#\!) "bang")
    ((#\$) "dollar")
    ((#\%) "percent")
    ((#\&) "ampersand")
    ((#\*) "star")
    ((#\+) "plus")
    ((#\-) "minus")
    ((#\.) "dot")
    ((#\/) "slash")
    ((#\:) "colon")
    ((#\<) "lt")
    ((#\=) "eq")
    ((#\>) "gt")
    ((#\?) "interrogation")
    ((#\@) "at")
    ((#\^) "carret")
    ((#\_) "underscore")
    ((#\~) "tilde")
    (else (make-string 1 c))))

(define (symbol->label sym)
  (let loop ((str (symbol->string sym)) (i 0) (acc ""))
    (if (>= i (string-length str))
        acc
        (loop str
              (+ i 1)
              (string-append acc (char->label-aux (string-ref str i)))))))


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
     delayed-asm)))


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
    ((,f . ,args)
     (let ((primitive (assq f primitive-funcs)))
       (if primitive
           (gen-prim-call primitive args env)
           (gen-fun-call f args env))))
    (,_ (error "unknown expression"))))


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
        else-label ":\n"
        else-comp
        endif-label ":\n"
        )))))



(define (gen-number n)
  ;;(list "movl $" n ", %eax\n"))
;; Boxed version
  (list
   "movl $" n ", %eax\n"
   "pushl %eax\n"
   "call __boxint\n"
   "addl $4, %esp            # end_nubmer\n"
   ))
;; More efficient version if we don't use the __box primitive.
;; (list
;;  "movl $" n ", %eax\n"
;;  "mul  $4, %eax\n"))


(define (gen-variable-access var env)
  (match (env-lookup env var)
    ((,varname local ,offset) (list "movl " (- (env-fs env) offset word-size) "(%esp), %eax\n"))
    ((,varname global ,label) (list "movl " label ", %eax\n"))))



(define (gen-fun-call f args env)
  (list
   (let loop ((env env) (args args))
     (if (null? args)
         '()
         (cons (list (compile-expr (car args) env)
                     "pushl %eax\n")
               (loop (env-fs++ env) (cdr args)))))
   ;(gen-variable-access f env)
   (compile-expr f env)
   "call *%eax\n"
   "addl $" (* 4 (length args)) ", %esp # cleaning up function\n"))




(define (gen-prim-call primitive args env)
  (match primitive
    ((,f ,nb-args ,label)
     (list
      (map (lambda (a)
             (list (compile-expr a env)
                   "pushl %eax\n")) args)
      "call " label "\n"
      "addl $" (* 4 nb-args) ", %esp # cleaning up primitive\n"))))



(define (gen-bool b)
  (list "movl $"
        (if b 0 *false*)
        " ,%eax\n"))
