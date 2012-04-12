(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
;(include "runtime.scm")


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

(define (gen-global-vars expr)
  (map (lambda (var)
         `("glob_" ,var ": .long 0\n"))
       (fv expr)))

(define (lookup sym env)
  (let ((x (assq sym env)))
    (if x
        x
        `(,sym global ,(string-append "glob_" (symbol->string sym))))))


(define (compile expr)
  (let ((asm-code (compile-expr expr '()))
        (delayed-asm (compile-delayed-lambdas)))
    (list asm-code
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
    (,s when (symbol? s) (gen-variable-access s env))
    ((let ,args ,body) (compile-let expr env))
    ((lambda ,args ,body) (delay-lambda expr env))
    ((,f . ,args)
     (let ((primitive (assq f primitive-funcs)))
       (if primitive
           (gen-prim-call primitive args env)
           (gen-fun-call f args env))))
    (,_ (error "unknown expression"))))

(define (update-env env args)
  (let loop ((new-env env) (args args) (fs -1))
    (if (null? args)
        new-env
        (loop (cons (list (car args) 'local fs) new-env)
              (cdr args)
              (- fs 1)))))

(define (compile-lambda sym fn env)
  (match fn
    ((lambda ,args ,body)
     (list "\n" sym ":\n"
           (compile-expr body (update-env env args))
           "ret\n\n"))))

(define (delay-lambda expr env)
  (match expr
    ((lambda ,args ,body)
     (let ((g (gensym)))
       (set! delayed-functions
             (append delayed-functions (list (list g expr env))))
       (list
        "movl " g ", %eax  # lambda pointer\n")))))


(define (compile-let expr env)
  (match expr
    ((let ,bindings ,body)
     ;; Update the environment.
     (let ((new-env (let loop ((fs 0) (bindings (reverse bindings)) (new-env '()))
                      (if (null? bindings)
                          new-env
                          (loop (- fs 1)
                                (cdr bindings)
                                (cons (list (caar bindings)
                                            'local
                                            fs)
                                      new-env))))))
       (list (map (lambda (b)
                    (list (compile-expr (cadr b) env)
                          "pushl %eax\n"))
                  bindings)
             (compile-expr body new-env)
             "addl $" (* 4 (length bindings)) ", %esp  # exiting let\n")))))





(define (gen-number n)
  (list
   "movl __INT_TYPE__, %eax\n"
   "pushl %eax\n"
   "movl $" n ", %eax\n"
   "pushl %eax\n"
   "call __box\n"
   "addl $8, %esp\n"
   ))
;; (list
;;  "movl $" n ", %eax\n"
;;  "mul  $4, %eax\n"))


(define (gen-variable-access var env)
  (match (lookup var env)
    ((,varname local ,offset) (list "movl " (* offset 4) "(%esp), %eax\n"))
    ((,varname global ,label) (list "movl " label ", %eax\n"))))



(define (gen-fun-call f args env)
  (list
   (map (lambda (a)
          (list (compile-expr a env)
                "pushl %eax\n")) args)
   (gen-variable-access f env)
   "call %eax\n"
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
