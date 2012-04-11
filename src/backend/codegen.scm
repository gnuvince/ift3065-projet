(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")
(include "../frontend/conversion.scm")
;(include "runtime.scm")

(define (gen-global-vars expr)
  (map (lambda (var)
         `("glob_" ,var ": .long 0\n"))
       (fv expr)))

(define (lookup sym env)
  (let ((x (assq sym env)))
    (if x
        x
        `(,sym global ,(string-append "glob_" (symbol->string sym))))))


(define (compile-let expr env)
  (match expr
    ((let ,bindings ,body)
     ;; Update the environment.
     (let ((new-env (let loop ((fs -1) (bindings (reverse bindings)) (new-env '()))
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
             (compile-expr body new-env))))))


(define (compile-expr expr env)
  (match expr
    (,n when (number? n) (gen-number n))
    (,s when (symbol? s) (gen-variable-access s env))
    ((,f . ,args) (gen-fun-call f args env))
    (,_ (error "unknown expression"))))


(define (gen-number n)
  (list "movl $" n ", %eax\n"))


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
   "call %eax\n"))
