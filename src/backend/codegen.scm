(include "../ir/match.scm")
(include "../ir/simplify.scm")
(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")

(define main-label "main")
(define false-sym 4294967294) ; 0xFFFFFFFE

(define funcs '())

(define (translate ast)
  (set! funcs '())
  (comp-function main-label ast '())
  funcs)

(define (comp-function name ast env)
  (set! funcs
        (cons (gen-function name
                            (compile ast env)
                            env)
              funcs))
  name)

(define (compile ast env)
  (match ast
    (,n when (number? n)
        (gen-literal n))

    (,b when (boolean? b)
        (if b
            (gen-literal 1)
            (gen-literal false-sym)))

    ((,op ,op1 ,op2) when (member op '(+ - *))
     (gen-bin-op op
                 (compile op1 env)
                 (compile op2 env)))

    ((,op ,op1 ,op2) when (member op '(/ modulo))
     (gen-div-op op
                 (compile op1 env)
                 (compile op2 env)))

    ((,op ,op1 ,op2) when (member op '(< <= = >= >))
     (gen-cmp-op op
                 (compile op1 env)
                 (compile op2 env)))

    ((if ,ex1 ,ex2) (compile `(if ,ex1 ,ex2 ()) env))

    ((if ,ex1 ,ex2 ,ex3) (gen-if (compile ex1 env)
                                 (compile ex2 env)
                                 (compile ex3 env)))


    (,s when (symbol? s)
        (let ((x (assq s env)))
          (if x
              (gen-parameter (cdr x))
              (error "invalid identifier"))))

    ((lambda ,params ,expr)
     (let* ((new-env (append (map cons params (range 1 (length params))) env))
            (func-name (comp-function (gen-label "anonyme")
                                      expr
                                      new-env)))
       (gen "    movl " func-name ", %eax\n")))


    ((,f ,args)
     (begin
       (compile f env)
       (compile args env)
       (gen (compile args env)
            "call %eax\n"
            "addl $4, (%esp)\n")))

    (,_
     (error "Unrecognized form: " ast))))


(define gen list)

(define gen-label
  (let ((_i 0))
    (lambda (prefix)
      (let ((i _i))
        (set! _i (+ i 1))
        (string-append prefix (number->string i))))))

(define (gen-function name code env)
  (gen name ":\n"
       code
       "    ret\n"))

(define (gen-bin-op op op1 op2)
  (let ((oper
         (case op
           ((+) "addl")
           ((-) "subl")
           ((*) "imull")
           (else (error "Invalid operator")))))
    (gen op1
         "    pushl   %eax\n"

         op2

         "    " oper "    (%esp), %eax\n"
         "    addl    $4, %esp\n")))


(define (gen-div-op op op1 op2)
  (gen op1
       "    pushl    %eax\n"
       op2
       "    movl     %eax, %ecx\n"
       "    popl     %eax\n"
       "    cdq\n"
       "    idivl    %ecx\n"
       (if (eq? op 'modulo)
           "    movl    %edx, %eax\n"
           "")))



;; Comparison operations are implemented by putting 1 in %eax if
;; the comparison returns true, and 0 otherwise.  The generated code
;; is inefficient, but simple and easy to understand.
(define (gen-cmp-op op op1 op2)
  (let ((oper
         (case op
           ((<)  "jl")
           ((<=) "jle")
           ((=)  "je")
           ((>=) "jge")
           ((>)  "jg")))
        (label (string-append "cmp_op_" (symbol->string (gensym)))))
    (gen op1
         "    pushl   %eax\n"
         op2
         "    cmp     %eax, (%esp)\n"
         "    movl    $1, %eax\n"
         "    " oper "      " label "\n"
         "    movl    $" false-sym ", %eax\n"
         label ":\n"
         "    addl    $4, %esp\n")))



(define (gen-if ex1 ex2 ex3)
  (let* ((g (gensym))
         (else-label (string-append "else_" (symbol->string g)))
         (endif-label (string-append "endif_" (symbol->string g))))
  (gen ex1
       "    pushl   %eax\n"
       (gen-literal false-sym)
       "    cmp     %eax, (%esp)\n"
       "    je      " else-label "\n"
       ex2
       "    jmp     " endif-label "\n"
       else-label ":\n"
       ex3
       endif-label ":\n"
       "    addl    $4, %esp\n")))

(define (gen-let val body)
  (gen val
       "    pushl   %eax\n"
       body
       "    addl    $4, %esp\n"))

(define (gen-parameter i)
  (gen "    movl    " (* 4 i) "(%esp), %eax\n"))

(define (gen-literal n)
  (gen "    movl    $" n ", %eax\n"))

(define (main ast)
  (print ".text\n")
  (print ".globl " main-label "\n")
  (print (translate ast)))
