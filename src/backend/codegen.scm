(include "../ir/match.scm")
(include "../ir/simplify.scm")
(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")

(define (translate ast)
  (comp-function "_main" ast '()))

(define (comp-function name ast cte)
  (gen-function name
                (compile ast cte)
                cte))

(define (compile ast cte)
  (match ast
    (,n when (number? n)
        (gen-literal n))

    ((,op ,op1 ,op2) when (member op '(+ - / *))
     (gen-bin-op op
                 (compile op1 cte)
                 (compile op2 cte)))

    ((,op ,op1 ,op2) when (member op '(< <= = >= >))
     (gen-cmp-op op
                 (compile op1 cte)
                 (compile op2 cte)))

    ((if ,ex1 ,ex2) (compile `(if ,ex1 ,ex2 ()) cte))

    ((if ,ex1 ,ex2 ,ex3)

    (,s when (symbol? s)
        (let ((x (assq s cte)))
          (if x
              (gen-parameter (cdr x))
              (error "invalid identifier"))))

    ((lambda ,params ,expr)
     (let ((new-cte (append (map cons params (range 1 (length params))) cte)))
       (comp-function (gen-label "anonyme")
                      (compile expr new-cte)
                      new-cte)))
    (,_
     (error "Unrecognized form: " ast))))


(define gen list)

(define gen-label
  (let ((_i 0))
    (lambda (prefix)
      (let ((i _i))
        (set! _i (+ i 1))
        (string-append prefix (number->string i))))))

(define (gen-function name code cte)
  (gen "    .text\n"
       ".globl " name "\n"
       name ":\n"
       code
       "    ret\n"))

(define (gen-bin-op op op1 op2)
  (let ((oper
         (case op
           ((+) "addl")
           ((-) "subl")
           ((/) "idivl")
           ((*) "imull")
           (else (error "Invalid operator")))))
    (gen op1
         "    pushl   %eax\n"

         op2

         "    " oper "    (%esp), %eax\n"
         "    addl    $4, %esp\n")))

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
        (label (symbol->string (gensym))))
    (gen op1
         "    pushl   %eax\n"
         op2
         "    cmp     %eax, (%esp)\n"
         "    movl    $1, %eax\n"
         "    " oper "      " label "\n"
         "    movl    $0, %eax\n"
         label ":\n"
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
  (print (translate ast)))
