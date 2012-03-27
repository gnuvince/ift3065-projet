(include "../ir/match.scm")
(include "../ir/simplify.scm")
(include "../utils/utilities.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")

(define (translate ast)
  (comp-function "_main" ast '()))

(define (comp-function name ast cte)
  (pp ast)
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

         (,s when (symbol? s)
             (let ((x (assq s cte)))
               (if x
                   (gen-parameter (cdr x))
                   (error "invalid identifier"))))
         
         ((lambda ,params ,expr)
          (let ((new-cte (append (map cons params (range 1 (length params))) cte))) 
            (comp-function (gen-label "anonyme")
                           expr
                           new-cte)))
         (,_
          (error "Invalid form"))))
                      

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
