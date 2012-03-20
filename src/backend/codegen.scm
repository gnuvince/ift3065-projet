(include "../ir/match.scm")
(include "../ir/simplify.scm")
(include "../frontend/lexer.scm")
(include "../frontend/reader.scm")
(include "../frontend/parser.scm")

(define (translate ast)
  (comp-function "_main" ast))

(define (comp-function name ast)
  (gen-function name
                (compile ast)))

(define (compile ast)
  (match ast
         (,n when (number? n)
             (gen-literal n))
         ((,op ,op1 ,op2) when (member op '(+ - / *))
          (gen-bin-op op
                      (compile op1)
                      (compile op2)))
         (,_
          (error "Invalid form"))))
                      

(define gen list)

(define gen-label
  (let ((_i 0))
    (lambda (prefix)
      (let ((i _i))
        (set! _i (+ i 1))
        (string-append prefix (number->string i))))))

(define (gen-function name code)
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
