#! /usr/bin/env gsi

(define (translate ast)
  (comp-function "_main" ast))

(define (comp-function name expr)
  (gen-function name
                (comp-expr expr 0 '((argc . 1)))))

(define (comp-expr expr fs cte) ;; fs = frame size
                                ;; cte = compile time environment

  (cond ((number? expr)
         (gen-literal expr))

        ((symbol? expr)
         (let ((x (assoc expr cte)))
           (if x
               (let ((index (cdr x)))
                 (gen-parameter (+ fs index)))
               (error "undefined variable" expr))))

        ((and (list? expr)
              (= (length expr) 3)
              (eq? (list-ref expr 0) 'let))
         (let ((binding (list-ref (list-ref expr 1) 0)))
           (gen-let (comp-expr (list-ref binding 1)
                               fs
                               cte)
                    (comp-expr (list-ref expr 2)
                               (+ fs 1)
                               (cons (cons (list-ref binding 0)
                                           (- (+ fs 1)))
                                     cte)))))

        ((and (list? expr)
              (= (length expr) 3)
              (member (list-ref expr 0) '(+ - * / bitwise-and bitwise-ior bitwise-xor)))
         (gen-bin-op
          (case (list-ref expr 0)
            ((+) "add")
            ((-) "sub")
            ((*) "imul")
            ((/) "idiv")
            ((bitwise-and) "and")
            ((bitwise-ior) "or")
            ((bitwise-xor) "xor"))
          (comp-expr (list-ref expr 2) fs cte)
          (comp-expr (list-ref expr 1) (+ fs 1) cte)))

        ((and (list? expr)
              (= (length expr) 3)
              (member (list-ref expr 0) '(arithmetic-shift)))
         (let ((opnd2 (comp-expr (list-ref expr 2) fs cte))
               (opnd1 (comp-expr (list-ref expr 1) (+ fs 1) cte))
               (reg-lbl (gen-label "REG"))
               (neg-lbl (gen-label "NEG"))
               (end-lbl (gen-label "END")))
           (gen
            opnd1
            "    pushl   %eax\n"
            opnd2
            "    movl    %eax, %ecx\n"
            "    popl    %eax\n"

            ;; If shift amount greater or equal to 32
            ;; the result should be 0
            "    cmp     $32, %ecx\n"
            "    movl    $0, %edx\n"
            "    cmovge  %edx, %eax\n"
            "    jge   " end-lbl "\n"

            ;; If shift amount less or equal to -32,
            ;; -1 if opnd1 is negative,
            ;;  0 otherwise
            "    cmp     $-32, %ecx\n"
            "    jg    " reg-lbl "\n"
            "    cmp     $0,  %eax\n"
            "    movl    $0,  %ecx\n"
            "    movl    $-1, %eax\n"
            "    cmovge  %ecx, %eax\n"
            "    jmp   " end-lbl "\n"

            reg-lbl ":\n"
            ;; Check if the shift amount is negative
            "    cmp     $0, %ecx\n"
            "    jl    " neg-lbl "\n"

            ;; Positive shift
            "    sall    %cl, %eax\n"
            "    jmp   " end-lbl "\n"

            ;; Negative shift
            neg-lbl ":\n"
            "    xorl    $-1, %ecx\n"
            "    incl    %ecx\n"
            "    sarl    %cl, %eax\n"

            end-lbl ":\n"
            )))

        ((and (list? expr)
              (= (length expr) 2)
              (member (list-ref expr 0) '(integer-length)))
         (let ((opnd (comp-expr (list-ref expr 1) fs cte))
               (loop-lbl (gen-label "LOOP"))
               (start-lbl  (gen-label "START"))
               (end-lbl  (gen-label "END")))
           (gen
            opnd
            "    movl    %eax, %ecx\n"
            ;; Test the most significant bit
            "    cmpl    $0, %eax\n"
            "    jge    " start-lbl "\n"
            ;; Find the inverse if negative and
            ;; add 1 for symmetry with positive case
            "    xor    $-1, %ecx\n"
            start-lbl ":\n"
            "    movl    $0, %eax\n"
            loop-lbl ":\n"
            "    testl   %ecx, %ecx\n"
            "    jz    " end-lbl "\n"
            "    sarl    $1, %ecx\n"
            "    inc     %eax\n"
            "    jmp   " loop-lbl "\n"
            end-lbl ":\n"
            )))

        (else
         (error "comp-expr cannot handle expression"))))

;;; Code generation for x86-32 using GNU as syntax

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

(define (gen-bin-op oper opnd1 opnd2)
  (gen opnd1

       ;; This is slow:
       ;; "    addl    $-4, %esp\n"
       ;; "    movl    %eax, (%esp)\n"

       ;; This is faster:
       "    pushl   %eax\n"

       opnd2

       "    " oper "l    (%esp), %eax\n"
       "    addl    $4, %esp\n"))

(define (gen-let val body)
  (gen val
       "    pushl   %eax\n"
       body
       "    addl    $4, %esp\n"))

(define (gen-parameter i)
  (gen "    movl    " (* 4 i) "(%esp), %eax\n"))

(define (gen-literal n)
  (gen "    movl    $" n ", %eax\n"))

;; Main program:

(define (main source-filename)
  (let ((ast (parse source-filename)))
    (let ((code (translate ast)))
      (with-output-to-file
          (string-append (path-strip-extension source-filename) ".s")
        (lambda ()
          (print code))))))
