(include "frontend/lexer.scm")
(include "frontend/token.scm")
(include "frontend/reader.scm")
(include "frontend/parser.scm")
(include "frontend/conversion.scm")

(include "backend/codegen.scm")
(include "backend/env.scm")

(include "utils/utilities.scm")



(define (compile-file src dest)
  (let* ((asm-tree (-> src
                       lex-from-file
                       parse
                       compile))
         (asm-code (with-output-to-string "" (lambda () (print asm-tree)))))
    (call-with-output-file dest
      (lambda (p)
        (display asm-code p)))))
