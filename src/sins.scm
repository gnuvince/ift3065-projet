(include "frontend/lexer.scm")
(include "frontend/token.scm")
(include "frontend/reader.scm")
(include "frontend/parser.scm")
(include "frontend/conversion.scm")

(include "backend/codegen.scm")
(include "backend/env.scm")

(include "utils/utilities.scm")



(define (compile-file src dest)
  (let* ((runtime-tokenization (lex-from-file "runtime.scm"))
         (program-tokenization (lex-from-file src))
         (asm-tree (-> (append runtime-tokenization program-tokenization)
                       parse
                       expand
                       alpha-conv
                       assign-conv
                       compile))
         (asm-code (with-output-to-string "" (lambda () (print asm-tree)))))
    (call-with-output-file dest
      (lambda (p)
        (display asm-code p)))))
