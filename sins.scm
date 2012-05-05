(include "src/frontend/lexer.scm")
(include "src/frontend/token.scm")
(include "src/frontend/reader.scm")
(include "src/frontend/parser.scm")
(include "src/frontend/conversion.scm")

(include "src/backend/codegen.scm")
(include "src/backend/env.scm")

(include "src/utils/utilities.scm")

(define sins-path #f)


(define (compile-file src dest keep-asm?)
  (if (not sins-path)
      (begin
        (display "Vous devez définir la variable sins-path dans sins.scm\n")
        (exit 1)))

  (let* ((backend-dir (string-append sins-path "src/backend/"))
         (runtime-tokenization (lex-from-file (string-append
                                               backend-dir "runtime.scm")))
         (program-tokenization (lex-from-file src))
         (asm-tree (-> (append runtime-tokenization program-tokenization)
                       parse
                       expand
                       alpha-conv
                       assign-conv
                       closure-conv
                       compile))
         (asm-code (with-output-to-string "" (lambda () (print asm-tree)))))
    (call-with-output-file (string-append src ".s")
      (lambda (p)
        (display asm-code p)))
    (shell-command (string-append "gcc -m32 -O1 "
                                  "-mpreferred-stack-boundary=2 "
                                  "-fleading-underscore "
                                  "-g -o " dest " "
                                  backend-dir "*.o "
                                  (string-append src ".s")))
    (if (not keep-asm?)
        (shell-command (string-append "rm " src ".s")))))
