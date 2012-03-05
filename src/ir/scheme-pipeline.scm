#! /usr/bin/env gsi

;; File: "scheme-pipeline.scm"

(include "match.scm")
(include "expand.scm")
(include "alpha-conv.scm")
(include "assign-conv.scm")
(include "closure-conv.scm")
(include "var-analysis.scm")
(include "set.scm")

(define (pipeline filename)

  (let ((source (with-input-from-file filename read-all)))
    (print "---------------------------- source:\n")
    (for-each pretty-print source)

    (let ((after-macro-exp (map expand source)))
      (print "---------------------------- after macro-expansion:\n")
      (for-each pretty-print after-macro-exp)

      (let ((after-alpha-conv (map alpha-conv after-macro-exp)))
        (print "---------------------------- after alpha-conversion:\n")
        (for-each pretty-print after-alpha-conv)

        (let ((after-assign-conv (map assign-conv after-alpha-conv)))
          (print "---------------------------- after assignment-conversion:\n")
          (for-each pretty-print after-assign-conv)

          (let ((after-closure-conv
                 (append '((define make-closure
                             vector)
                           (define closure-code
                             (lambda (clo) (vector-ref clo 0)))
                           (define closure-ref
                             (lambda (clo i) (vector-ref clo (+ i 1)))))
                         (map closure-conv after-assign-conv))))
            (print "---------------------------- after closure-conversion:\n")
            (for-each pretty-print after-closure-conv)))))))

(define (main . filenames)
  (for-each pipeline filenames))
