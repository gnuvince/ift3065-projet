(load "token.scm")
(load "lexer.scm")
(load "reader.scm")
(load "tests/tests.scm")

(define (read string)
  (sins-read (lex-from-string string)))

(define (incorrect? f) (with-exception-catcher (lambda (e) #t)
                                               (lambda () (f) #f)))

(define (test-datum)
  (and (equal? (read "#f") '(false))
       (equal? (read "#t") '(true))
       (equal? (read "#f #f") '(false false))
       (equal? (read "0") '(0))
       (equal? (read "12") '(12))
       (equal? (read "\"\"") '(""))
       (equal? (read "\"x\"") '("x"))
       (equal? (read "a") '(a))))


(define (test-proper-list)
  (and (equal? (read "()") '(()))
       (equal? (read "(1)") '((1)))
       (equal? (read "(abc 1)") '((abc 1)))
       (equal? (read "(\"hello\" #\\a)") '(("hello" #\a)))
       (equal? (read "(() ())") '( (() ()) ))
       (equal? (read "(1 (2 (3 (4) 5) 6) 7)") '((1 (2 (3 (4) 5) 6) 7)))
       (equal? (read "(f x (+ x 1))") '((f x (+ x 1))))
       ))

(define (test-dotted-list)
  (and (equal? (read "(1 . 2)") '((1 . 2)))
       (equal? (read "(1 . (2))") '((1 2)))
       (equal? (read "(1 . (2 3 . 4))") '((1 2 3 . 4)))))


(define (test-syntax-errors)
  (and
   (incorrect? (lambda () (read "(")))
   (incorrect? (lambda () (read ")")))
   (incorrect? (lambda () (read ".")))
   (incorrect? (lambda () (read "(.)")))
   (incorrect? (lambda () (read "(a .)")))
   (incorrect? (lambda () (read "(. a)")))
   (incorrect? (lambda () (read "(()")))))


(define (main)
  (display "\nREADER TESTS\n")
  (let ((result (run-tests (list test-datum
                                 test-proper-list
                                 test-dotted-list))))
    (if result
        (exit 0)
        (exit 1))))
