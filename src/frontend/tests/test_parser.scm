(include "../parser.scm")

(define (e->string e)
  (with-output-to-string "" (lambda () (write e))))

(define (test-parse e-in e-out)
  (equal? (parse (lex-from-string (e->string e-in)))
          (list e-out)))

;; Numerique
(define (test-number)
  (let ((e-in 42)
        (e-out 42))
    (test-parse e-in e-out)))

;; Char
(define (test-char)
  (let ((e-in #\a)
        (e-out #\a))
    (test-parse e-in e-out)))

;; Symbol
(define (test-symbol)
  (let ((e-in 'sym)
        (e-out 'sym))
    (test-parse e-in e-out)))

;; String, non-empty
(define (test-string1)
  (let ((e-in "allo")
        (e-out "allo"))
    (test-parse e-in e-out)))

;; String, empty
(define (test-string2)
  (let ((e-in "")
        (e-out ""))
    (test-parse e-in e-out)))

;; Binop 1
(define (test-binop1)
  (let ((e-in '(+))
        (e-out '(+)))
    (test-parse e-in e-out)))

;; Binop 2
(define (test-binop2)
  (let ((e-in '(+ 1))
        (e-out '(+ 1)))
    (test-parse e-in e-out)))

;; Binop 3
(define (test-binop3)
  (let ((e-in '(+ 1 2))
        (e-out '(+ 1 2)))
    (test-parse e-in e-out)))




;; (pp (parse (lex-from-string (e->string e))))
(display (test-number))(newline)
(display (test-char))(newline)
(display (test-symbol))(newline)
(display (test-string1))(newline)
(display (test-string2))(newline)
(display (test-binop1))(newline)
(display (test-binop2))(newline)
(display (test-binop3))(newline)
