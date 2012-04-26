(include "../utils/utilities.scm")

(define word-size 4)

;; Create a new environment with a frame size of 0.
(define (make-env)
  (list 0 '()))

;; Return the frame size of an environment.
(define (env-fs env)
  (car env))

;; Return the list of symbols of an environment.
(define (env-symbols env)
  (cadr env))

(define (env-fs++ env)
  (list (+ (env-fs env) word-size)
        (env-symbols env)))


(define (env-add-symbol env s scope value)
  (list (+ (env-fs env) word-size)
        (cons (list s scope value)
              (env-symbols env))))

;; Add a local symbol tuple to the environment:
;; (<symbol> local <offset>)
;; Decrease the frame size.
(define (env-add-local-symbol env s)
  (env-add-symbol env s 'local (env-fs env)))



;; Find a symbol in the environment; if it doesn't exist, return a
;; global symbol tuple.
(define (env-lookup env s)
  (let ((x (assq s (env-symbols env))))
    (if x
        x
        `(,s global ,(string-append "glob_" (symbol->label s))))))


;; Add some local bindings to the environment.
(define (env-update env args)
  (foldr (lambda (sym env) (env-add-local-symbol env sym))
         env
         args))



(define (char->label-aux c)
  (case c
    ((#\!) "bang")
    ((#\$) "dollar")
    ((#\%) "percent")
    ((#\&) "ampersand")
    ((#\*) "star")
    ((#\+) "plus")
    ((#\-) "minus")
    ((#\.) "dot")
    ((#\/) "slash")
    ((#\:) "colon")
    ((#\<) "lt")
    ((#\=) "eq")
    ((#\>) "gt")
    ((#\?) "interrogation")
    ((#\@) "at")
    ((#\^) "carret")
    ((#\_) "underscore")
    ((#\~) "tilde")
    (else (make-string 1 c))))

;; Rename symbol into something acceptable to assembly.
(define (symbol->label sym)
  (let loop ((str (symbol->string sym)) (i 0) (acc ""))
    (if (>= i (string-length str))
        acc
        (loop str
              (+ i 1)
              (string-append acc (char->label-aux (string-ref str i)))))))
