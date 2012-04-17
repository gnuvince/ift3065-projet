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


;; Add a local symbol tuple to the environment:
;; (<symbol> local <offset>)
;; Decrease the frame size.
(define (env-add-local-symbol env s)
  (list (+ (env-fs env) word-size)
        (cons (list s 'local (env-fs env))
              (env-symbols env))))


;; Find a symbol in the environment; if it doesn't exist, return a
;; global symbol tuple.
(define (env-lookup env s)
  (let ((x (assq s (env-symbols env))))
    (if x
        x
        `(,s global ,(string-append "glob_" (symbol->string s))))))


;; Add some local bindings to the environment.
(define (env-update env args)
  (foldr (lambda (sym env) (env-add-local-symbol env sym))
         env
         args))
