;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

;; make-token : type+value -> int -> int -> token
(define (make-token tv line col)
  (if tv
      (list 'token tv line col)
      (list 'token-error line col)))


;; token? : any -> bool
(define (token? t)
  (and (list? t)
       (= (length t) 4)
       (eq? (car t) 'token)))

(define (token-error? t)
  (and (list? t)
       (not (null? t))
       (eq? (car t) 'token-error)))

;; token-type : token -> type
;;
;; Accessor used to get the type of a token
(define (token-type t)
  (and (token? t)
       (car (token-symbol t))))

;; token-value : token -> value|#f
;;
;; Accesor used to get the value of a token.
;; If the token has no associated value (e.g. keywords),
;; #f is returned.
(define (token-value t)
  (and (token? t)
       (cdr (token-symbol t))))

;; token-symbol : token -> type+value
(define (token-symbol t)
  (and (token? t)
       (list-ref t 1)))

;; token-line : token -> int
(define (token-line t)
  (and (token? t)
       (list-ref t 2)))

;; token-col : token -> int
(define (token-col t)
  (and (token? t) (list-ref t 3)))
