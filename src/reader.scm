;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(include "token.scm")
(include "lexer.scm")
(include "utilities.scm")

(define stream #f)

(define (sins-read token-list)
  (define (consume-pair tree)
    (cond ((stream 'empty)
           (error "Early EOF"))
          ((open-paren-token? (stream 'next))
           (begin
             (consume-lparen)
             (consume-pair (cons (consume-pair '())
                                 tree))))
          ((close-paren-token? (stream 'next))
           (begin
             (consume-rparen)
             (reverse tree)))
          (else
           (consume-pair (cons (token-value (stream 'pop))
                               tree)))))
  
  (define (sins-read-aux tree)
    (cond ((stream 'empty)
           (reverse tree))
          ((open-paren-token? (stream 'next))
           (begin
             (consume-lparen)
             (sins-read-aux (cons (consume-pair '())
                             tree))))
          ((close-paren-token? (stream 'next))
           (error "Datum or EOF expected"))
          (else
           (sins-read-aux (cons (token-value (stream 'pop))
                           tree)))))
  
  (begin
    (set! stream (make-token-stream token-list))
    (sins-read-aux '())))
