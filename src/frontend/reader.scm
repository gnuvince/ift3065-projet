;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

;(include "token.scm")
(include "lexer.scm")
(include "../utils/utilities.scm")

(define open-paren-symbol (string->symbol "open-paren"))
(define close-paren-symbol (string->symbol "close-paren"))

(define stream #f)

(define (<list> lst)
  (if (stream 'empty)
      (error "Incomplete form, EOF reached")
      (let ((next (token-value (stream 'next))))
        (cond ((eq? close-paren-symbol next)
               (stream 'pop)
               (reverse lst))
              ((eq? 'dot next)
               (stream 'pop)
               (let ((dp (append (reverse lst) (<datum>))))
                 dp))
               (else
                (let ((d (<datum>)))
                  (<list> (cons d lst))))))))

(define (<datum>)
  (if (stream 'empty)
      (error "Datum expected")
      (let ((next (token-value (stream 'pop))))
        (cond ((eq? 'quote-prefix next)
               (list 'quote-prefix (<datum>)))
              ((eq? 'unquote-prefix next)
               (list 'unquote-prefix (<datum>)))
              ((eq? 'quasiquote-prefix next)
               (list 'quasiquote-prefix (<datum>)))
              ((eq? 'unquote-splicing-prefix next)
               (list 'unquote-splicing-prefix (<datum>)))
              ((eq? open-paren-symbol next)
               (<list> '()))
              ((or (boolean? next)
                   (number? next)
                   (string? next)
                   (char? next)
                   (symbol? next))
               next)
              (else
               (error "Datum or EOF expected"))))))

(define (sins-read-aux ast)
  (cond ((stream 'empty)
         (reverse ast))
        (else
         (let ((d (<datum>)))
           (sins-read-aux (cons d  ast))))))

(define (sins-read token-list)
  (begin
    (set! stream (make-token-stream token-list))
    (sins-read-aux '())))
