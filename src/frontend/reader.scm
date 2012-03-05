;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

;(include "token.scm")
(include "lexer.scm")
(include "../utils/utilities.scm")

(define stream #f)

(define (<list> lst)
  (if (stream 'empty)
      (error "Incomplete form, EOF reached")
      (let* ((next (stream 'next))
             (type (token-type next))
             (value (token-value next)))
        (cond
         ((and (eq? type 'punctuation)
               (eq? value 'close-paren))
          (stream 'pop)
          (reverse lst))

         ((and (eq? type 'punctuation)
               (eq? value 'dot))
          (if (null? lst)
              (error "Ill-formed dotted list")
              (begin
                (stream 'pop)
                (let ((dp (append (reverse lst) (<datum>))))
                  (consume-token 'punctuation 'close-paren "Ill-formed dotted list")
                  dp))))

         (else
          (let ((d (<datum>)))
            (<list> (cons d lst))))))))



(define (<datum>)
  (if (stream 'empty)
      (error "Datum expected")
      (let* ((next (stream 'pop))
             (type (token-type next))
             (value (token-value next)))

        (cond
         ((and (eq? type 'punctuation)
               (eq? value 'quote-prefix))
          (list 'quote-prefix (<datum>)))

         ((and (eq? type 'punctuation)
               (eq? value 'quasiquote-prefix))
          (list 'quasiquote-prefix (<datum>)))

         ((and (eq? type 'punctuation)
               (eq? value 'unquote-prefix))
          (list 'unquote-prefix (<datum>)))

         ((and (eq? type 'punctuation)
               (eq? value 'unquote-splicing-prefix))
          (list 'unquote-splicing-prefix (<datum>)))

         ((and (eq? type 'punctuation)
               (eq? value 'open-paren))
          (<list> '()))

         ((memq type '(boolean number string char ident))
          value)

         (else
          (error "Datum or EOF expected"))))))


(define (sins-read-aux ast)
  (cond ((stream 'empty)
         (reverse ast))
        (else
         (let ((d (<datum>)))
           (sins-read-aux (cons d ast))))))

(define (sins-read token-list)
  (begin
    (set! stream (make-token-stream token-list))
    (sins-read-aux '())))
