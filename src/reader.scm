;; SINS
;; IFT3065 - H12
;; Vincent Foley-Bourgon (FOLV08078309)
;; Eric Thivierge (THIE09016601)

(include "token.scm")
(include "lexer.scm")
(include "utilities.scm")

(define stream #f)

(define (<list> lst)
  (if (stream 'empty)
      (error "Incomplete form, EOF reached")
      (let ((next (token-value (stream 'next))))
        (cond ((eq? 'close-paren next)
               (stream 'pop)
               (reverse lst))
              ((eq? 'dot next)
               (stream 'pop)
               (let ((dp (append (reverse lst) (<datum>))))
                 (consume-rparen)
                 dp))
               (else
                (<list> (cons (<datum>)
                              lst)))))))

(define (<datum>)
  (if (stream 'empty)
      (error "Datum expected")
      (let ((next (token-value (stream 'pop))))
        (cond ((eq? 'quote-symbol next)
               (list 'quote (<datum>)))
              ((eq? 'comma next)
               (list 'unquote (<datum>)))
              ((eq? 'backquote next)
               (list 'quasiquote (<datum>)))
              ((eq? 'comma-at next)
               (list 'unquote-splicing (<datum>)))
              ((eq? 'open-paren next)
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
        (sins-read-aux (cons (<datum>) ast))))

(define (sins-read token-list)
  (begin
    (set! stream (make-token-stream token-list))
    (sins-read-aux '())))


;;;;;;;;;;;;;;;;;;;;
;; (define (sins-read token-list)
;;   (define (consume-pair tree)
;;     (cond ((stream 'empty)
;;            (error "Early EOF"))
;;           ((open-paren-token? (stream 'next))
;;            (begin
;;              (consume-lparen)
;;              (consume-pair (cons (consume-pair '())
;;                                  tree))))
;;           ((close-paren-token? (stream 'next))
;;            (begin
;;              (consume-rparen)
;;              (reverse tree)))
;;           (else
;;            (consume-pair (cons (token-value (stream 'pop))
;;                                tree)))))
  
;;   (define (sins-read-aux tree)
;;     (cond ((stream 'empty)
;;            (reverse tree))
;;           ((open-paren-token? (stream 'next))
;;            (begin
;;              (consume-lparen)
;;              (sins-read-aux (cons (consume-pair '())
;;                              tree))))
;;           ((close-paren-token? (stream 'next))
;;            (error "Datum or EOF expected"))
;;           (else
;;            (sins-read-aux (cons (token-value (stream 'pop))
;;                            tree)))))
  
;;   (begin
;;     (set! stream (make-token-stream token-list))
;;     (sins-read-aux '())))
