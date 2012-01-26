(define (member? lst a)
  (cond ((null? lst)
         #f)
        ((eq? (car lst) a)
         #t)
        (else
         (member? (cdr lst) a))))

(define (make-token-stream tokens)
  (let ((stream tokens))
    (define (dispatch msg)
      (case msg
        ((empty?)  (null? stream))
        ((next)    (cond ((dispatch 'empty?)
                          '())
                         (else
                          (car stream))))
        ((advance) (cond ((dispatch 'empty?)
                          (error "Advancing beyond last stream element"))
                         (else
                          (set! stream (cdr stream)))))
        ((pop) (begin
                 (let ((tok (dispatch 'next)))
                   (dispatch 'advance)
                   tok)))))
    dispatch))

(define (make-err-msg msg tok)
  (string-append msg
                 (token-line tok)
                 " col "
                 (token-col tok)
                 "\n"))
