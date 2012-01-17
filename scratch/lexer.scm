(define (make-stream str)
  (let ((current 0)
        (size (string-length str)))
    (lambda (msg)
      (case msg
        ((next) (if (>= current size)
                    #\nul
                    (string-ref str current)))
        ((advance) (set! current (+ current 1)))))))



(define digit (string->list "0123456789"))
(define alpha (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define extended (string->list "!$%&*+-./:<=>?@^_~"))

(define (char-identifier? c)
  (or (member c digit)
      (member c alpha)
      (member c extended)))


(define (tokenize stream)
  (let ((token
         (cond
           ((char=? (stream 'next) #\nul) (consume-eof stream))

           ((char=? (stream 'next) #\()   (consume-open-paren stream))

           ((char=? (stream 'next) #\))   (consume-close-paren stream))

           ((char=? (stream 'next) #\')   (consume-quote stream))

           ((char-identifier? (stream 'next))
            (let ((ident (consume-identifier stream)))
               (or (lexeme-keyword ident)
                   (lexeme-numeric ident)
                   (cons 'ident ident))))


           ((char-whitespace? (stream 'next))
            (begin
              (consume-whitespace stream)
              (tokenize stream)))

           (else (error
                  (string-append "unknown character: " (make-string 1 (stream 'next))))))))
    token))


(define (consume-eof stream)
  (stream 'advance)
  'eof)

(define (consume-open-paren stream)
  (stream 'advance)
  'open-paren)

(define (consume-close-paren stream)
  (stream 'advance)
  'close-paren)

(define (consume-quote stream)
  (stream 'advance)
  'quote-symbol)

(define (consume-identifier stream)
  (define (loop acc)
    (cond ((char-identifier? (stream 'next))
           (let ((c (make-string 1 (stream 'next))))
             (stream 'advance)
             (loop (string-append acc c))))
          (else acc)))
   (loop ""))

(define (lexeme-keyword lexeme)
  (cond
   ((string=? lexeme "define") 'define)
   ((string=? lexeme "let") 'let)
   ((string=? lexeme "lambda") 'lambda)
   ((string=? lexeme "set!") 'set!)
   ((string=? lexeme "do") 'do)
   ((string=? lexeme "cond") 'cond)
   ((string=? lexeme "if") 'if)
   ((string=? lexeme "quote") 'quote)
   (else #f)))


(define (lexeme-numeric lexeme)
  (let ((n (string->number lexeme)))
    (if n (cons 'number n) #f)))


(define (lex str)
  (let ((stream (make-stream str)))
    (define (loop)
      (let ((token (tokenize stream)))
        (cond
         ((eq? token 'eof) '())
         (else (cons token (loop))))))
    (loop)))
