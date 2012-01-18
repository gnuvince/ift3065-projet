;; make-stream : string -> stream
;;
;; Takes a string and returns a "stream" on that string.  The stream
;; supports two operations:
;; - next: returns the character under the cursor, without moving the cursor.
;;         Reading past the end of the stream returns the nul character.
;; - advance: moves to the cursor to the next character.
(define (make-stream str)
  (let ((current 0)
        (size (string-length str)))
    (lambda (msg)
      (case msg
        ((next) (if (>= current size)
                    #\nul
                    (string-ref str current)))
        ((advance) (set! current (+ current 1)))))))


;; Definition of lexical elements used in Scheme.  The extended
;; characters are taken from R5RS:
;; http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-5.html#%_sec_2.1
(define digit (string->list "0123456789"))
(define alpha (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define extended (string->list "!$%&*+-./:<=>?@^_~"))

;; char-identifier? : char -> bool
;;
;; Is `c` a character used in an identifier?
(define (char-identifier? c)
  (or (member c digit)
      (member c alpha)
      (member c extended)))



;; consume-eof : stream -> token
(define (consume-eof stream)
  (stream 'advance)
  'eof)

;; consume-open-paren : stream -> token
(define (consume-open-paren stream)
  (stream 'advance)
  'open-paren)

;; consume-close-paren : stream -> token
(define (consume-close-paren stream)
  (stream 'advance)
  'close-paren)

;; consume-quote : stream -> token
;; TODO: Should we return quote-symbol or quote like the keyword?
(define (consume-quote stream)
  (stream 'advance)
  'quote-symbol)

;; consume-identifier : stream -> token
;;
;; Read characters, concatenating them into a string, until a
;; non-identifier character is read.
(define (consume-identifier stream)
  (define (loop)
    (cond ((char-identifier? (stream 'next))
           (let ((c (stream 'next)))
             (stream 'advance)
             (cons c (loop))))
          (else '())))
   (list->string (loop)))


;; consume-whitespace : stream -> ()
;;
;; skip whitespace
(define (consume-whitespace stream)
  (if (char-whitespace? (stream 'next))
      (begin
        (stream 'advance)
        (consume-whitespace stream))))



;; lexeme-keyword : string -> token|bool
(define (lexeme-keyword lexeme)
  (cond
   ((string=? lexeme "define") 'define)
   ((string=? lexeme "let")    'let)
   ((string=? lexeme "lambda") 'lambda)
   ((string=? lexeme "set!")   'set!)
   ((string=? lexeme "do")     'do)
   ((string=? lexeme "cond")   'cond)
   ((string=? lexeme "if")     'if)
   ((string=? lexeme "quote")  'quote)
   (else #f)))


;; lexeme-numeric : string -> token|bool
(define (lexeme-numeric lexeme)
  (let ((n (string->number lexeme)))
    (if n
        (cons 'number n)
        #f)))



;; consume-string : stream -> token
(define (consume-string stream)
  (define (loop escaped?)
    (cond
     ((and (not escaped?) (char=? (stream 'next) #\"))
      (begin
        (stream 'advance) ; consume closing double-quote
        '()))

     ((and (not escaped?) (char=? (stream 'next) #\\))
      (begin
        (stream 'advance)
        (loop #t)))

     ((not escaped?)
      (let ((c (stream 'next)))
        (stream 'advance)
        (cons c (loop #f))))

     (escaped?
      (let ((c (stream 'next)))
        (stream 'advance)
        (cons (cond
               ((char=? c #\") #\")
               ((char=? c #\n) #\newline)
               ((char=? c #\t) #\tab)
               ((char=? c #\\) #\\))
              (loop #f))))))
  (stream 'advance) ; consume opening double-quote.
  (cons 'string (list->string (loop #f))))





(define (consume-character stream)
  (stream 'advance)
  )


;; tokenize : stream -> token
;;
;; Consume characters from the stream and return the next token.
;; Invalid characters raise an error.
(define (tokenize stream)
  (let ((token
         (cond
           ((char=? (stream 'next) #\nul) (consume-eof stream))

           ((char=? (stream 'next) #\()   (consume-open-paren stream))

           ((char=? (stream 'next) #\))   (consume-close-paren stream))

           ((char=? (stream 'next) #\')   (consume-quote stream))

           ((char=? (stream 'next) #\")   (consume-string stream))

           ((char=? (stream 'next) #\#)   (consume-character stream))

           ;; We'll skip all white space, then call `tokenize`
           ;; recursively to return the next token.
           ((char-whitespace? (stream 'next))
            (begin
              (consume-whitespace stream)
              (tokenize stream)))

           ;; Keywords, numbers and identifiers are all read the same way:
           ;; 1. Characters are read until we no longer have a char-identifier;
           ;; 2. If the characters read represent a keyword, return a keyword token;
           ;; 3. If the characters read represent a number, return a number token;
           ;; 4. Otherwise, return an identifier token.
           ((char-identifier? (stream 'next))
            (let ((ident (consume-identifier stream)))
               (or (lexeme-keyword ident)
                   (lexeme-numeric ident)
                   (cons 'ident ident))))

           (else (error
                  (string-append "unknown character: "
                                 (make-string 1 (stream 'next))))))))
    token))


;; lex : string -> [token]
;;
;; Call `tokenize` until EOF, accumulating tokens into a list.
(define (lex str)
  (let ((stream (make-stream str)))
    (define (loop)
      (let ((token (tokenize stream)))
        (cond
         ((eq? token 'eof) '())
         (else (cons token (loop))))))
    (loop)))
