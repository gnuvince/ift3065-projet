;; File: "runtime.scm"

(define number?
  (lambda (x)
    (%number? x)))

(define +
  (lambda (x y)
    (%+ x y)))

(define -
  (lambda (x y)
    (%- x y)))

(define *
  (lambda (x y)
    (%* x y)))


(define quotient
  (lambda (x y)
    (%quotient x y)))

(define remainder
  (lambda (x y)
    (%remainder x y)))

(define =
  (lambda (x y)
    (%= x y)))

(define <
  (lambda (x y)
    (%< x y)))

(define <=
  (lambda (x y)
    (%<= x y)))

(define >
  (lambda (x y)
    (%> x y)))

(define >=
  (lambda (x y)
    (%>= x y)))

(define pair?
  (lambda (x)
    (%pair? x)))

(define cons
  (lambda (x y)
    (%cons x y)))

(define car
  (lambda (x)
    (%car x)))

(define cdr
  (lambda (x)
    (%cdr x)))

(define set-car!
  (lambda (x y)
    (%set-car! x y)))

(define set-cdr!
  (lambda (x y)
    (%set-cdr! x y)))

(define null?
  (lambda (x)
    (%null? x)))

(define eq?
  (lambda (x y)
    (%eq? x y)))

(define not
  (lambda (x)
    (if x #f #t)))



(define list
  (lambda lst lst))

(define length
  (lambda (lst)
    (if (%pair? lst)
        (+ 1 (length (cdr lst)))
        0)))

(define list?
  (lambda (lst)
    (if (%null? lst)
        #t
        (and (%pair? lst) (list? (cdr lst))))))

(define append
  (lambda (lst1 lst2)
    (if (%pair? lst1)
        (%cons (%car lst1) (append (%cdr lst1) lst2))
        lst2)))

(define reverse
  (lambda (lst)
    (reverse-aux lst (list))))

(define reverse-aux
  (lambda (lst rev)
    (if (%pair? lst)
        (reverse-aux (%cdr lst) (%cons (%car lst) rev))
        rev)))

(define list-ref
  (lambda (lst i)
    (if (%= i 0)
        (%car lst)
        (list-ref (%cdr lst) (%- i 1)))))

(define list-set!
  (lambda (lst i x)
    (if (%= i 0)
        (%set-car! lst x)
        (list-set! (%cdr lst) (%- i 1) x))))

(define max
  (lambda (x y)
    (if (%> x y) x y)))

(define min
  (lambda (x y)
    (if (%< x y) x y)))

(define abs
  (lambda (x)
    (if (%< x 0) (%- 0 x) x)))

(define modulo
  (lambda (x y)
    (%remainder x y)))

(define display
  (lambda (str)
    (%display str)))


(define make-string
  (lambda (len)
    (%make-string len)))

(define string
  (lambda chars
    (let ((s (%make-string (length chars))))
      (let loop ((i 0) (chars chars))
        (if (null? chars)
            s
            (begin
              (%string-set! s i (car chars))
              (loop (+ i 1) (cdr chars))))))))

(define string-ref
  (lambda (s i)
    (%string-ref s i)))

(define string-set!
  (lambda (s i c)
    (%string-set! s i c)))

(define string->list
  (lambda (str)
    (%string->list str)))

;; (define list->string
;;   (lambda (chars)
;;     (%list->string chars)))

(define string-length
  (lambda (str)
    (%string-length str)))

;; (define string-append
;;   (lambda (str1 str2)
;;     (%list->string (append (%string->list str1) (%string->list str2)))))

;; (define substring
;;   (lambda (str start end)
;;     (%list->string
;;      (%substring-aux2
;;       (%substring-aux1 (%string->list str) start)
;;       (%- end start)))))

;; (define %substring-aux1
;;   (lambda (lst n)
;;     (if (%> n 1)
;;         (%substring-aux1 (%cdr lst) (%- n 1))
;;         lst)))

;; (define %substring-aux2
;;   (lambda (lst n)
;;     (if (%> n 1)
;;         (%cons (%car lst) (%substring-aux2 (%cdr lst) (%- n 1)))
;;         '())))

(define map
  (lambda (f lst)
    (if (%pair? lst)
        (%cons (f (%car lst))
               (map f (%cdr lst)))
        (list))))

(define for-each
  (lambda (f lst)
    (if (%pair? lst)
        (begin
          (f (%car lst))
          (for-each f (%cdr lst)))
        #f)))

;; (define write
;;   (lambda (x)
;;     (if (%string? x)
;;         (begin
;;           (%write-char #\")
;;           (display x)
;;           (%write-char #\"))
;;         (if (%number? x)
;;             (display (number->string x))
;;             (if (%pair? x)
;;                 (begin
;;                   (%write-char #\()
;;                   (write (%car x))
;;                   (%write-list (%cdr x)))
;;                 (if (%symbol? x)
;;                     (display "#<symbol>")
;;                     (display "#<object>")))))))

;; (define %write-list
;;   (lambda (lst)
;;     (if (%null? lst)
;;         (%write-char #\))
;;         (if (%pair? lst)
;;             (begin
;;               (%write-char #\space)
;;               (write (%car lst))
;;               (%write-list (%cdr lst)))
;;             (begin
;;               (display " . ")
;;               (write lst)
;;               (%write-char #\)))))))

;; (define number->string
;;   (lambda (n)
;;     (%list->string
;;      (if (%< n 0)
;;          (%cons #\- (%number->string-aux (%- 0 n) '()))
;;          (%number->string-aux n '())))))

;; (define %number->string-aux
;;   (lambda (n lst)
;;     (let ((rest (%cons (%+ #\0 (%remainder n 10)) lst)))
;;       (if (%< n 10)
;;           rest
;;           (%number->string-aux (%quotient n 10) rest)))))

;; (define pp
;;   (lambda (x)
;;     (begin
;;       (write x)
;;       (%write-char #\newline))))


(define sym-table (list))

(define string->symbol
  (lambda (str)
    (let ((x (let loop ((t sym-table))
               (if (%null? t)
                   #f
                   (if (%string=? (%car (%car t)) str)
                       (cdr (car t))
                       (loop (cdr t)))))))
      (if x
          x
          (let ((sym (%string->symbol str)))
            (set! sym-table (cons (cons str sym) sym-table))
            sym)))))

(define symbol?
  (lambda (sym)
    (%symbol? sym)))
