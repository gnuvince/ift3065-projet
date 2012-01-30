(load "ast.scm")


(define (test-ast-get-attr)
  (let ((ast (make-ast '((a . 1) (b . 2)) '())))
    (and (equal? (ast-get-attr ast 'a) '(a . 1))
         (equal? (ast-get-attr ast 'b) '(b . 2))
         (equal? (ast-get-attr ast 'c) #f))))

(define (test-ast-get-attr-value)
  (let ((ast (make-ast '((a . 1) (b . 2)) '())))
    (and (equal? (ast-get-attr-value ast 'a) 1)
         (equal? (ast-get-attr-value ast 'b) 2)
         (equal? (ast-get-attr-value ast 'c) #f))))

(define (test-ast-get-attrs)
  (let ((ast-1 (make-ast '((a . 1)) '()))
        (ast-2 (make-ast '() '())))
    (and (equal? (ast-get-attrs ast-1) '((a . 1)))
         (null? (ast-get-attrs ast-2)))))


(define (test-ast-get-children)
  (let ((ast-1 (make-ast '() '()))
        (ast-2 (make-ast '() '(a b))))
    (and (null? (ast-get-children ast-1))
         (equal? (ast-get-children ast-2) '(a b))
         )))


(define (test-ast-node?)
  (and (ast-node? (make-ast '() '()))
       (not (ast-node? '()))
       (not (ast-node? '(() ())))
       ))

(define (test-ast-add-child)
  (let ((ast-1 (make-ast '() '()))
        (ast-2 (make-ast '() '(a b))))
    (and (equal? (ast-get-children (ast-add-child ast-1 'c)) '(c))
         (equal? (ast-get-children (ast-add-child ast-2 'c)) '(a b c))
         )))

(define (test-ast-put-attr)
  (let ((ast-1 (make-ast '() '()))
        (ast-2 (make-ast '((a . 1)) '())))
    (and (equal? (ast-get-attr (ast-put-attr ast-1 'a 3) 'a) '(a . 3))
         (equal? (ast-get-attr (ast-put-attr ast-2 'a 3) 'a) '(a . 3))
         )))


(define (run-tests)
  (for-each (lambda (t)
              (display t)
              (display ": ")
              (display (if (t) "OK" "FAIL"))
              (newline))
            (list test-ast-get-attr
                  test-ast-get-attr-value
                  test-ast-get-attrs
                  test-ast-node?
                  test-ast-get-children
                  test-ast-add-child
                  test-ast-put-attr
                  )))
