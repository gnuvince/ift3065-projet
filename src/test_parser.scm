(define (test-stream-peek)
  (let ((s1 (make-token-stream ""))
        (s2 (make-token-stream "x")))
    (and
     (not (stream-peek s1))
     (eq? (stream-peek s2) (stream-peek s2)) ; doesn't alter the stream.
     )))

(define (test-stream-empty?)
  (let ((s1 (make-token-stream ""))
        (s2 (make-token-stream "x")))
    (and
     (stream-empty? s1)
     (not (stream-empty? s2))
     )))

(define (test-stream-consume)
  (let* ((s1 (make-token-stream ""))
         (s2 (make-token-stream "x"))
         (t (stream-peek s2)))
    (and
     (not (stream-consume s1))
     (eq? t (stream-consume s2))
     (stream-empty? s2))))


(define (test-stream-consume-type)
  (let ((s1 (make-token-stream "2")))
    (and
     (not (stream-consume-type s1 'ident))
     (eq? (token-type (stream-consume-type s1 'number)) 'number)
     (stream-empty? s1))))


(define (test-stream-consume-value)
  (let ((s1 (make-token-stream "2")))
    (and
     (not (stream-consume-value s1 3))
     (eq? (token-value (stream-consume-value s1 2)) 2)
     (stream-empty? s1))))

(define (test-stream-consume-type-value)
  (let ((s1 (make-token-stream "2")))
    (and
     (not (stream-consume-type-value s1 'identifier 3))
     (not (stream-consume-type-value s1 'number 3))
     (eq? (token-value (stream-consume-type-value s1 'number 2)) 2)
     (stream-empty? s1))))





(define (run-tests)
  (for-each (lambda (t)
              (display t)
              (display ": ")
              (display (if (t) "OK" "FAIL"))
              (newline))
            (list test-stream-peek
                  test-stream-consume
                  test-stream-empty?
                  test-stream-consume-type
                  test-stream-consume-value
                  test-stream-consume-type-value
                  )))
