(define (run-tests tests)
  (let loop ((tests tests))
    (if (null? tests)
        #t
        (let ((result ((car tests))))
          (print (car tests) ": " (if result "OK" "FAIL") "\n")
          (and result (loop (cdr tests)))))))
