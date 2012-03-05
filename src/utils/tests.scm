(define (run-tests tests)
  (let loop ((tests tests)
             (success #t))
    (if (null? tests)
        success
        (let ((result ((car tests))))
          (print (car tests) ": " (if result "OK" "FAIL") "\n")
          (loop (cdr tests)
                (and success result))))))
