(let loop ((x 0))
  (if (not (= x 5))
      (loop (+ x 1))
      x))
