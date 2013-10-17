
;; Imperative body:
(define (loops n)
  (let ((result 0))
    (let loop1 ((i1 1)) 
      (if (> i1 n)
          'done
          (begin
            (let loop2 ((i2 1)) 
              (if (> i2 n)
                  'done
                  (begin
                    (let loop3 ((i3 1)) 
                      (if (> i3 n)
                          'done
                          (begin
                            (let loop4 ((i4 1)) 
                              (if (> i4 n)
                                  'done
                                  (begin
                                    (let loop5 ((i5 1)) 
                                      (if (> i5 n)
                                          'done
                                          (begin
                                            (let loop6 ((i6 1)) 
                                              (if (> i6 n)
                                                  'done
                                                  (begin
                                                    (set! result (+ result 1))
                                                    (loop6 (+ i6 1)))))
                                            (loop5 (+ i5 1)))))
                                    (loop4 (+ i4 1)))))
                            (loop3 (+ i3 1)))))
                    (loop2 (+ i2 1)))))
            (loop1 (+ i1 1)))))
    result))

;; Functional body:
(define (func-loops n)
  (let loop1 ((i1 1)(result 0))
      (if (> i1 n)
          result
          (let loop2 ((i2 1)(result result))
            (if (> i2 n)
                (loop1 (+ i1 1) result)
                (let loop3 ((i3 1)(result result))
                  (if (> i3 n)
                      (loop2 (+ i2 1) result)
                      (let loop4 ((i4 1)(result result))
                        (if (> i4 n)
                            (loop3 (+ i3 1) result)
                            (let loop5 ((i5 1)(result result))
                              (if (> i5 n)
                                  (loop4 (+ i4 1) result)
                                  (let loop6 ((i6 1)(result result))
                                    (if (> i6 n)
                                        (loop5 (+ i5 1) result)
                                        (loop6 (+ i6 1) (+ result 1)))))))))))))))

(let ((cnt (if (with-input-from-file "input.txt" read) 18 1)))
  (time (let loop ((n 20) (v 0))
          (if (zero? n)
              v
              (loop (- n 1)
                    (list
                     (loops cnt)
                     (func-loops cnt)))))))
