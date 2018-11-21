
(define (linklet-virtual-machine-bytes)
  ;; #"chez-scheme"
  #vu8(99 104 101 122 45 115 99 104 101 109 101))

(define (write-linklet-bundle-hash ht dest-o)
  (let-values ([(o get) (open-bytevector-output-port)])
    (fasl-write* ht o)
    (let ([bstr (get)])
      (write-bytes (integer->integer-bytes (bytes-length bstr) 4 #f #f) dest-o)
      (write-bytes bstr dest-o))))
