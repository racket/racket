
(define (linklet-virtual-machine-bytes)
  ;; #"chez-scheme"
  #vu8(99 104 101 122 45 115 99 104 101 109 101))

(define (write-linklet-bundle-hash ht dest-o)
  (let-values ([(o get) (open-bytevector-output-port)])
    (fasl-write* (encode-linklet-paths ht) o)
    (let ([bstr (get)])
      (write-bytes (integer->integer-bytes (bytes-length bstr) 4 #f #f) dest-o)
      (write-bytes bstr dest-o))))

(define (encode-linklet-paths orig-ht)
  (let ([path->compiled-path (make-path->compiled-path 'write-linklet)])
    (let loop ([i (hash-iterate-first orig-ht)] [ht orig-ht])
      (cond
       [(not i) ht]
       [else
        (let-values ([(key v) (hash-iterate-key+value orig-ht i)])
          (let ([new-ht (if (and (linklet? v)
                                 (pair? (linklet-paths v)))
                            (hash-set ht key
                                      (set-linklet-paths
                                       v
                                       (map path->compiled-path
                                            (linklet-paths v))))
                            ht)])
            (loop (hash-iterate-next orig-ht i)
                  new-ht)))]))))
