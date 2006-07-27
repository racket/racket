(module hash2 mzscheme
  (define (main argv)
    (let* ([n (string->number (vector-ref argv 0))]
	   [hash1 (make-hash-table 'equal)]
	   [hash2 (make-hash-table 'equal)]
	   [zero (lambda () 0)])
      (let loop ([i 0])
	(unless (= i 10000)
	  (hash-table-put! hash1 (string-append "foo_" (number->string i)) i)
	  (loop (add1 i))))
      (let loop ([i 0])
	(unless (= i n)
	  (hash-table-for-each hash1 (lambda (key value)
				       (hash-table-put!
					hash2
					key
					(+ (hash-table-get hash2 key zero) value))))
	  (loop (add1 i))))
      (printf "~s ~s ~s ~s~n"
	      (hash-table-get hash1 "foo_1")
	      (hash-table-get hash1 "foo_9999")
	      (hash-table-get hash2 "foo_1")
	      (hash-table-get hash2 "foo_9999"))))

  (main (current-command-line-arguments)))
