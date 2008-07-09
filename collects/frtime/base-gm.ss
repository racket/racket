(module base-gm mzscheme
  (require mzlib/list
	   mzlib/etc)
  
  (provide assert
           print-each
           make-hash
	   hash-get
	   hash-put!
	   hash-remove!
	   hash-map
	   hash-for-each
	   hash-mem?
           hash-fold
           hash-keys
           hash-add-all!)

  (define-struct (exn:assert exn) ())

  (define-syntax (assert stx)
    (syntax-case stx ()
      [(src-assert bool) #'(src-assert bool "")]
      [(src-assert bool msg ...)
       (with-syntax ([src-text (datum->syntax-object
                                (syntax src-assert)
                                (format "~a:~a:~a: assertion failed: "
                                        (syntax-source (syntax bool))
                                        (syntax-line (syntax bool))
                                        (syntax-column (syntax bool))))])
         #'(unless bool
             (raise (make-exn:assert (format-each src-text msg ...)))))]))
  
  (define (format-each . args)
    (apply string-append (map (lambda (s) (format "~a " s)) args)))
  
  (define (print-each . args)
    (printf "~a~n" (apply format-each args)))

  
  (define make-hash make-hash-table)
  (define hash-get hash-table-get)
  (define hash-put! hash-table-put!)
  (define hash-remove! hash-table-remove!)
  (define hash-map hash-table-map)
  (define hash-for-each hash-table-for-each)
  (define (hash-mem? hash item) (hash-get hash item (lambda () false)))
  (define (hash-fold hash init fn)
    (hash-for-each hash (lambda (key val) (set! init (fn key val init)))) init)
  (define (hash-keys hash)
    (hash-fold hash empty (lambda (key val acc) (cons key acc))))
  (define (hash-add-all! to-hash from-hash) ;; // memcpy-style argument order
    (hash-for-each from-hash
                   (lambda (key val) (hash-put! to-hash key val))))

  )
