(module labels mzscheme
  (require (lib "md5.ss")
           (lib "etc.ss"))
  (provide make-labeling
           delete-tag-list!)
  
  ;; REQUIREMENT: The label code must be non-numeric.
  ;; REQUIREMENT: The first numeric character following the label code
  ;;              indicates the start of the unique suffix identifying
  ;;              the closure struct type.
  
  ;; add1/string: string -> string
  ;; a successor function on strings
  (define (add1/string str)
    (cond
      [(regexp-match "$^" str)
       => (lambda (x) "b")]
      [(regexp-match "z(.*)" str)
       => (lambda (m) (string-append "A" (cadr m)))]
      [(regexp-match "Z(.*)" str)
       => (lambda (m) (string-append "a" (add1/string (cadr m))))]
      [else
       (format "~a~a"
               (integer->char (add1 (char->integer (string-ref str 0))))
               (substring str 1))]))
    
  (define tag-file-path (this-expression-source-directory))
  (define default-file-name ".tag-list")
  (define file-system-mutex (make-semaphore 1))
  
  ;; read-tag-list: string -> (listof (list bytes string))
  ;; read the tag list from the file system
  (define (read-tag-list filename)
    (if (file-exists? (build-path tag-file-path filename))
        (call-with-input-file (build-path tag-file-path filename)
          read)
        '()))
  
  ;; save-tag-list!: (listof (list bytes string)) string -> (listof (list bytes string))
  ;; save the tag list in the file system
  (define (save-tag-list! new-list filename)
    (call-with-output-file (build-path tag-file-path filename)
      (lambda (o-port)
        (write new-list o-port))
      'replace))
  
  ;; delete-tag-list!: [filename] -> void
  ;; delete a file containing a tag-list
  (define delete-tag-list!
    (case-lambda
      [(filename)
       (when (file-exists? (build-path tag-file-path filename))
         (delete-file (build-path tag-file-path filename)))]
      [() (delete-tag-list! default-file-name)]))
  
  ;; lookup-tag: bytes string -> string
  ;; lookup a tag in the-tag-table
  (define (lookup-tag pgm filename)
    (let* ([the-tag-list (read-tag-list filename)]
           [hash-code (md5 pgm)]
           [new-tag
            (if (null? the-tag-list) "a"
                (add1/string (cadar the-tag-list)))])
      (let loop ([l the-tag-list])
        (cond
          [(null? l)
           (save-tag-list!
            (cons (list hash-code new-tag)
                  the-tag-list)
            filename)
           new-tag]
          [(bytes=? hash-code (caar l))
           (cadar l)]
          [else (loop (cdr l))]))))  
  
  ;; make-labeling: bytes -> (-> symbol)
  ;; produce the labeling function for a particular program
  (define make-labeling
    (case-lambda
      [(pgm) (make-labeling pgm default-file-name)]
      [(pgm filename)
       (dynamic-wind
        (lambda () (semaphore-wait file-system-mutex))
        (lambda ()
          (let ([tag (lookup-tag pgm filename)]
                [count 0])
            (lambda ()
              (begin0
                (string->symbol (format "~a~a" tag count))
                (set! count (add1 count))))))
        (lambda () (semaphore-post file-system-mutex)))]))
  )