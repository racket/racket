(module gen-system '#%kernel
  
  ;; Command-line argument: <dest-file> <target-machine> <cross-target-machine>
  
  (define-values (ht)
    (hash 'os (system-type 'os)
          'word (system-type 'word)
          'gc 'cs
          'vm 'chez-scheme
          'link 'static
          'machine (bytes->string/utf-8 (path->bytes (system-library-subpath #f)))
          'so-suffix (system-type 'so-suffix)
          'so-mode 'local
          'fs-change '#(#f #f #f #f)
          'target-machine (if (equal? "any" (vector-ref (current-command-line-arguments) 2))
                              #f
                              (string->symbol (vector-ref (current-command-line-arguments) 1)))))

  (call-with-output-file
   (vector-ref (current-command-line-arguments) 0)
   (lambda (o)
     (write ht o)
     (newline o))
   'truncate/replace))
