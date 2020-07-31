(module help-startup '#%kernel
  (#%provide get-linklet
             get-version-comparisons)
  
  (define-values (get-lines)
    (lambda (in)
      (let-values ([(l) (read-line in 'any)])
        (if (eof-object? l)
            null
            (cons l (get-lines in))))))

  (define-values (get-linklet)
    (lambda (src)
      (read
       (open-input-string
        (apply
         string-append
         (map (lambda (l)
                (regexp-replace* #rx"\\\\(.)"
                                 (substring l 1 (sub1 (string-length l)))
                                 "\\1"))
              (reverse (cdr (reverse (cddr (call-with-input-file src get-lines)))))))))))
    
  (define-values (get-version-comparisons)
    (lambda (vers)
      (call-with-input-file 
       vers
       (lambda (in)
         (letrec-values ([(get-version-comparisons)
                          (lambda ()
                            (let-values ([(line) (read-line in 'any)])
                              (if (eof-object? line)
                                  ""
                                  (let-values ([(m) (regexp-match #rx"^#define (MZSCHEME_VERSION_[A-Z]) ([0-9]+)"
                                                                  line)])
                                    (if m
                                        (string-append " || (" (cadr m) " != " (caddr m) ")"
                                                       (get-version-comparisons))
                                        (get-version-comparisons))))))])
           (get-version-comparisons)))))))
