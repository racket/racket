(module make-plt mzscheme
  (require (lib "file.ss")
           (lib "pack.ss" "setup")
           (lib "plthome.ss" "setup"))

  (current-directory plthome)

  (define (display-res f)
    (lambda (x)
      (let ([ans (f x)])
        (when ans
          (printf "including ~s~n" x))
        ans)))

  (define bad-dirs '("CVS" ".svn"))
  (define bad-files '(".DS_Store" "reduction-semantics.plt"))

  (pack (build-path "collects" "reduction-semantics" "reduction-semantics.plt")
        "Reduction Semantics"
        (list (build-path "collects" "reduction-semantics"))
        '(("reduction-semantics"))
        (display-res
         (lambda (filename)
           (let ([exp (reverse (explode-path (normalize-path filename)))])
             (cond
               [(member (cadr exp) bad-dirs)
                #f]
               [(member (car exp) bad-dirs)
                #f]
               [(member (car exp) bad-files)
                #f]
               [(regexp-match #rx"~$" (path->string filename))
                #f]
               [else
                (std-filter filename)]))))
        #t
        'file-replace))
