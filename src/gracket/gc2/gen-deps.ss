
#lang scheme/base

(for-each (lambda (f)
            (when (regexp-match #rx#"[.]d$" (path->bytes f))
              (let ([dir (ormap (lambda (d)
                                  (and (file-exists? (build-path d (path-replace-suffix f #".cc")))
                                       d))
                                (list "xsrc" "macxsrc"))])
                (when dir
                  (with-input-from-file f
                    (lambda ()
                      (with-output-to-file (path-replace-suffix f #".dd")
                        #:exists 'truncate/replace
                        (lambda ()
                          (let ([l (read-line)])
                            (unless (eof-object? l)
                              (let ([m (regexp-match #rx"(.*)[.]o(.*)" l)])
                                (printf "~a/~a.cc~a\n"
                                        dir
                                        (cadr m)
                                        (caddr m)))
                              (let loop ()
                                (let ([l (read-line)])
                                  (unless (eof-object? l)
                                    (printf "~a\n" l)
                                    (loop))))))))))))))
          (directory-list))

(when (file-exists? "macprecomp.d")
  (with-input-from-file "macprecomp.d"
    (lambda ()
      (with-output-to-file "macprecomp.dd"
        #:exists 'truncate/replace
        (lambda ()
          (when (regexp-match #rx"^macprecomp[.]o" 
                              (current-input-port)
                              0 #f
                              (current-output-port))
            (printf "macxsrc/xform_precomp.h"))
          (let loop ()
            (let ([c (read-char)])
              (unless (eof-object? c)
                (write-char c)
                (loop)))))))))
