(module pre-base '#%kernel
  (#%require "qq-and-or.rkt"
             "path.rkt"
             "kw.rkt")

  (#%provide new:collection-path
             new:collection-file-path)

  (define-values (new:collection-path)
    (let ([collection-path (new-lambda (collection 
                                        #:fail [fail (lambda (s)
                                                       (raise
                                                        (exn:fail:filesystem
                                                         (string-append "collection-path: " s)
                                                         (current-continuation-marks))))]
                                        . collections)
                             (collection-path fail collection collections))])
      collection-path))

  (define-values (new:collection-file-path)
    (let ([collection-file-path (new-lambda (file-name 
                                             collection
                                             #:check-compiled? [check-compiled?
                                                                (and (path-string? file-name)
                                                                     (regexp-match? #rx".[.]rkt$" file-name))]
                                             #:fail [fail (lambda (s)
                                                            (raise
                                                             (exn:fail:filesystem
                                                              (string-append "collection-file-path: " s)
                                                              (current-continuation-marks))))]
                                             . collections)
                                  (collection-file-path fail check-compiled? file-name collection collections))])
      collection-file-path)))
