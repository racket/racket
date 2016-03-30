#lang racket/base
(require racket/function
         racket/path
         racket/file)
(provide compile-file)

(define compile-file
  (case-lambda
    [(src)
     (define cdir (build-path (path-only src) "compiled"))
     (make-directory* cdir)
     (compile-file src (build-path cdir (path-add-suffix (file-name-from-path src) #".zo")))]
    [(src dest) 
     (compile-file src dest values)]
    [(src dest filter)
     (define in (open-input-file src))
     (dynamic-wind
      void
      (lambda ()
        (define ok? #f)
        ; This must be based on the path to dest. Renaming typically cannot be done
        ; atomically across file systems, so the temporary directory is not an option
        ; because it is often a ram disk. src (or dir below) couldn't be used because
        ; it may be on a different filesystem. Since dest must be a file path, this
        ; guarantees that the temp file is in the same directory. It would take a weird
        ; filesystem configuration to break that.
        (define temp-filename (make-temporary-file "tmp~a" #f (path-only dest)))
        (port-count-lines! in)
        (dynamic-wind
         void
         (lambda ()
           ; XXX: This seems like it should be a library function named 'relative-path-only'
           (define dir
             (let-values ([(base name dir?) (split-path src)])
               (if (eq? base 'relative)
                   (current-directory)
                   (path->complete-path base (current-directory)))))
           (define out (open-output-file temp-filename #:exists 'truncate/replace))
           (parameterize ([current-load-relative-directory dir]
                          [current-write-relative-directory dir])
             ; Rather than installing a continuation barrier, we detect reinvocation.
             ; The only thing that can cause reinvocation is if the filter captures the
             ; continuation and communicates it externally.
             (define count 0)
             (dynamic-wind
              (lambda ()
                (if (zero? count)
                    (set! count 1)
                    (error 'compile-file "filter function should not be re-entrant")))
              (lambda ()
                (for ([r (in-port (curry read-syntax src) in)])
                  (write (compile-syntax (filter (namespace-syntax-introduce r))) out))
                (set! ok? #t))
              (lambda () 
                (close-output-port out)))))
         (lambda ()
           (if ok?
               (rename-file-or-directory temp-filename dest #t)
               (with-handlers ([exn:fail:filesystem? void])
                 (delete-file temp-filename))))))
      (lambda () (close-input-port in)))
     (build-path dest)]))

