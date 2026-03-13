#lang racket/base
(require racket/file
         racket/runtime-path
         racket/place
         dynext/compile
         dynext/link
         racket/runtime-path)

(provide build-ffi2-lib
         test-async?)

(define-runtime-path ffi2.c "ffi2.c")

(define test-async?
  (and (place-enabled?) (not (eq? 'windows (system-type)))))

(define (build-ffi2-lib)
  (define test-tmp-dir
    (make-temporary-file "foreign~a" 'directory))

  (copy-file ffi2.c
             (build-path test-tmp-dir "ffi2.c"))

  (parameterize ([current-directory test-tmp-dir])
    (define c (build-path (current-directory) "ffi2.c"))
    (define o (build-path (current-directory)
                          (if (eq? 'windows (system-type))
                              "ffi2.obj" "ffi2.o")))
    (define so (build-path (current-directory)
                           (bytes->path (bytes-append #"foreign-test"
                                                      (system-type 'so-suffix)))))
    (when (file-exists? o) (delete-file o))
    (when (file-exists? so) (delete-file so))

    (parameterize ([current-standard-link-libraries '()]
                   [current-extension-compiler-flags
                    (if test-async?
                        (append '("-pthread" "-DUSE_THREAD_TEST") (current-extension-compiler-flags))
                        (current-extension-compiler-flags))]
                   [current-extension-linker-flags
                    (if test-async?
                        (append '("-pthread") (current-extension-linker-flags))
                        (current-extension-linker-flags))])
      (compile-extension #t c o '())
      (link-extension #t (list o) so))

    (values
     so
     (lambda ()
       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          (eprintf "warning: could not delete ~e\n" test-tmp-dir))])
         (delete-directory/files test-tmp-dir))))))
