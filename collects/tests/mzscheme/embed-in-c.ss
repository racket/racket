;; Works for Linux, Mac OS X.
;; Assumes 3m

(load-relative "loadtest.ss")

(Section 'embed-in-c)

(require scheme/system
         setup/dirs)

(define dir (collection-path "tests" "mzscheme"))
(define lib-dir (find-lib-dir))

(parameterize ([current-directory dir])
  (test #t system (format "cc -c -o embed-in-c.o -DMZ_COLLECTION_PATH='\"~a\"' -I\"~a\" -DMZ_PRECISE_GC embed-in-c.c"
                          (find-collects-dir)
                          (find-include-dir)))
  
  (test #t system (format "cc -o embed-in-c embed-in-c.o -lm -ldl -pthread ~a"
                          (case (system-type 'link)
                            [(framework)
                             (format "-F\"~a\" -framework PLT_MzScheme" lib-dir)]
                            [(static shared)
                             (format "-L\"~a\" -lmzscheme3m" lib-dir)]
                            [else
                             (error "unsupported")])))
  
  (let ([o (open-output-bytes)]
        [e (open-output-bytes)])
    (test #t
          (lambda (s)
            (parameterize ([current-input-port (open-input-bytes #"5\n(log-error \"ouch!\")")]
                           [current-output-port o]
                           [current-error-port e])
              (system s)))
          (format "~a./embed-in-c 1 2 3"
                  (case (system-type 'link)
                    [(framework)
                     (format "env DYLD_FRAMEWORK_PATH=\"~a\" " lib-dir)]
                    [else ""])))
    (test #"ouch!\n" get-output-bytes e)
    (test #"1\n2\n3\n> 5\n> > " get-output-bytes o))

  (let ([maybe-delete-file 
         (lambda (f) (when (file-exists? f) (delete-file f)))])
    (maybe-delete-file "embed-in-c.o")
    (maybe-delete-file "embed-in-c")))
