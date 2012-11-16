#lang racket

;; Works for Linux.
;; Almost works for Mac OS X.
;; Assumes 3m.

(require racket/system
         setup/dirs)

(define-syntax-rule (test expected expr)
  (let ([val expr])
    (unless (equal? expected val)
      (error 'test "failed at ~s: ~e; expected: ~e" 'expr val expected))))

(define dir (collection-path "tests" "racket"))
(define lib-dir (find-lib-dir))

(define (buildinfo def)
  (or (and (file-exists? (build-path lib-dir "buildinfo"))
	   (let ([m (call-with-input-file*
		     (build-path lib-dir "buildinfo")
		     (lambda (in)
		       (regexp-match (regexp (format "(?m:^~a=(.*)$)" def)) in)))])
	     (and m (cadr m))))
      ""))

(define (go use-declare?)
  (parameterize ([current-directory dir])
    (when use-declare?
      (system* (build-path (find-console-bin-dir) "raco")
               "ctool"
               "--3m"
               "--c-mods"
               (path->string (build-path (find-system-path 'temp-dir) "embed-base.c"))
               "++lib"
               "racket/base"
               "++lib"
               "racket/place"
               "++lib"
               "tests/racket/embed-place"))
    (unless (system (format "cc -c -o embed-in-c.o ~a-DMZ_COLLECTION_PATH='\"~a\"' -I\"~a\" -DMZ_PRECISE_GC ~a embed-in-c.c"
                            (if use-declare? 
                                (format "-DUSE_DECLARED_MODULE -I\"~a\" " (find-system-path 'temp-dir))
                                "")
                            (find-collects-dir)
                            (find-include-dir)
                            (buildinfo "CFLAGS")))
      (error "compile failed"))
    
    (unless (system (format "cc -o embed-in-c embed-in-c.o -lm -ldl -pthread ~a"
                            (case (system-type 'link)
                              [(framework)
                               (format "-F\"~a\" -framework Racket" lib-dir)]
                              [(static shared)
                               (format "-L\"~a\" -lracket3m ~a ~a" lib-dir
                                       (buildinfo "LDFLAGS") (buildinfo "LIBS"))]
                              [else
                               (error "unsupported")])))
      (error "link failed"))
    
    (let ([o (open-output-bytes)]
          [e (open-output-bytes)])
      (unless (parameterize ([current-input-port (open-input-bytes #"5\n(log-error \"ouch!\")")]
                             [current-output-port o]
                             [current-error-port e])
                (system
                 (format "~a./embed-in-c 1 2 3"
                         (case (system-type 'link)
                           [(framework)
                            (format "env DYLD_FRAMEWORK_PATH=\"~a\" " lib-dir)]
                           [else ""]))))
        (error 'run "failed: ~s" (get-output-bytes e)))
      (test #"ouch!\n" (get-output-bytes e))
      (test #"1\n2\n3\n> 5\n> > " (get-output-bytes o)))

    (let ([maybe-delete-file 
           (lambda (f) (when (file-exists? f) (delete-file f)))])
      (maybe-delete-file "embed-in-c.o")
      (maybe-delete-file "embed-in-c")))
  
  (printf "passed ~a declare\n" (if use-declare? "with" "without")))

(go #f)
(go #t)


