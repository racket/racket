#lang scheme
(require (planet jaymccarthy/job-queue)
         scheme/system
         "config.ss"
         "dirstruct.ss"
         "cache.ss")

(define test-workers (make-job-queue (number-of-cpus)))

(define start-revision #f)
(define history? #f)

(command-line #:program "time"
              #:once-each
              ["-H" "Run on all revisions"
                    (set! history? #t)]
              ["-r" rev
                    "Start with a particular revision"
                    (set! start-revision (string->number rev))])

(unless start-revision
  (init-revisions!)
  (set! start-revision (newest-revision)))

(define (make-log! filename)
  (submit-job!
   test-workers
   (lambda ()
     (apply 
      system*/exit-code
      (path->string
       (build-path (plt-directory) "plt" "bin" "mzscheme"))
      "-t" 
      (path->string (build-path (drdr-directory) "time-file.ss"))
      "--"
      (append
       (if history?
           (list "-H")
           (list "-r" (number->string start-revision)))
       (list
        (path->string filename))))
     (system*/exit-code
      (path->string
       (build-path (plt-directory) "plt" "bin" "mred"))
      "-t"
      (path->string (build-path (drdr-directory) "graph.ss"))
      "--"
      (path->string (path-timing-log filename))
      (path->string (path-timing-png filename))))))

(define (find-files p l)
  (for ([f (in-list (cached-directory-list* p))])
    (define fp (build-path p f))
    (define fl (list* f l))
    (if (cached-directory-exists? fp)
        (find-files fp fl)
        (make-log! (apply build-path (reverse fl))))))

(find-files (revision-log-dir start-revision)
            empty)

(stop-job-queue! test-workers)