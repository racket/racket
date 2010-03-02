#lang scheme
(require (planet jaymccarthy/job-queue)
         scheme/system
         "config.ss"
         "notify.ss"
         "path-utils.ss"
         "dirstruct.ss"
         "sema.ss"
         "cache.ss")

(define test-workers (make-job-queue (number-of-cpus)))

(define start-revision #f)
(define history? #f)
(define just-graphs? #f)

(command-line #:program "time"
              #:once-each
              ["-H" "Run on all revisions"
                    (set! history? #t)]
              ["-G" "Just graphs"
                    (set! just-graphs? #t)]
              ["-r" rev
                    "Start with a particular revision"
                    (set! start-revision (string->number rev))])

(unless start-revision
  (init-revisions!)
  (set! start-revision (newest-revision)))

(define rebaser
  (rebase-path (plt-data-directory) "/data"))

(define count-sema (make-semaphore 0))

(define (make-log! filename)
  (submit-job!
   test-workers
   (lambda ()
     (define prefix
       (path-timing-png-prefix filename))
     (unless just-graphs?
       (notify! "Dropping timing for ~a" filename)
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
          (path->string filename)))))
     (notify! "Generating graph for ~a" filename)
     (system*/exit-code
      (path->string
       (build-path (plt-directory) "plt" "bin" "mred-text"))
      "-t"
      (path->string (build-path (drdr-directory) "graphs" "build-graph.ss"))
      "--"
      "-l" (string-append "http://drdr.plt-scheme.org/~a/" (path->string* filename)) ; XXX
      "--image-loc" "/graph-images/"
      (path->string (path-timing-log filename))
      (path->string prefix)
      (path->string (rebaser prefix))
      (path->string (path-timing-html filename)))
     (notify! "Done with ~a" filename)
     (semaphore-post count-sema))))

(define (find-files p l)
  (for/fold ([i 0])
    ([f (in-list (cached-directory-list* p))])
    (define fp (build-path p f))
    (define fl (list* f l))
    (if (cached-directory-exists? fp)
        (+ i (find-files fp fl))
        (begin (make-log! (apply build-path (reverse fl)))
               (add1 i)))))

(define how-many-files
  (find-files (revision-log-dir start-revision)
              empty))

(semaphore-wait* count-sema how-many-files)

(stop-job-queue! test-workers)