#lang racket
(require (planet jaymccarthy/job-queue)
         racket/system
         "config.rkt"
         "notify.rkt"
         "dirstruct.rkt"
         "sema.rkt"
         "cache.rkt")

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

(define count-sema (make-semaphore 0))

(define (make-log! filename)
  (submit-job!
   test-workers
   (lambda ()
     (notify! "Dropping timing for ~a" filename)
     (apply 
      system*/exit-code
      (path->string
       (build-path (plt-directory) "plt" "bin" "racket"))
      "-t" 
      (path->string (build-path (drdr-directory) "time-file.rkt"))
      "--"
      (append
       (if history?
           (list "-H")
           (list "-r" (number->string start-revision)))
       (list
        (path->string filename))))
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
