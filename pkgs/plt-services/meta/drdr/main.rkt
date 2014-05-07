#lang racket

(require racket/system
         "dirstruct.rkt"
         "analyze.rkt"
         "monitor-scm.rkt"
         "notify.rkt"
         "retry.rkt"
         "config.rkt"
         "plt-build.rkt"
         "scm.rkt"
         "cache.rkt"
         "path-utils.rkt")

(init-revisions!)
(define cur-rev (newest-revision))
(define prev-rev (second-newest-revision))

(define (handle-revision prev-rev cur-rev)
  (define rev-dir (revision-dir cur-rev))
  (parameterize ([current-rev cur-rev]
                 [previous-rev prev-rev])
    (notify! "Removing future record for r~a" cur-rev)
    (safely-delete-directory (future-record-path cur-rev))

    (notify! "Starting to integrate revision r~a" cur-rev)
    (integrate-revision cur-rev)

    (notify! "Analyzing logs of r~a [prev: r~a]" cur-rev prev-rev)
    (analyze-revision cur-rev)

    (notify! "Recording timing data")
    (cache/file/timestamp
     (build-path rev-dir "timing-done")
     (lambda ()
       (system*/exit-code
        (path->string
         (build-path (plt-directory) "plt" "bin" "racket"))
        "-t"
        (path->string (build-path (drdr-directory) "time.rkt"))
        "--"
        "-r" (number->string cur-rev))))

    (notify! "Recompressing")
    (cache/file/timestamp
     (build-path rev-dir "recompressing")
     (lambda ()
       (parameterize ([current-directory rev-dir])
         (system*/exit-code
          "/bin/bash"
          (path->string
           (build-path (drdr-directory) "recompress.sh"))))))

    (notify! "Archiving old revisions")
    (cache/file/timestamp
     (build-path rev-dir "archiving-done")
     (lambda ()
       (system*/exit-code
        (path->string
         (build-path (plt-directory) "plt" "bin" "racket"))
        "-t"
        (path->string
         (build-path (drdr-directory) "make-archive.rkt"))
        "--"
        "--many" (number->string 45))))))

(notify! "Last revision is r~a" cur-rev)
(handle-revision prev-rev cur-rev)
(notify! "Starting to monitor @ r~a" cur-rev)
(monitor-scm (plt-repository)
             cur-rev
             (lambda (newer)
               (for ([rev (in-list newer)])
                 (write-cache!
                  (future-record-path rev)
                  (get-scm-commit-msg rev (plt-repository)))))
             (lambda (prev-rev cur-rev)
               (handle-revision prev-rev cur-rev)

               ;; We have problems running for a long time so just restart after each rev
               (exit 0)))
