#lang scheme

(require scheme/system
         "dirstruct.ss"
         "analyze.ss"
         "monitor-svn.ss"
         "notify.ss"
         "retry.ss"
         "config.ss"
         "plt-build.ss"
         "svn.ss"
         "cache.ss"
         "path-utils.ss")

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
         (build-path (plt-directory) "plt" "bin" "mzscheme"))
        "-t" 
        (path->string (build-path (drdr-directory) "time.ss"))
        "--"
        "-r" (number->string cur-rev))))
    
    (notify! "Archiving old revisions")
    (cache/file/timestamp
     (build-path rev-dir "archiving-done")
     (lambda ()
       (system*/exit-code
        (path->string
         (build-path (plt-directory) "plt" "bin" "mzscheme"))
        "-t" 
        (path->string (build-path (drdr-directory) "make-archive.ss"))
        "--"
        "--many" (number->string 100))))))

(notify! "Last revision is r~a" cur-rev)
(handle-revision prev-rev cur-rev)
(notify! "Starting to monitor SVN @ r~a" cur-rev)
(monitor-svn (plt-repository)
             cur-rev
             (lambda (newer)
               (for ([l (in-list newer)])
                 (write-cache! (future-record-path (svn-rev-log-num l)) l)))
             (lambda (prev-rev cur-rev _log)
               (handle-revision prev-rev cur-rev)
               
               ; We have problems running for a long time so just restart after each rev
               (exit 0)
               ))