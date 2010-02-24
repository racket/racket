#lang scheme
(require scheme/system
         "dirstruct.ss"
         "status.ss"
         (except-in "diff.ss"
                    log-different?)
         "path-utils.ss"
         "cache.ss"
         "config.ss")

(define event-print
  (match-lambda
    [(struct stdout (bs))
     (display bs) (newline)]
    [(struct stdout (bs))
     (display bs (current-error-port)) (newline)]))

(define (render-output-diff r1 r2 f)
  (define l1 (status-output-log (read-cache (build-path (revision-log-dir r1) f))))
  (define l2 (status-output-log (read-cache (build-path (revision-log-dir r2) f))))
  
  (for ([d (in-list (render-log-difference l1 l2))])
    (match d
      [(struct difference (e))
       (printf "! ")
       (event-print e)]
      [(struct same-itude (e))
       (printf "  ")
       (event-print e)])))

(command-line #:program "diffcmd"
              #:args (rev1 rev2 filename)
              (render-output-diff (string->number rev1)
                                  (string->number rev2)
                                  filename))