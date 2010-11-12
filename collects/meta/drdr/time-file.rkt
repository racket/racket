#lang racket
(require "config.rkt"
         "dirstruct.rkt"
         "cache.rkt"
         "path-utils.rkt"
         "status.rkt")

(define revision #f)

(define filename
  (command-line #:program "time-file"
                #:once-any
                ["-H" "Run on all revisions"
                      (set! revision #f)]
                ["-r" rev "Run on one revision"
                      (set! revision (string->number rev))]
                #:args (filename) filename))

(define (output-for-rev rev)
  (define log 
    (read-cache* (build-path (revision-log-dir rev) filename)))
  (when log
    (printf "~S\n"
            (list rev 
                  (status-duration log)
                  (filter-map
                   (match-lambda
                     [(struct stdout ((regexp #px#"cpu time: (\\d+) real time: (\\d+) gc time: (\\d+)" 
                                              (list _ 
                                                    (app (compose string->number bytes->string/utf-8) cpu)
                                                    (app (compose string->number bytes->string/utf-8) real)
                                                    (app (compose string->number bytes->string/utf-8) gc)))))
                      (list cpu real gc)]
                     [_
                      #f])
                   (status-output-log log))))))


(define data-file (path-timing-log filename))
(with-handlers ([exn:fail? void])
  (make-parent-directory data-file))
(printf "Making log for ~a\n" filename)

(if revision
    (with-output-to-file
        data-file
      #:exists 'append
      (lambda ()
        (output-for-rev revision)))
    (with-output-to-file
        data-file
      #:exists 'replace
      (lambda ()
        (init-revisions!)
        (for ([rev (in-list revisions)])
          (output-for-rev rev)))))
