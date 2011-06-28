#lang racket
(require racket/runtime-path
         racket/date
         "list-count.rkt"
         "scm.rkt"
         "formats.rkt"
         "cache.rkt"
         "metadata.rkt"
         "analyze.rkt"
         "rendering.rkt"
         "plt-build.rkt"
         "status.rkt"
         "replay.rkt"
         "notify.rkt"
         "path-utils.rkt"
         "dirstruct.rkt")

(build? #f)

(define show-log
  (command-line #:program "house-call"
                #:once-each
                [("-j" "--jobs") jobs "How many processes to run simultaneously" (number-of-cpus (string->number jobs))]
                ["--build" "Build the source first" (build? #t)]
                #:args log-to-view
                log-to-view))

; Find paths we need
(define (path->string^ p)
  (and p (path->string p)))

(git-path (path->string^ (find-executable-path "git")))
(Xvfb-path (and (on-unix?) (path->string^ (find-executable-path "Xvfb"))))
(fluxbox-path (and (on-unix?) (path->string^ (find-executable-path "fluxbox"))))

; Find where we are
(define-runtime-path here ".")
(drdr-directory here)
(define this-rev-dir (build-path here 'up 'up 'up))

; Setup directories that DrDr needs
(define (make-file-or-directory-link* from to)
  (unless (link-exists? to)
    (make-file-or-directory-link from to)))

(define house-calls (build-path this-rev-dir "house-calls"))
(plt-directory house-calls)
(for ([d (in-list (list "builds" "future-builds" "data"))])
  (make-directory* (build-path house-calls d)))

(make-file-or-directory-link* this-rev-dir (build-path house-calls "repo"))
(make-file-or-directory-link* this-rev-dir (build-path house-calls "plt"))

; Make up a revision and link it in
(define fake-rev (date->julian/scalinger (current-date)))
(current-rev fake-rev)
(define fake-trunk (revision-trunk-dir fake-rev))
(make-parent-directory fake-trunk)
(make-file-or-directory-link* this-rev-dir fake-trunk)
(write-cache! (revision-commit-msg fake-rev)
              (make-git-push fake-rev "you!" empty))

; Override the props file
(hash-set! props-cache fake-rev
           (dynamic-require `(file ,(path->string (build-path this-rev-dir "collects" "meta" "props")))
                            'get-prop))

; Setup the logger
(void
 (thread
  (lambda ()
    (define recv (make-log-receiver (current-logger) 'info))
    (let loop ()
      (match-define (vector level msg val) (sync recv))
      (display msg) (newline)
      (loop)))))

; Do it!
(notify! "DrDr is making a house call...")
(integrate-revision fake-rev)

(define re (rebase-path (revision-log-dir fake-rev) "/"))
(define (print-lc label lc)
  (define l (lc->list lc))
  (unless (empty? l)
    (printf "~a:\n" label)
    (for ([bs (in-list l)])
      (printf "\t~a\n" 
              (substring (path->string* (re (bytes->path bs))) 1)))
    (newline)))

(match (analyze-logs fake-rev)
  [(struct rendering (start end duration timeout unclean stderr _ _))
   
   (print-lc "Timeout" timeout)
   (print-lc "Unclean Exit" unclean)
   (print-lc "STDERR Output" stderr)
   
   (printf "Duration (Abs): ~a\n"
           (format-duration-ms (- end start)))
   (printf "Duration (Sum): ~a\n"
           (format-duration-ms duration))]
  [#f
   (void)])

(for ([p (in-list show-log)])
  (define lp (build-path (revision-log-dir fake-rev) p))
  (match (read-cache lp)
    [(? status? s)
     (newline)
     (printf "Replaying ~a:\n" p)
     (printf "~a\n" (regexp-replace* #rx"<current-rev>" (apply string-append (add-between (status-command-line s) " ")) (number->string fake-rev)))
     (replay-status s)]
    [x
     (printf "Could not get ~a's log; got: ~s\n" p x)]))
