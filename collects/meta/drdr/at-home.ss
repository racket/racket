#lang racket
(require racket/runtime-path
         "scm.ss"
         "cache.ss"
         "metadata.ss"
         "plt-build.ss"
         "notify.ss"
         "path-utils.ss"
         "dirstruct.ss")

(command-line #:program "drdr-at-home"
              #:once-each
              [("-j" "--jobs") jobs "How many processes to run simultaneously" (number-of-cpus (string->number jobs))]
              ["--no-build" "Do not build, just test" (build? #f)])

; Find paths we need
(define (path->string^ p)
  (and p (path->string p)))

(git-path (path->string^ (find-executable-path "git")))
(Xvfb-path #f #;(path->string^ (find-executable-path "Xvfb")))
(fluxbox-path #f #;(path->string^ (find-executable-path "fluxbox")))

; Find where we are
(define-runtime-path here ".")
(drdr-directory here)
(define this-rev-dir (build-path here 'up 'up 'up))

; Setup directories that DrDr needs
(define tmp-plt (make-temporary-file "plt~a" 'directory))
(plt-directory tmp-plt)
(for ([d (in-list (list "builds" "future-builds" "data"))])
  (make-directory* (build-path tmp-plt d)))

(make-file-or-directory-link this-rev-dir (build-path tmp-plt "repo"))
(make-file-or-directory-link this-rev-dir (build-path tmp-plt "plt"))

; Make up a revision and link it in
(define fake-rev 50)
(define fake-trunk (revision-trunk-dir fake-rev))
(make-parent-directory fake-trunk)
(make-file-or-directory-link this-rev-dir fake-trunk)
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
(notify! "Starting DrDr at home")
(integrate-revision fake-rev)

; Clean up
(delete-directory/files tmp-plt)