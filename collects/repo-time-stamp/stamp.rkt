#lang racket/base

(provide stamp)

#|

  This file provides a single binding, `stamp', with a string value that
  describes the Racket version.  The format of this stamp is a date, a
  SHA1, and a character describing how the information was retrieved:

    "YYYY-MM-DD(SHA1/H)"

  The description is attempted in several ways, with the `H' character
  indicating how it was actually obtained:

  * "a" -- the date and sha1 information were in the `archive-id' string
    below, which means that it had that information at the time that
    `git archive' created an archive out of a git repository.  This is
    the expected value for nightly builds.

  * "g" -- `archive-id' didn't have information, but we found a git
    executable and ran it.

  * "d" -- an executable was not found either, but a ".git" directory
    was found in the usual place, with a "HEAD" file that has eventually
    lead to a SHA1.  In this case, the time stamp is the stamp of the
    git reference file that was used.

  * "f" -- none of the above worked, so the last resort was to report
    the date of this file (which can provide a rough idea how old the
    tree is, but obviously this can be arbitrary).  In this case the
    SHA1 is missing and will be "-".

|#

(define archive-id "$Format:%ct|%h|a$")
;; when exported through `git archive', the above becomes something like
;; "1273562690|cabd414|a"

(require racket/system racket/runtime-path)

(define-runtime-path this-dir  ".")
(define-runtime-path this-file "stamp.rkt")

(define stamp
  (let ([rx:secs+id #rx"^([0-9]+)\\|([0-9a-f]+|-)\\|(.*?)[ \r\n]*$"])
    ;; info from an archive (incl. nightly builds)
    (define (from-archive-id) archive-id)
    ;; try to run git to get the current info
    (define (from-running-git)
      (let ([exe (or (find-executable-path "git")
                     (find-executable-path "git.exe")
                     (and (eq? 'macosx (system-type))
                          (find-executable-path "/opt/local/bin/git")))])
        (and exe (let ([out (open-output-string)])
                   (parameterize ([current-output-port out]
                                  [current-error-port out]
				  [current-input-port (open-input-string "")]
                                  [current-directory this-dir])
                     (system* exe "log" "-1" "--pretty=format:%ct|%h|g")
                     (get-output-string out))))))
    ;; try to find a ".git" directory (can't run git, so conventional
    ;; guess) and use the sha1 from that file and its date
    (define (from-git-dir)
      (define git-dir (build-path this-dir 'up 'up ".git"))
      (let loop ([file (build-path git-dir "HEAD")])
        (let ([l (and (file-exists? file)
                      (call-with-input-file file read-line))])
          (cond [(not l) #f]
                [(regexp-match #rx"^ref: +(.*)$" l)
                 => (lambda (m)
                      (loop (build-path git-dir (cadr m))))]
                [(regexp-match #px"^[[:xdigit:]]{40}$" l)
                 (format "~a|~a|d"
                         (file-or-directory-modify-seconds file)
                         (substring l 0 8))]))))
    ;; fallback: get the date of this file, no id
    (define (from-this-file)
      (format "~a|-|f" (file-or-directory-modify-seconds this-file)))
    (for*/or ([x (list from-archive-id
                       from-running-git
                       from-git-dir
                       from-this-file)])
      (let* ([x (x)]
             [m (and (string? x) (regexp-match rx:secs+id x))]
             [d (and m (seconds->date (string->number (cadr m))))]
             [id (and m (caddr m))]
             [how (and m (cadddr m))])
        (define (pad02 f) (let ([n (f d)]) (if (< n 10) (format "0~a" n) n)))
        (and d (format "~a-~a-~a(~a/~a)"
                       (date-year d) (pad02 date-month) (pad02 date-day)
                       id how))))))
