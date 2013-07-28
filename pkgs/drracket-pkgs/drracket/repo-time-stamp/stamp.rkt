#lang racket/base

(provide stamp)

#|

  This file provides a single binding, `stamp', with a string value that
  describes the Racket version, or #f to indicate a "release" without a
  stamp.

  The format of a string stamp is a date, a SHA1, and a character
  describing how the information was retrieved:

    "YYYY-MM-DD(SHA1/H)"

  The description is attempted in several ways, with the `H' character
  indicating how it was actually obtained:

  * "a" -- the date and sha1 information were in the installation's
    configuration (as a non-empty string). This is the expected value
    for a snapshot build, and if the installation's configuration has
    a `build-stamp' entry, this format or #f are the only possibilities.

  * "g" -- `archive-id' didn't have information, but we found a git
    executable and ran it. [*]  This strategy is currently disabled.

  * "d" -- an executable was not found either, but a ".git" directory
    was found in the usual place, with a "HEAD" file that has eventually
    lead to a SHA1.  In this case, the time stamp is the stamp of the
    git reference file that was used. [*]

  * "f" -- none of the above worked, so the last resort was to report
    the date of this file (which can provide a rough idea how old the
    tree is, but obviously this can be arbitrary).  In this case the
    SHA1 is missing and will be "-".

  [*] In case of "g"/"d", another part is added, indicating the branch
  name if one was found and if it isn't "master"; Eg, ".../g/foo".

|#

(require racket/system
         racket/runtime-path
         racket/string
         racket/date
         setup/dirs)

(define-runtime-path this-dir  ".")
(define-runtime-path this-file "stamp.rkt")

(define (compute-stamp)
  (let ([rx:secs+id #rx"^([0-9]+)\\|([0-9a-f]+|-)\\|(.*?)[ \r\n]*$"])
    ;; info from an archive (incl. nightly builds)
    (define (from-archive-id) 
      (define m (regexp-match #rx"^([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])-([0-9a-f]+)$"
                              (or (get-build-stamp) "")))
      (define (n pos) (string->number (list-ref m pos)))
      (and m
           (format "~a|~a|a" 
                   (with-handlers ([exn:fail? (lambda (exn) 
                                                ;; Last resort:
                                                (current-seconds))])
                     (with-handlers ([exn:fail? (lambda (exn)
                                                  ;; Local time failed; try UTC:
                                                  (find-seconds 0 0 12 (n 3) (n 2) (n 1) #f))])
                       ;; Convert to seconds, hopfully for round-trip in seconds->date
                       ;; conversion below:
                       (find-seconds 0 0 12 (n 3) (n 2) (n 1))))
                   (list-ref m 4))))
    ;; adds a branch name if applicable (and if different from `master')
    (define (add-branch str br*)
      (define br
        (and (string? br*)
             (not (member br* '("refs/heads/master" "")))
             (regexp-replace #rx"^refs/(?:heads/|remotes/)?" br* "")))
      (if br (string-append str "/" br) str))
    ;; try to run git to get the current info
    (define (from-running-git)
      (define exe
        (or (find-executable-path "git")
            (find-executable-path "git.exe")
            (and (eq? 'macosx (system-type))
                 (find-executable-path "/opt/local/bin/git"))))
      (define (git . args)
        (define out (open-output-string))
        (parameterize ([current-output-port out]
                       [current-error-port out]
                       [current-input-port (open-input-string "")]
                       [current-directory this-dir])
          (apply system* exe args)
          (string-trim (get-output-string out))))
      (and exe (add-branch (git "log" "-1" "--pretty=format:%ct|%h|g")
                           (git "rev-parse" "--symbolic-full-name" "HEAD"))))
    ;; try to find a ".git" directory (can't run git, so conventional
    ;; guess) and use the sha1 from that file and its date
    (define (from-git-dir)
      (define git-dir (build-path this-dir 'up 'up 'up 'up ".git"))
      (define branch #f)
      (let loop ([file (build-path git-dir "HEAD")])
        (define l (and (file-exists? file)
                       (call-with-input-file file read-line)))
        (cond [(not l) #f]
              [(regexp-match #rx"^ref: +(.*)$" l)
               => (λ (m) (unless branch (set! branch (cadr m)))
                         (loop (build-path git-dir (cadr m))))]
              [(regexp-match #px"^[[:xdigit:]]{40}$" l)
               (add-branch (format "~a|~a|d"
                                   (file-or-directory-modify-seconds file)
                                   (substring l 0 8))
                           branch)])))
    ;; fallback: get the date of this file, no id
    (define (from-this-file)
      (format "~a|-|f" (file-or-directory-modify-seconds this-file)))
    (for*/or ([x (list from-archive-id
                       ;; from-running-git
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

(define stamp
  (if (equal? "" (get-build-stamp))
      ;; Release
      #f
      ;; Compute it:
      (compute-stamp)))
