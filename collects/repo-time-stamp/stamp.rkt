#lang racket/base

(provide stamp)

(define archive-id "$Format:%ct|%h|a$")
;; when exported through `git archive', the above becomes something like
;; "1273562690|cabd414|a"

(require racket/system racket/runtime-path)

(define-runtime-path this-dir  ".")
(define-runtime-path this-file "stamp.rkt")

(define stamp
  (let ([rx:secs+id #rx"^([0-9]+)\\|([0-9a-f]+|-)\\|(.*?)[ \r\n]*$"])
    (for*/or ([x (list
                  ;; info from an archive (incl. nightly builds)
                  (lambda () archive-id)
                  ;; try to run git to get the current info
                  (lambda ()
                    (let ([exe (or (find-executable-path "git")
                                   (find-executable-path "git.exe"))])
                      (and exe
                           (let ([out (open-output-string)])
                             (parameterize ([current-output-port out]
                                            [current-error-port out]
                                            [current-directory this-dir])
                               (system* exe "log" "-1"
                                        "--pretty=format:%ct|%h|g")
                               (get-output-string out))))))
                  ;; fallback: get the date of this file, no id
                  (lambda ()
                    (format "~a|-|f"
                            (file-or-directory-modify-seconds this-file))))])
      (let* ([x (x)]
             [m (and (string? x) (regexp-match rx:secs+id x))]
             [d (and m (seconds->date (string->number (cadr m))))]
             [id (and m (caddr m))]
             [how (and m (cadddr m))])
        (define (pad02 f) (let ([n (f d)]) (if (< n 10) (format "0~a" n) n)))
        (and d (format "~a-~a-~a(~a/~a)"
                       (date-year d) (pad02 date-month) (pad02 date-day)
                       id how))))))
