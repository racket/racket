#lang scheme/base

(require scheme/system scheme/port)

(provide run-pdflatex)

(define (run-pdflatex file [notify void])
  (define cmd
    (list (get-pdflatex-binary)
          "-interaction=batchmode"
          (format "~a" file)))
  (define logfile (path-replace-suffix file #".log"))
  (define (run)
    (unless (parameterize ([current-output-port (open-output-nowhere)])
              (apply system* cmd))
      (unless (file-exists? logfile)
        (err "did not generate a log file at ~a" logfile))
      (call-with-input-file* logfile
        (lambda (log) (copy-port log (current-error-port))))
      (err "got error exit code")))
  (let loop ([n 0])
    (when (= n 5)
      (err "didn't get a stable result after ~a runs" n))
    (if (zero? n)
      (notify "running pdflatex on ~a" file)
      (notify " running ~a~a time"
              (add1 n) 
              (case (add1 n) [(2) 'nd] [(3) 'rd] [else 'th])))
    (run)
    ;; see if we get a "Rerun" note, these seem to come in two flavors
    ;; * Label(s) may have changed. Rerun to get cross-references right.
    ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
    (cond [(call-with-input-file* logfile
             (lambda (log) (regexp-match? #px#"changed\\.\\s+Rerun" log)))
           (loop (add1 n))]
          [(zero? n)
           (notify "WARNING: no \"Rerun\" found in first run of pdflatex for ~a"
                   file)]))
  (path-replace-suffix file #".pdf"))

(define (get-pdflatex-binary)
  (define ans
    (case (system-type)
      [(macosx) (or (find-executable-path "pdflatex")
                    (for/or ([macosx-candidate (in-list macosx-candidates)])
                      (and (file-exists? macosx-candidate)
                           macosx-candidate)))]
      [(windows) (or (find-executable-path "pdflatex")
                     (find-executable-path "pdflatex.exe"))]
      [(unix) (find-executable-path "pdflatex")]))
  (unless ans
    (err "could not find a `pdflatex' executable"))
  ans)

(define (err fmt . args) (apply error 'run-pdflatex fmt args))

;; under mac os x, gui apps do not get started with
;; a good path environment, so put likely candidates
;; for a latex installation here so that the "scribble
;; pdf" button is more likely to work in drracket
(define macosx-candidates
  '("/usr/texbin/pdflatex"))
