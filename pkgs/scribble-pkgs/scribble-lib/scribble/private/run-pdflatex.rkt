#lang scheme/base

(require scheme/system scheme/port)

(provide run-pdflatex)

(define (run-pdflatex file [notify void])
  (define (err fmt . args) (apply error 'run-pdflatex fmt args))
  (define cmd
    (list (or (find-executable-path "pdflatex")
              (and (eq? 'windows (system-type))
                   (find-executable-path "pdflatex.exe"))
              (err "could not find a `pdflatex' executable"))
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
