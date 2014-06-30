#lang scheme/base

(require scheme/system scheme/port)

(provide run-pdflatex run-dvipdf-latex)

(define (run-pdflatex file [notify void]) (run file notify #f))
(define (run-dvipdf-latex file [notify void]) 
  (parameterize ([function-name 'run-dvipdf-latex])
    (run file notify #t)))

(define max-runs 5)
(define (run file notify via-dvipdf?)
  (define latex-cmd-name (if via-dvipdf? "latex" "pdflatex"))
  (define cmd
    (list (get-latex-binary latex-cmd-name)
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
    (when (= n max-runs)
      (err "didn't get a stable result after ~a runs" n))
    (if (zero? n)
      (notify "running ~a on ~a" latex-cmd-name file)
      (notify " running ~a~a time"
              (add1 n) 
              (case (normalize-for-suffix (add1 n)) [(2) 'nd] [(3) 'rd] [else 'th])))
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
  (when via-dvipdf?
    (define dvi-file (path-replace-suffix file #".dvi"))
    (define ps-file (path-replace-suffix file #".ps"))
    (unless (file-exists? dvi-file) (err "didn't find .dvi file"))
    (define dvips (get-latex-binary "dvips"))
    (define pstopdf (get-latex-binary "pstopdf"))
    (notify "running dvips on ~a" dvi-file)
    (define stderr (open-output-bytes))
    (unless (parameterize ([current-output-port (open-output-nowhere)]
                           [current-error-port stderr])
              (system* dvips dvi-file))
      (displayln (get-output-bytes stderr))
      (err "got error exit code"))
    (unless (parameterize ([current-output-port (open-output-nowhere)]
                           [current-error-port stderr])
              (system* pstopdf ps-file))
      (displayln (get-output-bytes stderr))
      (err "got error exit code")))
  (path-replace-suffix file #".pdf"))

(define (normalize-for-suffix n)
  (cond
    [(<= 10 n 20) 0]
    [else (modulo n 10)]))

(define (get-latex-binary name)
  (define ans
    (case (system-type)
      [(macosx) (or (find-executable-path name)
                    (for/or ([macosx-candidate-dir (in-list macosx-candidate-dirs)])
                      (define macosx-candidate (build-path macosx-candidate-dir name))
                      (and (file-exists? macosx-candidate)
                           macosx-candidate)))]
      [(windows) (or (find-executable-path name)
                     (find-executable-path (format "~a.exe" name)))]
      [(unix) (find-executable-path name)]))
  (unless ans
    (err (format "could not find a `~a' executable" name)))
  ans)

(define function-name (make-parameter 'run-pdflatex))
(define (err fmt . args) (apply error (function-name) fmt args))

;; under mac os x, gui apps do not get started with
;; a good path environment, so put likely candidates
;; for directories holding latex/pdflatex binaries
;; here so that the "scribble pdf" button is more 
;; likely to work in drracket
(define macosx-candidate-dirs
  '("/usr/texbin"))
