#lang racket/base
(require racket/match
         racket/cmdline
         syntax/modresolve
         "check-requires.rkt")

#|
See comments in check-requires.rkt for what this analysis does and
how to interpret the results.

Example (from racket root directory):

  racket -l macro-debugger/analysis/check-requires-script \
    collects/syntax/*.rkt

|#

;; Configuration

;; show-keep? : (parameterof bool)
;; Show KEEP messages in output.
(define show-keep? (make-parameter #f))

;; show-bypass? : (parameterof bool)
;; Show BYPASS messages in output.
(define show-bypass? (make-parameter #f))

;; ========

(define (go mod)
  (printf "~s:\n" mod)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (printf "ERROR in ~s\n" mod)
                     ((error-display-handler) (exn-message exn) exn)
                     (newline))])
    (let ([recs (check-requires mod)])
      (for ([rec recs])
        (match rec
          [(list 'keep mpi phase comment)
           (when (show-keep?)
             (printf "KEEP ~a at ~a~a\n"
                     (mpi->key mpi) phase
                     (if comment (format "(~a)" comment #f) "")))]
          [(list 'bypass mpi phase)
           (when (show-bypass?)
             (printf "BYPASS ~a at ~a\n" (mpi->key mpi) phase))]
          [(list 'drop mpi phase)
           (printf "DROP ~a at ~a\n" (mpi->key mpi) phase)]))))
  (newline))

;; Command-line args are interpreted as files if the file exists,
;; module names otherwise.

(command-line
 #:once-each
 [("--show-keep")
  "Show KEEP recommendations"
  (show-keep? #t)]
 [("--show-bypass")
  "Show BYPASS recommendations"
  (show-bypass? #t)]
 [("--ignore-disappeared-use")
  "Ignore 'disappeared-use syntax property"
  (add-disappeared-uses? #f)]
 #:args args
 (for ([arg (in-list args)])
   (cond [(file-exists? arg)
          (go `(file ,arg))]
         [else
          (let* ([inport (open-input-string arg)]
                 [mod (read inport)])
            (unless (eof-object? (peek-char inport))
              (error "bad module name:" arg))
            (go mod))])))
