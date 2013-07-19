#lang racket/base
(require setup/dirs
         setup/variant)
(provide find-exe)

(define (find-exe [mred? #f] [variant (system-type 'gc)])
  (let* ([base (if mred?
                   (find-lib-dir)
                   (find-console-bin-dir))]
         [fail
          (lambda ()
            (error 'find-exe
                   "can't find ~a executable for variant ~a"
                   (if mred? "GRacket" "Racket")
                   variant))])
    (let ([exe (build-path
                base
                (case (system-type)
                  [(macosx)
                   (cond
                     [(not mred?)
                      ;; Need Racket:
                      (string-append "racket" (variant-suffix variant #f))]
                     [mred?
                      ;; Need GRacket:
                      (let ([sfx (variant-suffix variant #t)])
                        (build-path (format "GRacket~a.app" sfx)
                                    "Contents" "MacOS"
                                    (format "GRacket~a" sfx)))])]
                  [(windows)
                   (format "~a~a.exe" (if mred?
                                          "GRacket"
                                          "Racket")
                           (variant-suffix variant #t))]
                  [(unix)
                   (format "~a~a" (if mred?
                                      "gracket"
                                      "racket")
                           (variant-suffix variant #f))]))])
      (unless (or (file-exists? exe)
                  (directory-exists? exe))
        (fail))
      exe)))
