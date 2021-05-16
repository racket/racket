#lang racket/base
(require setup/dirs
         setup/variant
         setup/cross-system)
(provide find-exe)

(define (find-exe #:cross? [cross? #f]
                  #:untethered? [untethered? #f]
                  [mred? #f]
                  [variant (if cross?
                               (cross-system-type 'gc)
                               (system-type 'gc))])
  (define (->list a) (if a (list a) null))
  (define bases (if mred?
                    (append
                     (->list (and (not untethered?)
                                  (find-addon-tethered-gui-bin-dir)))
                     (->list (and (not untethered?)
                                  (find-config-tethered-gui-bin-dir)))
                     (if cross?
                         (get-cross-lib-search-dirs)
                         (get-lib-search-dirs)))
                    (append
                     (->list (and (not untethered?)
                                  (find-addon-tethered-console-bin-dir)))
                     (->list (and (not untethered?)
                                  (find-config-tethered-console-bin-dir)))
                     (get-console-bin-search-dirs))))
  (define exe
    (for/or ([base (in-list bases)])
      (define exe (build-path
                   base
                   (case (if cross?
                             (cross-system-type)
                             (system-type))
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
                              (variant-suffix variant #f))])))
      (and (or (file-exists? exe)
               (directory-exists? exe))
           exe)))
  (unless exe
    (error 'find-exe
           "can't find ~a executable for variant ~a"
           (if mred? "GRacket" "Racket")
           variant))
  exe)
