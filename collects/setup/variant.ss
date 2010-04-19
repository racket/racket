#lang scheme/base

(require (prefix-in config: config) setup/dirs scheme/promise)

(provide variant-suffix)

(define plain-mz-is-cgc?
  (delay (let* ([dir (find-console-bin-dir)]
                [exe (cond [(eq? 'windows (system-type)) "Racket.exe"]
                           [(equal? #".dll" (system-type 'so-suffix))
                            ;; in cygwin so-suffix is ".dll"
                            "racket.exe"]
                           [else "racket"])]
                [f (build-path dir exe)])
           (and (file-exists? f)
                (with-input-from-file f
                  (lambda ()
                    (regexp-match? #rx#"bINARy tYPe:..c"
                                   (current-input-port))))))))

(define (variant-suffix variant cased?)
  (let ([r (case variant
             [(3m script-3m)   (or (force config:3m-suffix)
                                   (if (force plain-mz-is-cgc?) "3m" ""))]
             [(cgc script-cgc) (or (force config:cgc-suffix)
                                   (if (force plain-mz-is-cgc?) "" "CGC"))]
             [else (error 'variant-suffix "unknown variant: ~e" variant)])])
    (if cased? r (string-downcase r))))
