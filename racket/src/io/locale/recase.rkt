#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/utf-16-encode.rkt"
         "../string/utf-16-decode.rkt"
         "../converter/main.rkt"
         "parameter.rkt"
         "string.rkt"
         "nul-char.rkt"
         "ucs-4.rkt")

(provide string-locale-upcase
         string-locale-downcase
         
         locale-recase)

(define/who (string-locale-upcase s)
  (check who string? s)
  (recase s #:up? #t))

(define/who (string-locale-downcase s)
  (check who string? s)
  (recase s #:up? #f))

(define (recase s #:up? up?)
  ;; Primitive functions don't work with nul characters, so we handle
  ;; those directly
  (define len (string-length s))
  (let loop ([pos 0])
    (define i-len (+ pos (string-length-up-to-nul s pos len)))
    (cond
      [(= i-len len)
       (define new-s (recase/no-nul (maybe-substring s pos len) up?))
       (if (eqv? pos 0)
           new-s
           (list new-s))]
      [else
       (define new-s (recase/no-nul (substring s pos i-len) up?))
       (define r (loop (+ i-len 1)))
       (if (eqv? pos 0)
           (apply string-append new-s (string #\nul) r)
           (cons new-s (cons (string #\nul) r)))])))

(define (recase/no-nul s up?)
  (cond
    [(and (equal? (current-locale) "")
          (not (zero? (bitwise-and (rktio_convert_properties rktio) RKTIO_CONVERT_RECASE_UTF16))))
     ;; The OS provides a UTF-16-based function, so use that
     (define s-16 (utf-16-encode s))
     (start-atomic)
     (define r (rktio_recase_utf16 rktio
                                   up?
                                   s-16 (arithmetic-shift (bytes-length s-16) -1)
                                   #f))
     (define sr (rktio_to_shorts r))
     (rktio_free r)
     (end-atomic)
     (utf-16-decode sr)]
    [else
     ;; We don't just convert to a locale encoding and recase,
     ;; because there might be an encoding error; we'll leave
     ;; encoding-error bytes alone.
     (define c #f)
     (define in-bstr (string->bytes/ucs-4 s 0 (string-length s)))
     (dynamic-wind
      (lambda ()
        (set! c (bytes-open-converter ucs-4-encoding (locale-string-encoding))))
      (lambda ()
        (let loop ([pos 0])
          (cond
            [(= pos (bytes-length in-bstr))
             (if (eqv? pos 0)
                 ""
                 '(""))]
            [else
             (define-values (bstr in-used status)
               (bytes-convert c in-bstr pos))
             (start-atomic)
             (sync-locale!)
             (define sr (locale-recase #:up? up? bstr))
             (end-atomic)
             (define ls (bytes->string/locale sr))
             (cond
               [(eq? status 'complete)
                (if (eqv? pos 0)
                    ls
                    (list ls))]
               [else
                (define r (loop (+ pos in-used 4)))
                (define err-s (string (string-ref s (arithmetic-shift (+ pos in-used) -2))))
                (if (eqv? pos 0)
                    (apply string-append ls err-s r)
                    (list* ls err-s r))])])))
      (lambda ()
        (bytes-close-converter c)))]))

;; in atomic mode
;; Assumes that the locale is sync'ed
(define (locale-recase #:up? up? s)
  (define p (rktio_locale_recase rktio up? s))
  (define r (rktio_to_bytes p))
  (rktio_free p)
  r)
