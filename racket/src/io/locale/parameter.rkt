#lang racket/base
(require "../host/place-local.rkt"
         "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/convert.rkt")

(provide current-locale
         locale-string-encoding
         system-language+country

         locale-encoding-is-utf-8?
         locale-string-encoding/bytes
         sync-locale!)

(define current-locale
  (make-parameter (string->immutable-string "")
                  (lambda (v)
                    (unless (or (not v) (string? v))
                      (raise-argument-error 'current-locale "(or/c #f string?)" v))
                    (and v (string->immutable-string v)))
                  'current-locale))

(define-place-local installed-locale #f)

;; in rktio mode
;; Any rktio function that depends on the locale should be called in
;; a rktio region that includes an earlier `(sync-locale!)`
(define (sync-locale!)
  (define loc (current-locale))
  (unless (or (not loc)
              (equal? installed-locale loc))
    (set! installed-locale (current-locale))
    (rktio_set_locale rktio (string->bytes/utf-8 installed-locale))))

(void (rktio_set_default_locale #""))
(void (sync-locale!))

;; potentially in rktio mode
(define (locale-encoding-is-utf-8?)
  (define t (system-type))
  (define loc (current-locale))
  (or (not loc)
      (and (or (eq? t 'macosx)
               (eq? t 'windows))
           (equal? loc ""))
      (zero? (bitwise-and (rktio_convert_properties rktio) RKTIO_CONVERTER_SUPPORTED))))

;; in rktio mode
(define (locale-string-encoding/bytes)
  (cond
    [(locale-encoding-is-utf-8?) #"UTF-8"]
    [else
     (sync-locale!)
     (define e (rktio_locale_encoding rktio))
     (cond
       [(rktio-error? e)
        (end-rktio)
        (raise-rktio-error 'locale-string-encoding e "error getting locale encoding")]
       [else
        (begin0
          (rktio_to_bytes e)
          (rktio_free e))])]))

(define (locale-string-encoding)
  (bytes->string/utf-8 (rktioly (locale-string-encoding/bytes)) #\?))

(define/who (system-language+country)
  (start-rktio)
  (define c (rktio_system_language_country rktio))
  (cond
    [(rktio-error? c)
     (end-rktio)
     (raise-rktio-error who c "error getting language and country information")]
    [else
     (bytes->string/utf-8
      (begin0
        (rktio_to_bytes c)
        (rktio_free c)
        (end-rktio))
      #\?)]))
