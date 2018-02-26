#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../string/utf-16-encode.rkt"
         "../converter/main.rkt"
         "parameter.rkt"
         "string.rkt"
         "recase.rkt"
         "nul-char.rkt"
         "ucs-4.rkt")

(provide string-locale<?
         string-locale=?
         string-locale>?
         string-locale-ci<?
         string-locale-ci=?
         string-locale-ci>?)

(define (make-string-comparsion who cmp portable-cmp ci?)
  (lambda (arg . args)
    (check who string? arg)
    (for ([arg (in-list args)])
      (check who string? arg))
    (define locale-on? (current-locale))
    (let loop ([prev arg] [args args])
      (cond
        [(null? args) #t]
        [(if locale-on?
             (cmp (collate prev (car args) ci?) 0)
             (portable-cmp prev (car args)))
         (loop (car args) (cdr args))]
        [else #f]))))

(define/who string-locale<?
  (make-string-comparsion who < string<? #f))
(define/who string-locale=?
  (make-string-comparsion who = string=? #f))
(define/who string-locale>?
  (make-string-comparsion who > string>? #f))

(define/who string-locale-ci<?
  (make-string-comparsion who < string-ci<? #t))
(define/who string-locale-ci=?
  (make-string-comparsion who = string-ci=? #t))
(define/who string-locale-ci>?
  (make-string-comparsion who > string-ci>? #t))

;; The rktio-provided string-comparison functions don't handle strings
;; that contain the nul character, and locale-specific conversion also
;; may not support nul characters. So, we handle nul ourselves,
;; imposing the rule that a string is greater than any prefix of the
;; string.
(define (collate s1 s2 ci?)
  (define l1 (string-length s1))
  (define l2 (string-length s2))
  (let loop ([i1 0] [i2 0])
    (define t-l1 (+ i1 (string-length-up-to-nul s1 i1 l1)))
    (define t-l2 (+ i2 (string-length-up-to-nul s2 i2 l2)))
    (cond
      [(and (= l1 t-l1)
            (= l2 t-l2))
       (collate/no-nul (maybe-substring s1 i1 l1) (maybe-substring s2 i2 l2) ci?)]
      [else
       (define v (collate/no-nul (substring s1 i1 t-l1)
                                 (substring s2 i2 t-l2)
                                 ci?))
       (cond
         [(not (zero? v)) v]
         [(= l1 t-l1) (if (= l2 t-l2) 0 -1)]
         [(= l2 t-l2) 1]
         [else
          ;; Both strings have more content, so skip nuls and check more
          (loop (+ t-l1 1) (+ t-l2 1))])])))

;; Compare two strings that do not include the nul character
(define (collate/no-nul s1 s2 ci?)
  (cond
    [(and (equal? (current-locale) "")
          (not (zero? (bitwise-and (rktio_convert_properties rktio) RKTIO_CONVERT_STRCOLL_UTF16))))
     ;; The OS provides a UTF-16-based collation function, so use that
     (define s1-16 (utf-16-encode s1))
     (define s2-16 (utf-16-encode s2))
     (rktio_strcoll_utf16 rktio
                          s1-16 (arithmetic-shift (bytes-length s1-16) -1)
                          s2-16 (arithmetic-shift (bytes-length s2-16) -1)
                          ci?)]
    [else
     ;; We don't just convert to a locale encoding and compare,
     ;; because there might be an encoding error, and we want
     ;; to treat unencodable as strictly greater than encodable.
     (define c1 #f)
     (define c2 #f)
     (define in-bstr1 (string->bytes/ucs-4 s1 0 (string-length s1)))
     (define in-bstr2 (string->bytes/ucs-4 s2 0 (string-length s2)))
     (dynamic-wind
      (lambda ()
        (set! c1 (bytes-open-converter ucs-4-encoding (locale-string-encoding)))
        (set! c2 (bytes-open-converter ucs-4-encoding (locale-string-encoding))))
      (lambda ()
        (let loop ([pos1 0] [pos2 0] [end1 (bytes-length in-bstr1)] [end2 (bytes-length in-bstr2)])
          (define-values (bstr1 in-used1 status1)
            (bytes-convert c1 in-bstr1 pos1 end1))
          (define-values (bstr2 in-used2 status2)
            (bytes-convert c2 in-bstr2 pos2 end2))
          (define new-pos1 (+ in-used1 pos1))
          (define new-pos2 (+ in-used2 pos2))
          (define done1? (= new-pos1 end1))
          (define done2? (= new-pos2 end2))
          (define (check-one-byte)
            (define ch1 (string-ref s1 (arithmetic-shift new-pos1 -2)))
            (define ch2 (string-ref s2 (arithmetic-shift new-pos2 -2)))
            (cond
              [(char<? ch1 ch2) -1]
              [(char<? ch2 ch1) 1]
              [else
               (loop (+ new-pos1 4) (+ new-pos2 4) (bytes-length in-bstr1) (bytes-length in-bstr2))]))
          (cond
            [(and done1? done2?)
             (define v
               (atomically
                (sync-locale!)
                (if ci?
                    (rktio_locale_strcoll rktio
                                          (locale-recase #:up? #f bstr1)
                                          (locale-recase #:up? #f bstr2))
                    (rktio_locale_strcoll rktio bstr1 bstr2))))
             (cond
               [(zero? v)
                ;; If there's a conversion either for either string, then
                ;; count an error as greater than anything. If a conversion
                ;; error in both, then compare bytes
                (define really-done1? (= new-pos1 (bytes-length in-bstr1)))
                (define really-done2? (= new-pos2 (bytes-length in-bstr2)))
                (cond
                  [(and really-done1? really-done2?) 0]
                  [really-done1? -1]
                  [really-done2? 1]
                  [else (check-one-byte)])]
               [else v])]
            [else
             ;; One or both had encoding errors. Try again using
             ;; the most bytes that succeeded for both
             (define len (min in-used1 in-used2))
             (cond
               [(zero? len)
                ;; There's no prefix we can try for both.
                (cond
                  [(and (zero? in-used1) (zero? in-used2))
                   ;; Both had an encoding error
                   (check-one-byte)]
                  [(zero? in-used1) 1]
                  [else -1])]
               [else
                (loop pos1 pos2 (+ pos1 len) (+ pos2 len))])])))
      (lambda ()
        (bytes-close-converter c1)
        (bytes-close-converter c2)))]))
