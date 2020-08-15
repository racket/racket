#lang racket/base
(require "../host/thread.rkt"
         "../host/place-local.rkt"
         "../converter/main.rkt"
         (submod "../converter/main.rkt" reset)
         "ucs-4.rkt")

(provide convert-cache-init!

         bytes-open-converter/cached-to
         bytes-open-converter/cached-to2
         bytes-open-converter/cached-from
         bytes-close-converter/cached-to
         bytes-close-converter/cached-to2
         bytes-close-converter/cached-from)

(struct cache (enc to to2 from)
  #:mutable
  #:authentic)

(define (new-cache) (cache #f #f #f #f))

(define-place-local local-cache (new-cache))
(define-place-local converter-custodian (unsafe-make-custodian-at-root))

(define (convert-cache-init!)
  (set! local-cache (new-cache))
  (set! converter-custodian (unsafe-make-custodian-at-root)))

(define (cache-clear! get update!)
  (define c (get local-cache))
  (update! local-cache #f)
  (when c
    (bytes-close-converter c)))

(define (cache-lookup! enc get update!)
  (atomically
   (and (equal? enc (cache-enc local-cache))
        (let ([c (get local-cache)])
          (when c
            (update! local-cache #f))
          c))))

(define (cache-save! c enc get update!)
  (when c
    (atomically
     (unless (equal? enc (cache-enc local-cache))
       (cache-clear! cache-to set-cache-to!)
       (cache-clear! cache-to2 set-cache-to2!)
       (cache-clear! cache-from set-cache-from!)
       (set-cache-enc! local-cache enc))
     (cond
       [(get local-cache)
        (bytes-close-converter c)]
       [else
        (bytes-reset-converter c) ; must be in atomic mode
        (update! local-cache c)]))))

;; ----------------------------------------

(define (bytes-open-converter/cached-to enc)
  (or (cache-lookup! enc cache-to set-cache-to!)
      (bytes-open-converter-in-custodian 'bytes-open-converter/cached-to converter-custodian ucs-4-encoding enc)))

(define (bytes-open-converter/cached-to2 enc)
  (or (cache-lookup! enc cache-to2 set-cache-to2!)
      (bytes-open-converter-in-custodian 'bytes-open-converter/cached-to2 converter-custodian ucs-4-encoding enc)))

(define (bytes-open-converter/cached-from enc)
  (or (cache-lookup! enc cache-from set-cache-from!)
      (bytes-open-converter-in-custodian 'bytes-open-converter/cached-from converter-custodian enc "UTF-8")))

(define (bytes-close-converter/cached-to c enc)
  (cache-save! c enc cache-to set-cache-to!))

(define (bytes-close-converter/cached-to2 c enc)
  (cache-save! c enc cache-to2 set-cache-to2!))

(define (bytes-close-converter/cached-from c enc)
  (cache-save! c enc cache-from set-cache-from!))
