#lang racket/base
(require racket/fixnum
         "../path/path.rkt"
         "../path/complete.rkt"
         "../path/parameter.rkt"
         "../path/cleanse.rkt"
         "../path/protect.rkt"
         "../path/simplify-nofs.rkt"
         "../host/rktio.rkt"
         "../security/main.rkt")

(provide ->host
         ->host/as-is
         host->
         host-element->)

;; Note: `(host-> (->host x who flags))` is not the same as `x`, since
;; it normalizes `x`. That's why `(host-> (->host x))` is generally
;; used in error reporting.

(define (->host p who guards)
  (let ([p (->path p)])
    (when who
      (security-guard-check-file who p guards))
    (path-bytes
     (handle-long-path
      who
      (cleanse-path/convert-slashes (path->complete-path p current-directory #:wrt-given? #f))))))

(define (->host/as-is p who src)
  (let ([p (->path p)])
    (when who
      (if src
          (security-guard-check-file-link who src p)
          (security-guard-check-file who p '(exists))))
    (path-bytes p)))

(define (host-> s)
  (path (bytes->immutable-bytes s)
        (system-path-convention-type)))

(define (host-element-> s)
  (if (eq? 'windows (system-path-convention-type))
      (path (protect-path-element (bytes->immutable-bytes s) 'windows)
            'windows)
      (host-> s)))

;; If we end up with a Windows path that is longer than 247 bytes
;; (traditional file path limit: 259; directory path limit: 247)
;; then add "\\?\" or "\\?\UNC" to the front. The path needs to be
;; abbsolute and otherwise fully normalized so that just adding to the
;; front is possible.
(define LONGEST-NON-BSBS-PATH 247)
(define (handle-long-path who p)
  (cond
    [(eq? (system-type) 'windows)
     (define bstr (path-bytes p))
     (cond
       [(and ((bytes-length bstr) . > . LONGEST-NON-BSBS-PATH)
             (not (and (fx= (bytes-ref bstr 0) (char->integer #\\))
                       (fx= (bytes-ref bstr 1) (char->integer #\\))
                       (fx= (bytes-ref bstr 2) (char->integer #\?))
                       (fx= (bytes-ref bstr 3) (char->integer #\\)))))
        ;; First, simplify to get rid of directory indicators:
        (define simple-p (simplify-path-syntactically who p #f))
        (define simple-bstr (path-bytes simple-p))
        (cond
          [((bytes-length simple-bstr) . <= . LONGEST-NON-BSBS-PATH)
           ;; Simplified path is short enough
           simple-p]
          [(fx= (bytes-ref simple-bstr 0) (char->integer #\\))
           ;; Must be UNC
           (path (bytes-append #"\\\\?\\UNC" (subbytes simple-bstr 0)) 'windows)]
          [else
           ;; Must be drive letter
           (path (bytes-append #"\\\\?\\" simple-bstr) 'windows)])]
       [else
        ;; Short enough or already a "\\?\" path
        p])]
    [else p]))
