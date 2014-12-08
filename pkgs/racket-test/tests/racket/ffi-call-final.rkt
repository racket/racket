#lang racket/base

;; Check for a good effort at error reporting on an attempt to
;; use a foreign function that is finalized already.

(define src
  '(module m racket/base
     (require ffi/unsafe)
     (for ([i 10])
       (for ([i 10])
         (define m (get-ffi-obj 'fabs #f (_fun _double -> _double)))
         ;; Since `m' is accessible only via the finalized value, it
         ;; can be finalized before `(list m)':
         (register-finalizer (list m) (lambda (p) ((car p) 10.0))))
       (collect-garbage))))

(define l (make-logger))
(define r (make-log-receiver l 'error))

(parameterize ([current-namespace (make-base-namespace)]
               [current-logger l])
  (eval src)
  (namespace-require ''m))

;; Print logged errors, of which there are likely to be
;; some (although it's not guaranteed) if the finalizer
;; thread is logging correctly:
(let loop ()
  (define m (sync/timeout 0 r))
  (when m
    (printf "~s\n" m)
    (loop)))
