#lang racket/base

(require ffi/unsafe)

(define libesd (ffi-lib "libesd"))

;; Use this type to properly close the esd channel
(define-struct esd (num))
(define _esd
  (make-ctype _int esd-num
    (lambda (e)
      (if (and (integer? e) (<= 0 e))
        (let ([new (make-esd e)])
          (register-finalizer new esd-close)
          new)
        (error '_esd "expecting an integer >=0, got: ~e" e)))))

;; Use this type to free collected samples
(define-struct sample (num))
(define _sample
  (make-ctype _int sample-num
    (lambda (s)
      (if (and (integer? s) (<= 0 s))
        (let ([new (make-sample s)])
          (register-finalizer
           new
           (lambda (x)
             (esd-sample-free x)))
          new)
        (error '_sample "expecting an integer >=0, got: ~e" s)))))
;; similar but no finalizer
(define _sample* (make-ctype _int sample-num make-sample))

(provide esd-open-sound)
(define esd-open-sound ; -> esd
  (let ([f (get-ffi-obj "esd_open_sound" libesd (_fun _string -> _esd))])
    (lambda host? (f (and (pair? host?) (car host?))))))

(define (with-default ffi)
  (lambda args
    (if (and (pair? args) (esd? (car args)))
      (apply ffi args)
      (apply ffi (default-esd) args))))

(define (c-name x)
  (regexp-replaces x '((#rx"-" "_") (#rx"[*?]$" ""))))

(define-syntax defesd
  (syntax-rules (: _esd)
    [(_ name : [_esd] type ...)
     (define name
       (with-default
        (get-ffi-obj (c-name 'name) libesd (_fun _esd type ...))))]
    [(_ name : type ...)
     (define name
       (get-ffi-obj (c-name 'name) libesd (_fun type ...)))]))

(define-syntax defesd*
  (syntax-rules ()
    [(_ name x ...) (begin (provide name) (defesd name x ...))]))

(defesd  esd-close        : [_esd] -> _int)
(defesd* esd-send-auth    : [_esd] -> _int)
(defesd* esd-lock         : [_esd] -> _int)
(defesd* esd-unlock       : [_esd] -> _int)
(defesd* esd-standby      : [_esd] -> _int)
(defesd* esd-resume       : [_esd] -> _int)
(defesd* esd-get-latency  : [_esd] -> _int)
(defesd* esd-play-file : (prefix : _string) _file (fallback? : _bool) -> _int)
(defesd* esd-file-cache   : [_esd] (prefix : _string) _file -> _sample)
(defesd* esd-sample-getid : [_esd] _string -> _sample*)
(defesd  esd-sample-free  : [_esd] _sample -> _int)
(defesd* esd-sample-play  : [_esd] _sample -> _int)
(defesd* esd-sample-loop  : [_esd] _sample -> _int)
(defesd* esd-sample-stop  : [_esd] _sample -> _int)
(defesd* esd-sample-kill  : [_esd] _sample -> _int)
(provide default-esd)
(define default-esd (make-parameter (esd-open-sound) esd?))
