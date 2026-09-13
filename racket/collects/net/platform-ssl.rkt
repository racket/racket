#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path)

;; Unifies osx-ssl.rkt and win32-ssl.rkt, without loading either module
;; on platforms where they wouldn't work.

(define-runtime-module-path-index osx-ssl.rkt "osx-ssl.rkt")
(define-runtime-module-path-index win32-ssl.rkt "win32-ssl.rkt")

(define-syntax (define-predicate-stubs stx)
  (syntax-case stx ()
    [(_ (id . args) ...)
     #'(begin
         (provide id ...)
         (define (id . args) #f) ...)]))

(define-predicate-stubs
  (osx-ssl-output-port? _p)
  (osx-old-openssl?)
  (win32-ssl-port? _p))

(provide win32-ssl-available?)
(define win32-ssl-available? #f)

(define-syntax (define-stubs stx)
  (syntax-case stx ()
    [(_ (id . args) ...)
     #'(begin
         (provide id ...)
         (define (id . args)
           (error 'id "not available"))
         ...)]))

(define-stubs
  (osx-ssl-connect _host _port [_protocol 'auto])
  (osx-ssl-abandon-port _p)
  (win32-ssl-connect _host _port [_protocol 'auto])
  (win32-ssl-abandon-port _p)
  (ports->win32-ssl-ports _i _o
                          #:encrypt [_protocol 'auto]
                          #:hostname [_hostname #f]))

(define-syntax (define-platform-ssl-procedures stx)
  (syntax-case stx ()
    [(_ (platform mod-expr (export-id ...)) ...)
     #'(case (system-type)
         [(platform)
          (let ([mod mod-expr])
            (set! export-id (dynamic-require mod 'export-id)) ...)
          (void)] ...
         [else
          (void)])]))

(define-platform-ssl-procedures
  {macosx osx-ssl.rkt (osx-ssl-connect
                       osx-ssl-abandon-port
                       osx-ssl-output-port?
                       osx-old-openssl?)}
  {windows win32-ssl.rkt (win32-ssl-connect
                          win32-ssl-abandon-port
                          ports->win32-ssl-ports
                          win32-ssl-port?
                          win32-ssl-available?)})
