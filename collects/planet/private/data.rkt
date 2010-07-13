#lang racket/base

(provide (all-defined-out))

; ==========================================================================================
; DATA
; defines common data used by the PLaneT system
; ==========================================================================================

; exn:i/o:protocol: exception indicating that a protocol error occured
(define-struct (exn:i/o:protocol exn:fail:network) ())

; FULL-PKG-SPEC : struct pkg-spec
(define-struct pkg-spec 
  (name           ; string
   maj            ; (Nat | #f)
   minor-lo       ; (Nat | #f)
   minor-hi       ; (Nat | #f)
   path           ; (listof string)
   stx            ; (syntax | #f)
   core-version   ; string
   )
  #:transparent)
; PKG : string (listof string) Nat Nat path ORIGIN
(define-struct pkg (name route maj min path origin))
; UNINSTALLED-PKG : path FULL-PKG-SPEC Nat Nat
(define-struct uninstalled-pkg (path spec maj min))
; PKG-PROMISE : PKG | UNINSTALLED-PKG
; ORIGIN : 'normal | 'development-link

(define (pkg-promise? p) (or (pkg? p) (uninstalled-pkg? p)))

(define (normally-installed-pkg? p)
  (eq? (pkg-origin p) 'normal))

(define (development-link-pkg? p)
  (eq? (pkg-origin p) 'development-link))
