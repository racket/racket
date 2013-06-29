
#lang racket/base
(require racket/unit
         mred/mred-sig
         setup/plt-installer-sig
         net/tcp-sig
         net/url-sig
         net/url-unit
         "browser-sig.rkt"
         "private/bullet.rkt"
         "private/html.rkt"
         "private/hyper.rkt"
         "private/sig.rkt")

(provide browser@)

(define-unit-from-context bullet@ bullet-export^)

(define-compound-unit/infer pre-browser@
  (import setup:plt-installer^
          mred^
          url^)
  (export hyper^ html-export^ bullet-export^)
  (link html@ hyper@ bullet@))

(define-unit/new-import-export browser@
  (import setup:plt-installer^
          mred^
          url^)
  (export browser^)
  ((hyper^ html-export^ bullet-export^) 
   pre-browser@
   setup:plt-installer^
   mred^
   url^))