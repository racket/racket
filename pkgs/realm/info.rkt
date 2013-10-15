#lang info

(define collection "realm")

(define name "Realm of Racket Source Code")

(define compile-omit-paths
  '(
    "chapter5/ufo-source.rkt"
;    "chapter6"
;    "chapter8"
;    "chapter10"
;    "chapter12"
;    "chapter13"
;    "chapter14"
    ))

; (define scribblings '(("htdp.scrbl")))
(define deps '("base"
               "htdp-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "Sample code for _Realm of Racket_")

(define pkg-authors '(matthias))
