#lang scheme
(provide (all-defined-out))

(define collector:deref false)
(define collector:alloc-flat false)
(define collector:cons false)
(define collector:first false)
(define collector:rest false)
(define collector:flat? false)
(define collector:cons? false)
(define collector:set-first! false)
(define collector:set-rest! false)

(define (set-collector:deref! proc)
  (set! collector:deref proc))

(define (set-collector:alloc-flat! proc)
  (set! collector:alloc-flat proc))

(define (set-collector:cons! proc)
  (set! collector:cons proc))

(define (set-collector:first! proc)
  (set! collector:first proc))

(define (set-collector:rest! proc)
  (set! collector:rest proc))

(define (set-collector:flat?! proc)
  (set! collector:flat? proc))

(define (set-collector:cons?! proc)
  (set! collector:cons? proc))

(define (set-collector:set-first!! proc)
  (set! collector:set-first! proc))

(define (set-collector:set-rest!! proc)
  (set! collector:set-rest! proc))
