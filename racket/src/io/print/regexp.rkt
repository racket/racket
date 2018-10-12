#lang racket/base

(provide printable-regexp?
         set-printable-regexp?!)

(define printable-regexp? (lambda (v) #f))
(define (set-printable-regexp?! proc) (set! printable-regexp? proc))
