#lang scheme/base

(provide application-file-handler
         application-quit-handler
         application-about-handler
         application-pref-handler)

(define afh void)
(define application-file-handler
  (case-lambda
   [(proc) (set! afh proc)]
   [() afh]))
(define aqh void)
(define application-quit-handler
  (case-lambda
   [(proc) (set! aqh proc)]
   [() aqh]))
(define aah void)
(define application-about-handler
  (case-lambda
   [(proc) (set! aah proc)]
   [() aah]))
(define aph void)
(define application-pref-handler
  (case-lambda
   [(proc) (set! aph proc)]
   [() aph]))

