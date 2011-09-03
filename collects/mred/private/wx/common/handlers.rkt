#lang racket/base

(provide
 (protect-out application-file-handler
              application-quit-handler
              application-about-handler
              application-pref-handler
              
              nothing-application-pref-handler
              nothing-application-about-handler))

(define saved-files null)
(define afh (lambda (f)
              (set! saved-files (cons f saved-files))))
(define application-file-handler
  (case-lambda
   [(proc) 
    (set! afh proc)
    (let ([sf saved-files])
      (set! saved-files null)
      (for-each proc (reverse sf)))]
   [() afh]))

(define aqh void)
(define application-quit-handler
  (case-lambda
   [(proc) (set! aqh proc)]
   [() aqh]))

(define (nothing-application-about-handler) (void))
(define aah nothing-application-about-handler)
(define application-about-handler
  (case-lambda
   [(proc) (set! aah proc)]
   [() aah]))

(define (nothing-application-pref-handler) (void))
(define aph nothing-application-pref-handler)
(define application-pref-handler
  (case-lambda
   [(proc) (set! aph proc)]
   [() aph]))
