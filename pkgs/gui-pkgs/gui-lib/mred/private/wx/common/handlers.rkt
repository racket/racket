#lang racket/base

(provide
 (protect-out application-file-handler
              application-start-empty-handler
              application-quit-handler
              application-about-handler
              application-pref-handler
              
              nothing-application-pref-handler
              nothing-application-about-handler))

(define saved-files null)
(define orig-afh (lambda (f)
                   (if (eq? afh orig-afh)
                       (set! saved-files (cons f saved-files))
                       ;; handler has changed between time a call here
                       ;;  was queued and the call happened
                       (let ([sf (cons f saved-files)])
                         (set! saved-files null)
                         (for-each afh (reverse sf))))))
(define afh orig-afh)
(define application-file-handler
  (case-lambda
   [(proc) 
    (set! afh proc)
    (let ([sf saved-files])
      (set! saved-files null)
      (for-each proc (reverse sf)))]
   [() afh]))

(define started-empty? #f)
(define orig-aseh (lambda ()
                    (if (eq? aseh orig-aseh)
                        (set! started-empty? #t)
                        (aseh))))
(define aseh orig-aseh)
(define application-start-empty-handler
  (case-lambda
   [(proc) 
    (set! aseh proc)
    (when started-empty?
      (set! started-empty? #f)
      (proc))]
   [() aseh]))

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
