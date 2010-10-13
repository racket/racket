#lang racket/load

;; purpose: make sure that each clause exists at most once 
;; (why am I running this in scheme/load for the namespace in eval)

(error-print-source-location #f)

(with-handlers ((exn:fail:syntax? 
                 (lambda (e) 
                   (define msg (exn-message e))
                   (define ext "big-bang: duplicate on-draw clause") ; " in: (on-draw render2 400 200)")
                   (unless (string=? msg ext)
                     (raise e)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (require 2htdp/image)
           
           (define (render1 n) (text (number->string n) 12 "red"))
           (define (render2 n) (text (number->string n) 10 "blue"))
           
           (define (main a)
             (big-bang 0
                       (on-draw render1 200 400)
                       (on-draw render2 400 200)
                       ; (on-tick sub1)
                       (on-tick add1))))))

(with-handlers ((exn:fail:syntax? 
                 (lambda (e) 
                   (define msg (exn-message e))
                   (unless (string=? msg "universe: duplicate on-tick clause"); " in: (on-tick sub1)")
                     (raise e)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           
           (define (main a)
             (universe 0
                       (on-tick add1)
                       (on-tick sub1))))))

(with-handlers ((exn:fail:syntax? 
                 (lambda (e) 
                   (define msg (exn-message e))
                   (unless (string=? msg "big-bang: duplicate to-draw clause") 
                     (raise e)))))
  (eval '(module a racket
	   (require 2htdp/universe)
	   (define s "")
	   (define x 0)
	   (big-bang 0
	     (on-tick (lambda (w) (begin (set! x (+ x 1)) w)))
	     (to-draw (lambda (w) (set! s (number->string w))))
	     (on-draw (lambda (w) (set! s (number->string w))))) )))
