#lang racket/gui
#|

This is an attempt to simulate what happens when
DrRacket redraws a full window's worth of text

|#

(require framework)

(define t%
  (text:column-guide-mixin
   (text:line-numbers-mixin
    racket:text%)))

(define (time-print-mixin t%)
  (class t%
    (super-new)
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (cond
        [before?
         (set! start-time (current-process-milliseconds (current-thread)))
         (super on-paint before? dc left top right bottom dx dy draw-caret)]
        [else
         (super on-paint before? dc left top right bottom dx dy draw-caret)
         (define now (current-process-milliseconds (current-thread)))
         (set! times (cons (- now start-time) times))
         (semaphore-post s)]))))
(define start-time #f)

(define times '())
(define s (make-semaphore))
(define t (new (time-print-mixin t%)))
(send t show-line-numbers! #t)
(send t insert "#lang racket/base\n")
(for ([x (in-range 1000)])
  (send t insert (format "~s\n" '(let loop ([x ""])
                                   (when (< (string-length x) 100)
                                     (loop (string-append x "y")))))))
(define f (new frame% [label ""] [width 1000] [height 1400]))
(define ec (new editor-canvas% [parent f] [editor t]))
(define height
  (let ([yb (box 0)])
    (send t position-location (send t last-position) #f yb)
    (unbox yb)))
(define width
  (let ([xb (box 0)])
    (for/fold ([width 0]) ([para (in-range (+ (send t last-paragraph) 1))])
      (send t position-location (send t paragraph-end-position para) xb #f #t #t)
      (max width (unbox xb)))))
(void (send ec scroll-to 0 (/ height 2) 1 1 #t))
(send f show #t)

;; wait for syntax coloring to finish
(send t freeze-colorer)

(define number-of-experiments 10)

(queue-callback
 (λ ()
   (set! times '())
   (set! s (make-semaphore)) ;; because earlier paints happend before we were ready
   (for ([i (in-range number-of-experiments)])
     (collect-garbage) (collect-garbage) (collect-garbage)
     (send ec refresh)
     (yield s))
   (semaphore-post done))
 #f)

(define done (make-semaphore 0))
(void (yield done))

;; show the actual times.
times

;; print in drdr friendly way
(let ([t (apply + times)])
  (printf "cpu time: ~a real time: ~a gc time: ~a\n" t t t))

(send f show #f) (exit)
