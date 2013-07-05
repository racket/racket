#lang racket/base
(require framework
         pict
         racket/runtime-path
         racket/gui/base
         (for-syntax racket/base)
         racket/class
         racket/promise
         redex/pict
         redex/reduction-semantics)

(provide btest done show-bitmap-test-gui?)

(define show-bitmap-test-gui? (make-parameter #t))

(define just-save-failures? (getenv "PLTJUSTSAVEFAILURES"))

(define tests 0)
(define failed-tests 0)
(define failed-panels '())
(define (done)
  (if (zero? failed-tests)
      (printf "bitmap-test.rkt: ~a tests, all passed\n" tests)
      (eprintf "bitmap-test.rkt: ~a tests, ~a failed\n" tests failed-tests)))

(define-syntax (btest stx)
  (syntax-case stx ()
    [(_ test-exp bitmap-filename)
     #`(test/proc
        #,(syntax-line stx)
        (λ () test-exp)
        bitmap-filename)]))

(define-runtime-path bmps-dir (format "bmps-~a" (system-type)))

(define (test/proc line-number pict-thunk raw-bitmap-filename)
  (set! tests (+ tests 1))
  (let* ([pict (set-fonts/call pict-thunk)]
         [bitmap-filename 
          (build-path bmps-dir
                      raw-bitmap-filename)]
         [old-bitmap (if (file-exists? bitmap-filename)
                         (read-bitmap bitmap-filename)
                         (let* ([bm (make-bitmap 100 20)]
                                [bdc (make-object bitmap-dc% bm)])
                           (send bdc clear)
                           (send bdc draw-text "does not exist" 0 0)
                           (send bdc set-bitmap #f)
                           bm))]
         [new-bitmap ((if (eq? (system-type) 'unix)
                          make-bitmap
                          make-screen-bitmap)
                       (ceiling (inexact->exact (pict-width pict))) 
                       (ceiling (inexact->exact (pict-height pict))))]
         [bdc (make-object bitmap-dc% new-bitmap)])
    (send bdc clear)
    (draw-pict pict bdc 0 0)
    (send bdc set-bitmap #f)
    (unless (bitmaps-same? old-bitmap new-bitmap)
      (set! failed-tests (+ failed-tests 1))
      (cond
        [just-save-failures? 
         (eprintf "saving ~a\n" bitmap-filename)
         (void (send new-bitmap save-file bitmap-filename 'png))]
        [else
         (when (show-bitmap-test-gui?)
           (let ([failed-panel (make-failed-panel line-number bitmap-filename old-bitmap new-bitmap)])
             (set! failed-panels (append failed-panels (list failed-panel)))))]))))

(define (set-fonts/call thunk)
  (case (system-type)
    [(unix)
     (let ([rewrite-style 
            (λ (s)
              (let loop ([s s])
                (cond
                  [(pair? s) (cons (loop (car s)) (loop (cdr s)))]
                  [(eq? s 'roman) (verify-face "DejaVu Serif")]
                  [(eq? s 'swiss) (verify-face "DejaVu Sans")]
                  [else s])))])
       (parameterize ([label-style (rewrite-style (label-style))]
                      [literal-style (rewrite-style (literal-style))]
                      [metafunction-style (rewrite-style (metafunction-style))]
                      [non-terminal-style (rewrite-style (non-terminal-style))]
                      [non-terminal-subscript-style (rewrite-style (non-terminal-subscript-style))]
                      [non-terminal-superscript-style (rewrite-style (non-terminal-superscript-style))]
                      [default-style (rewrite-style (default-style))])
         (thunk)))]
    [else
     (thunk)]))

(define (verify-face face)
  (unless (member face (get-face-list))
    (error 'verify-face "unknown face: ~s" face))
  face)

(define (bitmaps-same? old-bitmap new-bitmap)
  (let ([w (send old-bitmap get-width)]
        [h (send old-bitmap get-height)])
    (and (= w (send new-bitmap get-width))
         (= h (send new-bitmap get-height))
         (let ([bytes1 (make-bytes (* w h 4))]
               [bytes2 (make-bytes (* w h 4))])
           (send old-bitmap get-argb-pixels 0 0 w h bytes1)
           (send new-bitmap get-argb-pixels 0 0 w h bytes2)
           (equal? bytes1 bytes2)))))

(define test-result-single-panel #f)
(define (get-test-result-single-panel)
  (cond
    [test-result-single-panel
     test-result-single-panel]
    [else
     (let ()
       (define f (new frame% [label "bitmap-test.rkt failures"]))
       (define lined (new vertical-panel% [parent f] [style '(border)]))
       (define sp (new panel:single% [parent lined]))
       (define current-index 0)
       (define hp (new horizontal-panel% [parent f] [alignment '(center center)]))
       (define prev 
         (new button% 
              [label "Prev"] 
              [parent hp]
              [callback
               (λ (x y)
                 (set! current-index (modulo (- current-index 1) (length failed-panels)))
                 (update-gui))]))
       (define next (new button% 
                         [label "Next"] 
                         [parent hp]
                         [callback
                          (λ (x y)
                            (set! current-index (modulo (+ current-index 1) (length failed-panels)))
                            (update-gui))]))
       (define (update-gui) 
         (send sp active-child (list-ref failed-panels current-index)))
       (set! test-result-single-panel sp)
       (send f show #t)
       sp)]))

(define (compute-diffs old-bitmap new-bitmap)
  (define ow (send old-bitmap get-width))
  (define nw (send new-bitmap get-width))
  (define oh (send old-bitmap get-height))
  (define nh (send new-bitmap get-height))
  (define w (max ow nw))
  (define h (max oh nh))
  (define old-bytes (make-bytes (* ow oh 4)))
  (define new-bytes (make-bytes (* nw nh 4)))
  (define diff-bytes (make-bytes (* w h 4) 255))
  (define number-of-different-pixels 0)
  (send old-bitmap get-argb-pixels 0 0 ow oh old-bytes)
  (send new-bitmap get-argb-pixels 0 0 nw nh new-bytes)
  (let loop ([x 0])
    (unless (= x w)
      (let loop ([y 0])
        (unless (= y h)
          (define diff-start (* 4 (+ (* y w) x)))
          (cond
            [(and (< x nw)
                  (< y nh)
                  (< x ow)
                  (< y oh))
             (define old-start (* 4 (+ (* y ow) x)))
             (define new-start (* 4 (+ (* y nw) x)))
             (define a (bytes-ref old-bytes old-start))
             (define r (bytes-ref old-bytes (+ old-start 1)))
             (define g (bytes-ref old-bytes (+ old-start 2)))
             (define b (bytes-ref old-bytes (+ old-start 3)))
             (cond
               [(and (= a (bytes-ref new-bytes new-start))
                     (= r (bytes-ref new-bytes (+ new-start 1)))
                     (= g (bytes-ref new-bytes (+ new-start 2)))
                     (= b (bytes-ref new-bytes (+ new-start 3))))
                (bytes-set! diff-bytes diff-start a)
                (bytes-set! diff-bytes (+ diff-start 1) r)
                (bytes-set! diff-bytes (+ diff-start 2) g)
                (bytes-set! diff-bytes (+ diff-start 3) b)]
               [else
                (set! number-of-different-pixels (+ number-of-different-pixels 1))
                ;; don't need to set diff-start or (+ diff-start 1) since
                ;; the bytes are initialized to 255
                (bytes-set! diff-bytes (+ diff-start 2) 0)
                (bytes-set! diff-bytes (+ diff-start 3) 0)])]
            [else
             (bytes-set! diff-bytes (+ diff-start 2) 0)
             (bytes-set! diff-bytes (+ diff-start 3) 0)])
          (loop (+ y 1))))
      (loop (+ x 1))))
  (define diff-bitmap (make-bitmap w h))
  (send diff-bitmap set-argb-pixels 0 0 w h diff-bytes)
  (values diff-bitmap number-of-different-pixels))

(define (make-failed-panel line-number filename old-bitmap new-bitmap)
  (define diff-bitmap 'unk)
  (define number-of-different-pixels #f)
  (define f (new vertical-panel% [parent (get-test-result-single-panel)]))
  (define msg (new message% [label (format "line ~a" line-number)] [parent f]))
  (define hp (new horizontal-panel% [parent f]))
  (define vp1 (new vertical-panel% [parent hp]))
  (define vp2 (new vertical-panel% [parent hp]))
  (define computing/differences-msg (new message% [label ""] [auto-resize #t] [parent f]))
  (define chk (new check-box% 
                   [label "Show diff"]
                   [parent f]
                   [callback
                    (λ (_1 _2)
                      (define (update-check)
                        (cond
                          [(send chk get-value)
                           (send right-hand set-label (force diff-bitmap))]
                          [else
                           (send right-hand set-label new-bitmap)]))
                      (cond
                        [(eq? diff-bitmap 'unk)
                         (send chk enable #f)
                         (send computing/differences-msg set-label "Computing diff ...")
                         (thread
                          (λ ()
                            (define-values (_diff-bitmap number-of-different-pixels) (compute-diffs old-bitmap new-bitmap))
                            (set! diff-bitmap _diff-bitmap)
                            (queue-callback
                             (λ ()
                               (send computing/differences-msg set-label (format "~a pixels different" number-of-different-pixels))
                               (send chk enable #t)
                               (update-check)))))]
                        [else
                         (update-check)]))]))
  (define btn (new button%
                   [parent f]
                   [label "Save"]
                   [callback
                    (λ (x y)
                      (send new-bitmap save-file filename 'png))]))
  (define left-label (new message% 
                          [parent vp1]
                          [label (format "Old ~ax~a" (send old-bitmap get-width) (send old-bitmap get-height))]))
  (define left-hand (new message%
                         [parent vp1]
                         [label old-bitmap]))
  (define right-label (new message% 
                           [parent vp2]
                           [label (format "New ~ax~a" (send new-bitmap get-width) (send new-bitmap get-height))]))
  (define right-hand (new message%
                          [parent vp2]
                          [label new-bitmap]))
  (send left-hand set-label old-bitmap)
  (send right-hand set-label new-bitmap)
  f)
