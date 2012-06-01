#lang racket/base
(require framework
         slideshow/pict
         racket/runtime-path
         racket/gui/base
         (for-syntax racket/base)
         racket/class
         racket/promise
         "../pict.rkt"
         "../reduction-semantics.rkt")

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

(define (compute-diffs old-bitmap new-bitmap)
   (let* ([w (max (send old-bitmap get-width)
                  (send new-bitmap get-width))]
          [h (max (send old-bitmap get-height)
                  (send new-bitmap get-height))]
          [diff-bitmap (make-bitmap w h)] ;; this bitmap holds the diff only, that we compute via set-pixel, not drawing
          [new (make-object bitmap-dc% new-bitmap)]
          [old (make-object bitmap-dc% old-bitmap)]
          [diff (make-object bitmap-dc% diff-bitmap)]
          [new-c (make-object color%)]
          [old-c (make-object color%)])
     (let loop ([x 0])
       (unless (= x w)
         (let loop ([y 0])
           (unless (= y h)
             (cond
               [(and (< x (send new-bitmap get-width))
                     (< y (send new-bitmap get-height))
                     (< x (send old-bitmap get-width))
                     (< y (send old-bitmap get-height)))
                (send new get-pixel x y new-c)
                (send old get-pixel x y old-c)
                (cond
                  [(and (= (send new-c red) (send old-c red))
                        (= (send new-c green) (send old-c green))
                        (= (send new-c blue) (send old-c blue)))
                   (send diff set-pixel x y new-c)]
                  [else
                   (send new-c set 255 0 0)
                   (send diff set-pixel x y new-c)])]
               [else 
                (send new-c set 255 0 0)
                (send diff set-pixel x y new-c)])
             (loop (+ y 1))))
         (loop (+ x 1))))
     (send diff set-bitmap #f)
     (send old set-bitmap #f)
     (send new set-bitmap #f)
     diff-bitmap))

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

(define (make-failed-panel line-number filename old-bitmap new-bitmap)
  (define diff-bitmap 'unk)
  (define f (new vertical-panel% [parent (get-test-result-single-panel)]))
  (define msg (new message% [label (format "line ~a" line-number)] [parent f]))
  (define hp (new horizontal-panel% [parent f]))
  (define vp1 (new vertical-panel% [parent hp]))
  (define vp2 (new vertical-panel% [parent hp]))
  (define computing-label "Computing diff ...")
  (define computing-msg (new message% [label computing-label] [parent f]))
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
                         (send computing-msg set-label computing-label)
                         (thread
                          (λ ()
                            (set! diff-bitmap (compute-diffs old-bitmap new-bitmap))
                            (queue-callback
                             (λ ()
                               (send computing-msg set-label "")
                               (send chk enable #t)
                               (update-check)))))]
                        [else
                         (update-check)]))]))
  (send computing-msg set-label "")
  (define btn (new button%
                   [parent f]
                   [label "Save"]
                   [callback
                    (λ (x y)
                      (send new-bitmap save-file filename 'png))]))
  (define left-label (new message% [parent vp1] [label "Old"]))
  (define left-hand (new message%
                         [parent vp1]
                         [label old-bitmap]))
  (define right-label (new message% [parent vp2] [label "New"]))
  (define right-hand (new message%
                          [parent vp2]
                          [label new-bitmap]))
  (send left-hand set-label old-bitmap)
  (send right-hand set-label new-bitmap)
  f)
