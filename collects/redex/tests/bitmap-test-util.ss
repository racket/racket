#lang scheme/gui
(require framework
         slideshow/pict
         "../pict.ss"
         "../reduction-semantics.ss")

(provide test done)

(define tests 0)
(define failed '())
(define (done)
  (printf "~a tests" tests)
  (if (null? failed)
      (printf ", all passed\n")
      (printf ", ~a failed\n" (length failed))))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ test-exp bitmap-filename)
     #`(test/proc
        #,(syntax-line stx)
        (λ () test-exp)
        bitmap-filename)]))

(define (test/proc line-number pict-thunk raw-bitmap-filename)
  (set! tests (+ tests 1))
  (let* ([pict (set-fonts/call pict-thunk)]
         [bitmap-filename 
          (build-path (format "bmps-~a" (system-type))
                      raw-bitmap-filename)]
         [old-bitmap (if (file-exists? bitmap-filename)
                         (make-object bitmap% bitmap-filename)
                         (let* ([bm (make-object bitmap% 100 20)]
                                [bdc (make-object bitmap-dc% bm)])
                           (send bdc clear)
                           (send bdc draw-text "does not exist" 0 0)
                           (send bdc set-bitmap #f)
                           bm))]
         [new-bitmap (make-object bitmap% 
                       (ceiling (inexact->exact (pict-width pict))) 
                       (ceiling (inexact->exact (pict-height pict))))]
         [bdc (make-object bitmap-dc% new-bitmap)])
    (send bdc clear)
    (draw-pict pict bdc 0 0)
    (send bdc set-bitmap #f)
    (let ([diff-bitmap (compute-diffs old-bitmap new-bitmap)])
      (when diff-bitmap
        (let ([failed-panel (make-failed-panel line-number bitmap-filename old-bitmap new-bitmap diff-bitmap)])
          (set! failed (append failed (list failed-panel))))))))

(define (set-fonts/call thunk)
  (case (system-type)
    [(unix)
     (let ([rewrite-style 
            (λ (s)
              (let loop ([s s])
                (cond
                  [(pair? s) (cons (loop (car s)) (loop (cdr s)))]
                  [(eq? s 'roman) (verify-face " DejaVu Serif")]
                  [(eq? s 'swiss) (verify-face " DejaVu Sans")]
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
  
(define (compute-diffs old-bitmap new-bitmap)
  (let* ([w (max (send old-bitmap get-width)
                 (send new-bitmap get-width))]
         [h (max (send old-bitmap get-height)
                 (send new-bitmap get-height))]
         [diff-bitmap (make-object bitmap% w h)]
         [new (make-object bitmap-dc% new-bitmap)]
         [old (make-object bitmap-dc% old-bitmap)]
         [diff (make-object bitmap-dc% diff-bitmap)]
         [new-c (make-object color%)]
         [old-c (make-object color%)]
         [any-different? #f])
    (let loop ([x 0])
      (unless (= x w)
        (let loop ([y 0])
          (unless (= y h)
            (cond
              [(and (<= x (send new-bitmap get-width))
                    (<= y (send new-bitmap get-height))
                    (<= x (send old-bitmap get-width))
                    (<= y (send old-bitmap get-height)))
               (send new get-pixel x y new-c)
               (send old get-pixel x y old-c)
               (cond
                 [(and (= (send new-c red) (send old-c red))
                       (= (send new-c green) (send old-c green))
                       (= (send new-c blue) (send old-c blue)))
                  (send diff set-pixel x y new-c)]
                 [else
                  (set! any-different? #t)
                  (send new-c set 255 0 0)
                  (send diff set-pixel x y new-c)])]
              [else 
               (set! any-different? #t)
               (send new-c set 255 0 0)
               (send diff set-pixel x y new-c)])
            (loop (+ y 1))))
        (loop (+ x 1))))
    (send diff set-bitmap #f)
    (send old set-bitmap #f)
    (send new set-bitmap #f)
    (and any-different? diff-bitmap)))

(define test-result-single-panel #f)
(define (get-test-result-single-panel)
  (cond
    [test-result-single-panel
     test-result-single-panel]
    [else
     (let ()
       (define f (new frame% [label "bitmap-test.ss failures"]))
       (define lined (new vertical-panel% [parent f] [style '(border)]))
       (define sp (new panel:single% [parent lined]))
       (define current-index 0)
       (define hp (new horizontal-panel% [parent f]))
       (define prev 
         (new button% 
              [label "Prev"] 
              [parent hp]
              [callback
               (λ (x y)
                 (set! current-index (modulo (- current-index 1) (length failed)))
                 (update-gui))]))
       (define next (new button% 
                         [label "Next"] 
                         [parent hp]
                         [callback
                          (λ (x y)
                            (set! current-index (modulo (+ current-index 1) (length failed)))
                            (update-gui))]))
       (define (update-gui) 
         (send sp active-child (list-ref failed current-index)))
       (set! test-result-single-panel sp)
       (send f show #t)
       sp)]))

(define (make-failed-panel line-number filename old-bitmap new-bitmap diff-bitmap)
  (define f (new vertical-panel% [parent (get-test-result-single-panel)]))
  (define msg (new message% [label (format "line ~a" line-number)] [parent f]))
  (define hp (new horizontal-panel% [parent f]))
  (define vp1 (new vertical-panel% [parent hp]))
  (define vp2 (new vertical-panel% [parent hp]))
  (define chk (new check-box% 
                   [label "Show diff"]
                   [parent f]
                   [callback
                    (λ (_1 _2)
                      (cond
                        [(send chk get-value)
                         (send right-hand set-label diff-bitmap)]
                        [else
                         (send right-hand set-label new-bitmap)]))]))
  (define btn (new button%
                   [parent f]
                   [label "Save"]
                   [callback
                    (λ (x y)
                      (send new-bitmap save-file filename 'png))]))
  (define left-label (new message% [parent vp1] [label "Old"]))
  (define left-hand (new message%
                         [parent vp1]
                         [label diff-bitmap]))
  (define right-label (new message% [parent vp2] [label "New"]))
  (define right-hand (new message%
                          [parent vp2]
                          [label diff-bitmap]))
  (send left-hand set-label old-bitmap)
  (send right-hand set-label new-bitmap)
  f)
