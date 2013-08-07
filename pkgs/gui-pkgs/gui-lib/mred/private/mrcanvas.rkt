#lang racket/base

(require racket/class
         racket/list
         (prefix-in wx: "kernel.rkt")
         "lock.rkt"
         "const.rkt"
         "helper.rkt"
         "check.rkt"
         "wx.rkt"
         "wxcanvas.rkt"
         "mrwindow.rkt"
         "mrcontainer.rkt"
         "mrtop.rkt")

(provide canvas<%>
         canvas%
         editor-canvas%)

(define canvas-default-size 20) ; a default size for canvases tht fits borders without losing client sizes
(define canvas-scroll-size 10)
(define canvas-control-border-extra
  (case (system-type)
    [(windows) 2]
    [else 0]))

(define canvas<%>
  (interface (subwindow<%>)
    min-client-width min-client-height
    on-char on-event on-paint on-tab-in
    get-dc
    set-canvas-background get-canvas-background
    set-resize-corner))

(define basic-canvas%
  (class* (make-subwindow% (make-window% #f (make-subarea% area%))) (canvas<%>)
    (init mk-wx mismatches parent)
    (define/public (on-char e) (send wx do-on-char e))
    (define/public (on-event e) (send wx do-on-event e))
    (define/public (on-paint) (when wx (send wx do-on-paint)))
    (define/public (on-tab-in) (void))
      
    (define min-client-width (param (lambda () wx) min-client-width))
    (define min-client-height (param (lambda () wx) min-client-height))
    (public min-client-width min-client-height)

    (define get-dc (entry-point (lambda () (send wx get-dc))))
    (public get-dc)
    (define/public (make-bitmap w h)
      (unless (exact-positive-integer? w)
        (raise-argument-error (who->name '(method canvas% make-bitmap))
                              "exact-positive-integer?"
                              w))
      (unless (exact-positive-integer? h)
        (raise-argument-error (who->name '(method canvas% make-bitmap))
                              "exact-positive-integer?"
                              h))
      (send wx make-compatible-bitmap w h))

    (define/public (suspend-flush)
      (send wx begin-refresh-sequence))
    (define/public (resume-flush)
      (send wx end-refresh-sequence))
    (define/public (flush) (send wx flush))

    (define set-canvas-background
      (entry-point
       (lambda (c)
         (unless (c . is-a? . wx:color%)
           (raise-argument-error (who->name '(method canvas<%> set-canvas-background))
                                 "(is-a?/c color%)"
                                 c))
         (unless (send wx get-canvas-background)
           (raise-arguments-error (who->name '(method canvas<%> set-canvas-background))
                                  "cannot set a transparent canvas's background color"
                                  "given color" c))
         (send wx set-canvas-background c))))
    (public set-canvas-background)
    (define get-canvas-background
      (entry-point
       (lambda ()
         (send wx get-canvas-background))))
    (public get-canvas-background)

    (define/public (set-resize-corner on?)
      (send wx set-resize-corner on?))
    (define wx #f)
    (as-entry
     (lambda ()
       (super-make-object (lambda () (set! wx (mk-wx)) wx) (lambda () wx) (lambda () wx) mismatches #f parent #f)))))

(define default-paint-cb (lambda (canvas dc) (void)))

(define canvas%
  (class basic-canvas%
    (init parent [style null] [paint-callback default-paint-cb] [label #f] [gl-config #f]
          ;; inherited inits
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define paint-cb paint-callback)
    (define has-x? (and (list? style) (memq 'hscroll style)))
    (define has-y? (and (list? style) (memq 'vscroll style)))
    (inherit get-client-size get-dc set-label 
             suspend-flush resume-flush flush
             get-canvas-background)
    (let ([cwho '(constructor canvas)])
      (check-container-parent cwho parent)
      (check-style cwho #f '(border hscroll vscroll gl deleted control-border combo no-autoclear 
                                    transparent resize-corner no-focus)
                   style)
      (check-callback cwho paint-callback)
      (check-label-string/false cwho label))
    (define/public (on-scroll e) (send wx do-on-scroll e))
    (define/public (swap-gl-buffers)
      (let ([ctx (send (send wx get-dc) get-gl-context)])
        (when ctx
          (send ctx swap-buffers))))
    (define/public (with-gl-context
                    thunk
                    #:fail [fail (lambda () 
                                   (error (who->name '(method canvas% with-gl-context))
                                          "no gl context available"))])
      (let ([ctx (send (send wx get-dc) get-gl-context)])
        (if ctx
            (send ctx call-as-current thunk)
            (fail))))
    (define accept-tab-focus
      (entry-point
       (case-lambda
         [() (send wx get-tab-focus)]
         [(on?) (send wx set-tab-focus (and on? #t))])))
    (public accept-tab-focus)
    (define get-virtual-size
      (entry-point
       (lambda () (double-boxed
                   0 0
                   (lambda (x y) (send wx get-virtual-size x y))))))
    (public get-virtual-size)
    (define get-view-start
      (entry-point
       (lambda () (double-boxed
                   0 0
                   (lambda (x y) (send wx view-start x y))))))
    (public get-view-start)

    (define scroll
      (entry-point (lambda (x y) 
                     (when x (check-fraction '(method canvas% scroll) x))
                     (when y (check-fraction '(method canvas% scroll) y))
                     (send wx scroll (or x -1) (or y -1)))))
    (public scroll)

    (define/public (init-auto-scrollbars w h x y)
      (when w (check-gauge-integer '(method canvas% init-auto-scrollbars) w))
      (when h (check-gauge-integer '(method canvas% init-auto-scrollbars) h))
      (check-fraction '(method canvas% init-auto-scrollbars) x)
      (check-fraction '(method canvas% init-auto-scrollbars) y)
      (let-values ([(cw ch) (get-client-size)])
        (send wx set-scrollbars (if w 1 0) (if h 1 0)
              (or w 0) (or h 0) 1 1
              (if w (inexact->exact (floor (* x (max 0 (- w cw))))) 0)
              (if h (inexact->exact (floor (* y (max 0 (- h ch))))) 0)
              #t)))
      
    (define/public (init-manual-scrollbars x-len y-len x-page y-page x-val y-val)
      (let ([who '(method canvas% init-auto-scrollbars)])
        (when x-len (check-gauge-integer who x-len))
        (when y-len (check-gauge-integer who y-len))
        (check-gauge-integer who x-page)
        (check-gauge-integer who y-page)
        (check-gauge-integer who x-val)
        (check-gauge-integer who y-val)
        (when (and x-len (< x-len x-val))
          (raise-arguments-error (who->name who)
                                 "horizontal value is larger than the horizontal range"
                                 "value" x-val
                                 "range" x-len))
        (when (and y-len (< y-len y-val))
          (raise-arguments-error (who->name who)
                                 "vertical value is larger than the vertical range"
                                 "value" y-val
                                 "range" y-len)))
      (send wx set-scrollbars (if x-len 1 0) (if y-len 1 0)
            (or x-len 0) (or y-len 0) x-page y-page x-val y-val #f))

    (define/public (show-scrollbars x-on? y-on?)
      (let ([bad (lambda (which what)
                   (raise-arguments-error 
                    (who->name '(method canvas% show-scrollbars))
                    (format
                     "cannot show ~a scrollbars;\n the canvas style did not include ~a"
                     which
                     what)
                    "canvas" this))])
        (when x-on? (unless has-x? (bad "horizontal" 'hscroll)))
        (when y-on? (unless has-y? (bad "vertical" 'vscroll)))
        (send wx show-scrollbars x-on? y-on?)))

    (define/private (check-scroll name d v must-positive?)
      (unless (or (eq? d 'horizontal) (eq? d 'vertical))
        (raise-argument-error (who->name `(method canvas% ,name)) "(or/c 'horizontal 'vertical)" d))
      (let ([bottom (if must-positive? 1 0)])
        (unless (<= bottom v GAUGE-MAX)
          ((check-bounded-integer bottom GAUGE-MAX #f) `(method canvas% ,name) v))))

    (define get-scroll-pos (entry-point (lambda (d) (check-scroll 'get-scroll-pos d 1 #f) (send wx get-scroll-pos d))))
    (define set-scroll-pos (entry-point (lambda (d v) (check-scroll 'set-scroll-pos d v #f) (send wx set-scroll-pos d v))))
    (define get-scroll-range (entry-point (lambda (d) (check-scroll 'get-scroll-range d 1 #f) (send wx get-scroll-range d))))
    (define set-scroll-range (entry-point (lambda (d v) (check-scroll 'set-scroll-range d v #f) (send wx set-scroll-range d v))))
    (define get-scroll-page (entry-point (lambda (d) (check-scroll 'get-scroll-page d 1 #t) (send wx get-scroll-page d))))
    (define set-scroll-page (entry-point (lambda (d v) (check-scroll 'set-scroll-page d v #t) (send wx set-scroll-page d v))))
    (public get-scroll-pos set-scroll-pos
            get-scroll-range set-scroll-range
            get-scroll-page set-scroll-page)
    (define/override (on-paint)
      (if (eq? paint-cb default-paint-cb)
          (super on-paint)
          (paint-cb this (get-dc))))
    (define no-clear? (memq 'no-autoclear style))
    (define/public (refresh-now [do-paint (lambda (dc) (on-paint))]
                                #:flush? [flush? #t])
      (let ([dc (get-dc)])
        (dynamic-wind
          (lambda ()
            (suspend-flush))
          (lambda ()
            (unless no-clear?
              (let ([bg (get-canvas-background)])
                (if bg
                    (let ([old-bg (send dc get-background)])
                      (as-entry
                       (lambda ()
                         (send dc set-background bg)
                         (send dc clear)
                         (send dc set-background old-bg))))
                    (send dc erase))))
            (do-paint dc))
          (lambda ()
            (resume-flush)))
        (when flush? (flush))))
    (define wx #f)
    (super-new
     [mk-wx
      (lambda ()
        (let ([ds (+ (cond
                      [(memq 'control-border style) (+ 4 canvas-control-border-extra)]
                      [(memq 'border style) 4]
                      [else 0])
                     (if (or has-x? has-y?)
                         canvas-default-size
                         1))])
          (set! wx (make-object wx-canvas% this this
                                (mred->wx-container parent)
                                -1 -1
                                (+ ds (if (memq 'combo style) side-combo-width 0)) ds
                                style
                                gl-config)))
        wx)]
     [mismatches
      (lambda ()
        (let ([cwho '(constructor canvas)])
          (check-container-ready cwho parent)))]
     [parent parent]
     [enabled enabled]
     [horiz-margin horiz-margin]
     [vert-margin vert-margin]
     [min-width min-width]
     [min-height min-height]
     [stretchable-width stretchable-width]
     [stretchable-height stretchable-height])
    (when label
      (set-label label))
    (send parent after-new-child this)))

(define editor-canvas%
  (class basic-canvas%
    (init parent [editor #f] [style null] [scrolls-per-page 100] [label #f]
          [wheel-step no-val] [line-count no-val]
          [horizontal-inset 5] [vertical-inset 5]
          ;; inherited inits
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (let ([cwho '(constructor editor-canvas)])
      (check-container-parent cwho parent)
      (check-instance cwho internal-editor<%> "text% or pasteboard%" #t editor)
      (check-style cwho #f '(hide-vscroll hide-hscroll no-vscroll no-hscroll auto-vscroll auto-hscroll
                                          deleted control-border combo transparent no-border resize-corner
                                          no-focus)
                   style)
      (check-gauge-integer cwho scrolls-per-page)
      (check-label-string/false cwho label)
      (unless (eq? wheel-step no-val)
        (check-wheel-step cwho wheel-step))
      (unless (or (not line-count) (eq? line-count no-val))
        ((check-bounded-integer 1 1000 #t) cwho line-count))
      (unless (eq? horizontal-inset 5)
        (check-margin-integer cwho horizontal-inset))
      (unless (eq? vertical-inset 5)
        (check-margin-integer cwho vertical-inset)))
    (inherit set-label)
    (define force-focus? #f)
    (define scroll-to-last? #f)
    (define scroll-bottom? #f)
    (define/public (call-as-primary-owner f) (send wx call-as-primary-owner f))
    (define allow-scroll-to-last
      (entry-point 
       (case-lambda
         [() scroll-to-last?]
         [(on?) (set! scroll-to-last? (and on? #t))
          (send wx allow-scroll-to-last on?)])))
    (public allow-scroll-to-last)
    (define scroll-with-bottom-base
      (entry-point
       (case-lambda
         [() scroll-bottom?]
         [(on?) (set! scroll-bottom? (and on? #t))
          (send wx scroll-with-bottom-base on?)])))
    (public scroll-with-bottom-base)
    (define lazy-refresh
      (entry-point
       (case-lambda
         [() (send wx get-lazy-refresh)]
         [(on?) (send wx set-lazy-refresh on?)])))
    (public lazy-refresh)
    (define force-display-focus
      (entry-point
       (case-lambda
         [() force-focus?]
         [(on?) (set! force-focus? (and on? #t))
          (send wx force-display-focus on?)])))
    (public force-display-focus)
  
    (define accept-tab-focus
      (entry-point
       (case-lambda
         [() (send wx get-tab-focus)]
         [(on?) (send wx set-tab-focus (and on? #t))])))
    (public accept-tab-focus)
    (define allow-tab-exit
      (entry-point
       (case-lambda
         [() (send wx is-tabable?)]
         [(on?) (send wx set-tabable (and on? #t))])))
    (public allow-tab-exit)
  
    (define set-line-count
      (entry-point
       (lambda (n)
         ((check-bounded-integer 1 1000 #t) '(method editor-canvas% set-line-count) n)
         (send wx set-line-count n))))
    (public set-line-count)
    (define get-line-count
      (entry-point
       (lambda ()
         (send wx get-line-count))))
    (public get-line-count)
  
    (define scroll-to
      (case-lambda 
        [(x y w h refresh?) (send wx scroll-to x y w h refresh?)]
        [(x y w h refresh? bias) (send wx scroll-to x y w h refresh? bias)]))
    (public scroll-to)
  
    (define get-editor (entry-point (lambda () (send wx get-editor))))
    (define set-editor (entry-point 
                        (case-lambda 
                          [(m) (send wx set-editor m)]
                          [(m upd?) (send wx set-editor m upd?)])))
    (public get-editor set-editor)
    (define ws
      (case-lambda 
        [() (let ([v (send wx get-wheel-step)])
              (if (zero? v) #f v))]
        [(wheel-step)
         (check-wheel-step '(method editor-canvas% wheel-step) wheel-step)
         (send wx set-wheel-step (or wheel-step 0))]))
    (public [ws wheel-step])
    (define vi
      (entry-point
       (case-lambda
         [() (send wx get-y-margin)]
         [(m) 
          (check-margin-integer '(method editor-canvas% vertical-inset) m)
          (as-exit (lambda () (send wx set-y-margin m)))])))
    (public [vi vertical-inset])
    (define hi
      (entry-point
       (case-lambda
         [() (send wx get-x-margin)]
         [(m) 
          (check-margin-integer '(method editor-canvas% horizontal-inset) m)
          (as-exit (lambda () (send wx set-x-margin m)))])))
    (public [hi horizontal-inset]) 
    (define wx #f)
    (super-new
     [mk-wx
      (lambda ()
        (let* ([no-h? (or (memq 'no-vscroll style)
                          (memq 'hide-vscroll style))]
               [no-v? (or (memq 'no-hscroll style)
                          (memq 'hide-hscroll style))]
               [get-ds (lambda (no-this? no-other?)
                         (+ (if (memq 'control-border style)
                                canvas-control-border-extra
                                0)
                            (cond
                             [(and no-this? no-other?) 14]
                             [no-this? canvas-default-size]
                             [else (+ canvas-scroll-size canvas-default-size)])))])
          (set! wx (make-object wx-editor-canvas% this this
                                (mred->wx-container parent) -1 -1
                                (+ (get-ds no-h? no-v?) (if (memq 'combo style) side-combo-width 0))
                                (get-ds no-v? no-h?)
                                #f
                                (append
                                 (if (memq 'no-border style)
                                     null
                                     '(border))
                                 (remq 'no-border style))
                                scrolls-per-page #f))
          wx))]
     [mismatches
      (lambda ()
        (let ([cwho '(constructor editor-canvas)])
          (check-container-ready cwho parent)))]
     [parent parent]
     [enabled enabled]
     [horiz-margin horiz-margin]
     [vert-margin vert-margin]
     [min-width min-width]
     [min-height min-height]
     [stretchable-width stretchable-width]
     [stretchable-height stretchable-height])
    (unless (eq? wheel-step no-val)
      (ws wheel-step))
    (when label
      (set-label label))
    (when editor
      (set-editor editor))
    (send parent after-new-child this)
    (unless (or (not line-count) (eq? line-count no-val))
      (set-line-count line-count))
    (unless (or (eq? vertical-inset 5))
      (vi vertical-inset))
    (unless (or (eq? horizontal-inset 5))
      (hi horizontal-inset))))
