#lang racket/base
(require racket/class
         "../syntax.rkt"
         racket/snip/private/snip
         racket/snip/private/snip-admin
         (only-in "cycle.rkt"
                  popup-menu%)
         (prefix-in wx: "wx.rkt"))

(provide standard-snip-admin%)

(define TAB-WIDTH 20)


(defclass standard-snip-admin% snip-admin%
  (init-field editor)

  (super-new)

  (def/override (get-editor) editor)
  (def/override (get-dc) (send editor get-dc))
  (def/override (get-view-size [maybe-box? w] [maybe-box? h])
    (get-view #f #f w h #f))

  (def/override (get-view [maybe-box? x] [maybe-box? y] [maybe-box? w] [maybe-box? h] 
                          [(make-or-false snip%) [snip #f]])
    (let ([admin (send editor get-admin)]
          [zeros (lambda ()
                   (when x (set-box! x 0.0))
                   (when y (set-box! y 0.0))
                   (when w (set-box! w 0.0))
                   (when h (set-box! h 0.0)))])
      (if snip
          (if admin
              (let-boxes ([mx 0.0] [my 0.0]
                          [mw 0.0] [mh 0.0])
                  (send admin get-view mx my mw mh #f)
                (let ([mb (+ my mh)]
                      [mr (+ mx mw)])
                  (let-boxes ([ok? #f]
                              [sl 0.0]
                              [st 0.0])
                      (set-box! ok? (if (send editor locked-for-read?)
                                        #f
                                        (send editor get-snip-location snip sl st #f)))
                    (if ok?
                        (let-boxes ([sr 0.0][sb 0.0])
                            (send editor get-snip-location snip sr sb #t)
                          (let ([l (max mx sl)]
                                [t (max my st)]
                                [r (min mr sr)]
                                [b (min mb sb)])
                            (when x (set-box! x (- l sl)))
                            (when y (set-box! y (- t st)))
                            (when w (set-box! w (max 0 (- r l))))
                            (when h (set-box! h (max 0 (- b t))))))
                        (zeros)))))
              (zeros))
          (if admin
              (send admin get-view x y w h #t)
              (zeros)))))

  (def/override (scroll-to [snip% s]
                           [real? localx] [real? localy]
                           [nonnegative-real? w] [nonnegative-real? h]
                           [any? [refresh? #t]]
                           [(symbol-in start end none) [bias 'none]])
    (and (eq? (send s get-admin) this)
         (send editor scroll-to s localx localy w h refresh? bias)))

  (def/override (set-caret-owner [snip% s] [(symbol-in imeditorte display global) dist])
    (when (eq? (send s get-admin) this)
      (send editor set-caret-owner s dist)))

  (def/override (resized [snip% s] [any? redraw?])
    (when (eq? (send s get-admin) this)
      (send editor resized s redraw?)))

  (def/override (recounted [snip% s] [any? redraw?])
    (when (eq? (send s get-admin) this)
      (send editor recounted s redraw?)))

  (def/override (needs-update [snip% s] [real? localx] [real? localy]
                              [nonnegative-real? w] [nonnegative-real? h])
    (when (eq? (send s get-admin) this)
      (send editor needs-update s localx localy w h)))

  (def/override (release-snip [snip% s])
    (and (eq? (send s get-admin) this)
         (send editor release-snip s)))

  (def/override (update-cursor)
    (let ([admin (send editor get-admin)])
      (when admin
        (send admin update-cursor))))

  (def/override (popup-menu [popup-menu% m][snip% snip][real? x][real? y])
    (let ([admin (send editor get-admin)])
      (and admin
           (let-boxes ([sl 0.0]
                       [st 0.0]
                       [ok? #f])
               (set-box! ok? (send editor get-snip-location snip sl st #f))
             (and ok?
                  (send admin popup-menu m (+ x sl) (+ y st)))))))

  (def/override (modified [snip% s] [any? modified?])
    (when (eq? (send s get-admin) this)
      (send editor on-snip-modified s modified?)))
  
  (def/override (get-line-spacing)
    (if (object-method-arity-includes? editor 'get-line-spacing 0)
        (send editor get-line-spacing)
        0))
  
  (def/override (get-tabs [maybe-box? [length #f]] [maybe-box? [tab-width #f]] [maybe-box? [in-units #f]])
    (if (object-method-arity-includes? editor 'get-tabs 3)
        (send editor get-tabs length tab-width in-units)
        (begin (when length (set-box! length 0))
               (when tab-width (set-box! tab-width TAB-WIDTH))
               (when in-units (set-box! in-units #t))
               null)))
  
  (def/override (get-selected-text-color)
    (wx:get-highlight-text-color))
  
  (def/override (call-with-busy-cursor [procedure? thunk])
    (dynamic-wind
     wx:begin-busy-cursor
     thunk
     wx:end-busy-cursor))
  )
