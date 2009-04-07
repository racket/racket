#lang scheme/base
(require scheme/class
         "../syntax.ss"
         "snip.ss"
         (only-in "cycle.ss" 
                  set-snip-admin%!
                  popup-menu%)
         "wx.ss")

(provide snip-admin%
         standard-snip-admin%)

(defclass snip-admin% object%
  (super-new)

  (def/public (get-editor) #f)
  (def/public (get-dc) #f)
  (def/public (get-view-size [maybe-box? w] [maybe-box? h])
    #f)

  (def/public (get-view [maybe-box? x] [maybe-box? y] [maybe-box? w] [maybe-box? h] 
                        [(make-or-false snip%) snip])
    #f)

  (def/public (scroll-to [snip% s]
                         [real? x] [real? y]
                         [nonnegative-real? w] [nonnegative-real? h]
                         [any? refresh?]
                         [(symbol-in start end none) [bias 'none]])
    #f)

  (def/public (set-caret-owner [snip% s] [(symbol-in imeditorte display global) dist])
    (void))

  (def/public (resized [snip% s] [any? redraw?]) (void))

  (def/public (recounted [snip% s] [any? redraw?]) (void))

  (def/public (needs-update [snip% s] [real? x] [real? y]
                            [nonnegative-real? w] [nonnegative-real? h])
    (void))

  (def/public (release-snip [snip% s]) #f)

  (def/public (update-cursor) (void))

  (def/public (popup-menu [popup-menu% p][snip% snip][real? x][real? y])
    #f)

  (def/public (modified [snip% s] [any? modified?])
    (void)))

(set-snip-admin%! snip-admin%)

(defclass standard-snip-admin% snip-admin%
  (init-field editor)

  (super-new)

  (def/override (get-editor) editor)
  (def/override (get-dc) (send editor get-dc))
  (def/override (get-view-size [maybe-box? w] [maybe-box? h])
    (get-view #f #f w h #f))

  (def/override (get-view [maybe-box? x] [maybe-box? y] [maybe-box? w] [maybe-box? h] 
                          [(make-or-false snip%) snip])
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
                      (set-box! ok? (send editor get-snip-location snip sl st #f))
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
      (send editor on-snip-modified s modified?))))