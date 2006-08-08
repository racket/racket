
(module hrule-snip mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  (provide hrule-snip%)

  ;; hrule-snip%
  ;; A snip for drawing horizontal separating lines.
  (define hrule-snip%
    (class snip%
      (inherit get-admin)
      (define/override (get-extent dc x y bw bh bdescent bspace blspace brspace)
        (let-values [((h) (get-xheight dc))
                     ((fw fh) (send dc get-size))]
          (let ([ad-x (box 0)]
                [ad-y (box 0)])
            (send (get-admin) get-view-size ad-x ad-y)
            #;(set-box?! bw fw)
            (set-box?! bw (unbox ad-x))
            (set-box?! bh h))))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let* [(xh (get-xheight dc))
               (ny (+ y (/ xh 2)))]
          (send dc draw-line x ny right ny)))
      (define/private (set-box?! b v)
        (when (box? b) (set-box! b v)))
      (define/private (get-xheight dc)
        (or cached-xheight
            (let-values [((w h descent extra) (send dc get-text-extent "x"))]
              (set! cached-xheight h)
              h)))
      (define cached-xheight #f)
      (super-new)))
  )
