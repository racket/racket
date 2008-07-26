#lang scheme/base
(require scheme/gui/base
         scheme/class)

(provide first-line-text-mixin)

(define (first-line-text-mixin text%)
  (class text%
    (inherit get-text paragraph-end-position get-admin invalidate-bitmap-cache position-location)
    (define bx (box 0))
    (define by (box 0))
    (define bw (box 0))
    (define fancy-first-line? #t)
    
    (define/override (scroll-to snip localx localy width height refresh? [bias 'none])
      (printf "~s\n" (list 'scroll-to snip localx localy width height refresh? bias))
      (super scroll-to snip localx localy width height refresh? bias))
    
    (define/public (highlight-first-line on?)
      (set! fancy-first-line? on?)
      (invalidate-bitmap-cache)
      (send (send this get-canvas) refresh))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (when fancy-first-line? 
          (let ([admin (get-admin)])
            (when admin
              (send admin get-view bx by bw #f #f)
              (unless (= (unbox by) 0)
                (let ([first-line (get-text 0 (paragraph-end-position 0))]
                      [old-pen (send dc get-pen)]
                      [old-brush (send dc get-brush)]
                      [old-smoothing (send dc get-smoothing)]
                      [old-α (send dc get-alpha)]
                      [old-font (send dc get-font)])
                  (send dc set-font (get-font))
                  (send dc set-smoothing 'aligned)
                  (let-values ([(tw th _1 _2) (send dc get-text-extent first-line)])
                    (let ([line-height (+ (unbox by) dy th 1)]
                          [line-left (+ (unbox bx) dx)]
                          [line-right (+ (unbox bx) dx (unbox bw))])
                      
                      (send dc set-pen "black" 1 'solid)
                      (send dc draw-line line-left line-height line-right line-height)
                      
                      (when (eq? (send dc get-smoothing) 'aligned)
                        (send dc set-pen "black" 1 'solid)
                        (let loop ([i 10])
                          (unless (zero? i)
                            (let ([g (+ 200 (* i 5))])
                              (send dc set-alpha (+ 1/5 (* i -1/50)))
                              (send dc draw-line 
                                    line-left
                                    (+ line-height i)
                                    line-right
                                    (+ line-height i)))
                            (loop (- i 1))))))
                    
                    (send dc set-alpha 1)
                    (send dc set-pen "gray" 1 'transparent)
                    (send dc set-brush "white" 'solid)
                    (send dc draw-rectangle 
                          (+ (unbox bx) dx)
                          (+ (unbox by) dy)
                          (unbox bw)
                          th)
                    (send dc draw-text first-line (+ (unbox bx) dx) (+ (unbox by) dy)))
                  
                  (send dc set-font old-font)
                  (send dc set-pen old-pen)
                  (send dc set-brush old-brush)
                  (send dc set-alpha old-α)
                  (send dc set-smoothing old-smoothing)))))))
      (super on-paint before? dc left top right bottom dx dy draw-caret))
    
    (inherit get-style-list)
    (define/private (get-font)
      (let* ([style-list (get-style-list)]
             [std (or (send style-list find-named-style "Standard")
                      (send style-list find-named-style "Basic"))])
        (send std get-font)))
    
    (super-new)))

(begin
  (define f (new frame% [label ""] [width 200] [height 200]))
  (define t (new (first-line-text-mixin text%)))
  (send t insert (apply string-append (map (λ (x) (build-string 100 (λ (i) (if (= i 99) #\newline x))))
                                           (string->list "abcdefghijklnopqrstuvwxyz"))))
  (define c (new editor-canvas% [parent f] [editor t]))
  (define b (new button% [callback (λ (c dc) (send t highlight-first-line #t))] [label "button"] [parent f]))
  (send f show #t))