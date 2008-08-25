#lang scheme/base

(require scheme/gui/base
         scheme/class
         framework)

(provide first-line-text-mixin
         first-line-text-mixin<%>)

(define first-line-text-mixin<%>
  (interface ()
    highlight-first-line))

(define first-line-text-mixin
  (mixin ((class->interface text%)) (first-line-text-mixin<%>)
    (inherit get-text paragraph-end-position get-admin invalidate-bitmap-cache position-location
             scroll-to local-to-global get-dc)
    (define bx (box 0))
    (define by (box 0))
    (define bw (box 0))
    
    (define fancy-first-line? #f)
    
    (define first-line "")
    (define end-of-first-line 0)
    (define first-line-is-lang? #f)
    
    (define/private (show-first-line?)
      (and fancy-first-line? first-line-is-lang?))
    
    (define/private (update-first-line)
      (set! end-of-first-line (paragraph-end-position 0))
      (set! first-line (get-text 0 end-of-first-line))
      (set! first-line-is-lang? (is-lang-line? first-line)))
    
    (define/augment (after-insert start len)
      (when (<= start end-of-first-line)
        (update-first-line)))
    (define/augment (after-delete start len)
      (when (<= start end-of-first-line)
        (update-first-line)))
    
    (define/private (fetch-first-line-height)
      (let-values ([(_1 h _2 _3) (send (get-dc) get-text-extent first-line (get-font))])
        h))
    
    (define/override (scroll-editor-to localx localy width height refresh? bias)
      (let ([admin (get-admin)])
        (cond
          [(not admin)
           #f]
          [(show-first-line?)
           (let ([h (fetch-first-line-height)])
             (set-box! by localy)
             (local-to-global #f by)
             (cond
               [(<= (unbox by) h)
                (send admin scroll-to localx (- localy h) width height refresh? bias)]
               [else
                (send admin scroll-to localx localy width height refresh? bias)]))]
          [else
           (send admin scroll-to localx localy width height refresh? bias)])))
    
    (define/public (highlight-first-line on?)
      (unless (equal? fancy-first-line? on?)
        (set! fancy-first-line? on?)
        (invalidate-bitmap-cache)
        (send (send this get-canvas) refresh)))
    
    (define/override (on-event event)
      (cond
        [(or (send event moving?)
             (send event leaving?)
             (send event entering?))
         (super on-event event)]
        [else
         (let ([y (send event get-y)]
               [h (fetch-first-line-height)]
               [admin (get-admin)])
           (unless admin (send admin get-view #f by #f #f #f))
           (cond
             [(and (< y h)
                   admin
                   (not (= (unbox by) 0)))
              (send admin scroll-to (send event get-x) 0 0 0 #t)
              (super on-event event)]
             [else         
              (super on-event event)]))]))
    
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (when (show-first-line?) 
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
                            (send dc set-alpha (+ 2/5 (* i -1/25)))
                            (send dc draw-line 
                                  line-left
                                  (+ line-height i)
                                  line-right
                                  (+ line-height i))
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

;; is-lang-line? : string -> boolean
;; given the first line in the editor, this returns #t if it is a #lang line.
(define (is-lang-line? l)
  (let ([m (regexp-match #rx"^#(!|(lang ))([-+_/a-zA-Z0-9]+)(.|$)" l)])
    (and m
         (let ([lang-name (list-ref m 3)]
               [last-char (list-ref m 4)])
           (and (not (char=? #\/ (string-ref lang-name 0)))
                (not (char=? #\/ (string-ref lang-name (- (string-length lang-name) 1))))
                (or (string=? "" last-char)
                    (char-whitespace? (string-ref last-char 0))))))))

;; test cases for is-lang-line?
#;
(list (is-lang-line? "#lang x")
      (is-lang-line? "#lang scheme")
      (is-lang-line? "#lang scheme ")
      (not (is-lang-line? "#lang schemeα"))
      (not (is-lang-line? "#lang scheme/ "))
      (not (is-lang-line? "#lang /scheme "))
      (is-lang-line? "#lang sch/eme ")
      (is-lang-line? "#lang r6rs")
      (is-lang-line? "#!r6rs")
      (is-lang-line? "#!r6rs ")
      (not (is-lang-line? "#!/bin/sh")))

#;
(begin
  (define f (new frame% [label ""] [width 200] [height 200]))
  (define t (new (editor:standard-style-list-mixin (first-line-text-mixin text%))))
  (send t insert (apply string-append (map (λ (x) (build-string 100 (λ (i) (if (= i 99) #\newline x))))
                                           (string->list "abcdefghijklnopqrstuvwxyz"))))
  (define c (new editor-canvas% [parent f] [editor t]))
  (define b (new button% [callback (λ (c dc) (send t highlight-first-line #t))] [label "on"] [parent f]))
  (define b2 (new button% [callback (λ (c dc) (send t highlight-first-line #f))] [label "off"] [parent f]))
  (send c focus)
  (send f show #t))