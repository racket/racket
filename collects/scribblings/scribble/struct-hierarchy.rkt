#lang racket/base

(require "class-diagrams.rkt"
         texpict/mrpict
         texpict/utils
         racket/system
         racket/class
         racket/draw)

(define (mk-ps-diagram)
  ;; thicken up the lines for postscript
  (linewidth .8 (mk-diagram)))

(provide mk-diagram)

(define (mk-diagram)
  
  (define part-name (class-name "part"))
  (define part-blocks-field (field-spec #f "blocks"))
  (define part-subparts-field (field-spec #f "subparts"))
  (define part-title-field (field-spec #f "title"))
  (define part-box (class-box part-name (list part-title-field part-blocks-field part-subparts-field) #f))
  
  (define block-name (class-name "block"))
  (define block-box (class-box block-name #f #f))
  
  (define para-name (class-name "paragraph"))
  (define para-style (field-spec #f "style"))
  (define para-content (field-spec #f "content"))
  (define para-box (class-box para-name (list para-style para-content) #f))
  
  (define compound-para-name (class-name "compound-\nparagraph"))
  (define compound-para-style (field-spec #f "style"))
  (define compound-para-blocks (field-spec #f "blocks"))
  (define compound-para-box (class-box compound-para-name (list compound-para-style compound-para-blocks) #f))
  
  (define table-name (class-name "table"))
  (define table-style (field-spec #f "style"))
  (define table-cells (field-spec #f "cells")) ;; blockss
  (define table-box (class-box table-name (list table-style table-cells) #f))
  
  (define itemization-name (class-name "itemization"))
  (define itemization-style (field-spec #f "style"))
  (define itemization-items (field-spec #f "items")) ;; blockss
  (define itemization-box (class-box itemization-name (list itemization-style itemization-items) #f))
  
  (define nested-flow-name (class-name "nested-\nflow"))
  (define nested-flow-style (field-spec #f "style"))
  (define nested-flow-blocks (field-spec #f "blocks"))
  (define nested-flow-box (class-box nested-flow-name (list nested-flow-style nested-flow-blocks) #f))
  
  (define delayed-block-name (class-name "delayed-block"))
  (define delayed-block-block (field-spec #f "block"))
  (define delayed-block-box (class-box delayed-block-name (list delayed-block-block) #f))
  
  (define traverse-block-name (class-name "traverse-\nblock"))
  (define traverse-block-block (field-spec #f "block"))
  (define traverse-block-box (class-box traverse-block-name (list traverse-block-block) #f))
  
  (define content-name (class-name "content"))
  (define content-box (class-box content-name #f #f))
  
  (define element-name (class-name "element"))
  (define element-style (field-spec #f "style"))
  (define element-box (class-box element-name (list element-style) #f))
  
  (define string-name (class-name "string"))
  (define string-box (class-box string-name #f #f))

  (define symbol-name (class-name "symbol"))
  (define symbol-box (class-box symbol-name #f #f))

  (define pict-name (class-name "pict"))
  (define pict-box (class-box pict-name #f #f))

  (define list-name (class-name "list"))
  (define list-box (class-box list-name #f #f))
  
  (define target-element-name (class-name "target-\nelement"))
  (define target-tag (field-spec #f "tag"))
  (define target-content (field-spec #f "content"))
  (define target-element-box (class-box target-element-name 
                                        (list target-tag target-content)
                                        #f))
  
  (define link-element-name (class-name "link-\nelement"))
  (define link-tag (field-spec #f "tag"))
  (define link-content (field-spec #f "content"))
  (define link-element-box (class-box link-element-name
                                      (list link-tag link-content)
                                      #f))
  
  (define delayed-element-name (class-name "delayed-\nelement"))
  (define delayed-content (field-spec #f "content"))
  (define delayed-element-box (class-box delayed-element-name (list delayed-content) #f))
  
  (define render-element-name (class-name "render-\nelement"))
  (define render-content (field-spec #f "content"))
  (define render-element-box (class-box render-element-name (list render-content) #f))
  
  (define traverse-element-name (class-name "traverse-\nelement"))
  (define traverse-content (field-spec #f "content"))
  (define traverse-element-box (class-box traverse-element-name (list traverse-content) #f))
  
  (define collect-element-name (class-name "collect-\nelement"))
  (define collect-content (field-spec #f "content"))
  (define collect-element-box (class-box collect-element-name (list collect-content) #f))
  
  (define index-element-name (class-name "index-\nelement"))
  (define index-element-tag (field-spec #f "tag"))
  (define index-element-keywords (field-spec #f "keywords"))
  (define index-element-content (field-spec #f "content"))
  (define index-element-box (class-box index-element-name
                                       (list index-element-tag index-element-keywords index-element-content)
                                       #f))
  
  (define image-element-name (class-name "image-\nelement"))
  (define image-element-box (class-box image-element-name (list) #f))
  
  (define multi-arg-element-name (class-name "multi-arg-\nelement"))
  (define multi-arg-element-tag (field-spec #f "tag"))
  (define multi-arg-element-content (field-spec #f "content"))
  (define multi-arg-element-box (class-box multi-arg-element-name (list multi-arg-element-tag multi-arg-element-content) #f))

  (define part-relative-element-name (class-name "part-relative-\nelement"))
  (define part-relative-element-resolve (field-spec #f "resolve"))
  (define part-relative-element-box (class-box part-relative-element-name (list part-relative-element-resolve) #f))
  
  (define block-hierarchy
    (hierarchy
     (vc-append block-box
                (blank 0 50)
                (ht-append 20 
                           (ht-append 30
                                      compound-para-box
                                      para-box)
                           (vc-append (blank 0 30) itemization-box)
                           table-box)
                (blank 0 25)
                (ht-append nested-flow-box 
                           (blank 120 0)
                           (vc-append (blank 0 30) delayed-block-box)
                           (blank 80 0)
                           traverse-block-box))
     (list block-box)
     (list compound-para-box
           para-box
           nested-flow-box
           itemization-box
           table-box
           delayed-block-box
           traverse-block-box)))

  (define target-element-parent-link (blank))
  (define render-element-parent-link (blank))
  (define delayed-element-parent-link (blank))
  (define part-relative-element-parent-link (blank))
  (define traverse-element-parent-link (blank))
  
  (define element-hierarchy
    (hierarchy
     (vc-append element-box
                (blank 0 50)
                (ht-append 20 
                           (hc-append collect-element-box (blank 30 0))
                           (vc-append (blank 0 10) multi-arg-element-box)
                           (vc-append (blank 0 20) index-element-box)
                           (vc-append (blank 0 10) image-element-box)
                           link-element-box)
                (blank 0 20)
                (ht-append 10
                           (rt-superimpose target-element-box 
                                           (ht-append target-element-parent-link
                                                      (blank 8 0)))
                           (lt-superimpose render-element-box 
                                           (ht-append (blank 8 0)
                                                      render-element-parent-link))
                           (blank 250 0))
                (ht-append 10
                           (blank 130 0)
                           (vc-append (blank 0 60) 
                                      (rt-superimpose delayed-element-box
                                                      (ht-append delayed-element-parent-link
                                                                 (blank 15 0))))
                           (vc-append (blank 0 30) 
                                      (ct-superimpose part-relative-element-box
                                                      (ht-append (blank 20 0) 
                                                                 part-relative-element-parent-link)))
                           (ct-superimpose traverse-element-box
                                           (ht-append traverse-element-parent-link
                                                      (blank 30 0)))))
     (list element-box)
     (list collect-element-box
           index-element-box
           image-element-box
           target-element-parent-link
           multi-arg-element-box
           link-element-box
           delayed-element-parent-link
           traverse-element-parent-link
           part-relative-element-parent-link
           render-element-parent-link
           link-element-box)))

  (define content-hierarchy
    (hierarchy
     (vc-append content-box
                (blank 0 50)
                (ht-append (ht-append 20
                                      string-box
                                      symbol-box)
                           (inset element-hierarchy -130 0)
                           (ht-append 20
                                      pict-box
                                      list-box)))
     (list content-box)
     (list string-box
           symbol-box
           pict-box
           element-box
           list-box)))
    
  (define raw
    (vc-append part-box
               (blank 0 20)
               (vc-append block-hierarchy
                          (blank 0 20)
                          content-hierarchy)))
  
  (define w/connections
    (double
     right-right-reference
     (double
      left-left-reference
      (double
       left-left-reference
       (double
        left-left-reference
        (double
         right-right-reference
         (double
          left-left-reference
          (double
           left-left-reference
           (triple
            right-right-reference
            (triple
             right-right-reference
             (double
              left-left-reference
              (double
               left-left-reference
               (double
                right-right-reference 
                (double
                 left-left-reference 
                 (double
                  left-left-reference 
                  raw
                  part-box part-title-field content-box content-name 21)
                 part-box part-blocks-field block-box block-name)
                part-box part-subparts-field part-box part-name 2)
               para-box para-content content-box content-name 2)
              compound-para-box compound-para-blocks block-box block-name 3)
             table-box table-cells block-box block-name 2)
            itemization-box itemization-items block-box block-name 10)
           nested-flow-box nested-flow-blocks block-box block-name 1)
          target-element-box target-content content-box content-name 8)
         link-element-box link-content content-box content-name)
        multi-arg-element-box multi-arg-element-content content-box content-name 14)
       index-element-box index-element-content content-box content-name 26)
      collect-element-box collect-content content-box content-name 1)
     list-box list-box content-box content-name))
  
  (define w/delayed-connections 
    (dotted-right-right-reference
     (dotted-right-right-reference
      (dotted-right-right-reference
       (dotted-right-right-reference
        (dotted-right-right-reference
         (dotted-right-right-reference
          w/connections
          render-element-box render-content content-box content-name 31)
         traverse-block-box traverse-block-block block-box block-name 1)
        delayed-block-box delayed-block-block block-box block-name 17)
       traverse-element-box traverse-content content-box content-name 5)
      delayed-element-box delayed-content content-box content-name 27)
     part-relative-element-box part-relative-element-resolve content-box content-name 14))
  
  ;; one extra pixel on the right so we get the
  ;; line drawn to the outermost turning point
  (inset (panorama w/delayed-connections) 0 0 1 0))

(define (double f p0 a b c d [count 1])
  (let ([arrows1 (launder (f (ghost p0) a b c d count #:dot-delta 1))]
        [arrows2 (launder (f (ghost p0) a b c d count #:dot-delta -1))])
    (cc-superimpose p0
                    arrows1
                    arrows2)))

(define (triple f p0 a b c d [count 1])
  (let ([arrows (launder (f (ghost p0) a b c d count))]
        [up-arrows (launder (f (ghost p0) a b c d count #:dot-delta 2))]
        [down-arrows (launder (f (ghost p0) a b c d count #:dot-delta -2))])
    (cc-superimpose p0
                    arrows
                    up-arrows
                    down-arrows)))

(define (connect-circly-dots show-arrowhead? main dot1 . dots)
  (let loop ([prev-dot dot1]
             [dots dots]
             [pict main])
    (cond
      [(null? dots) pict]
      [else 
       (loop (car dots) 
             (cdr dots)
             (connect-two-circly-dots pict prev-dot (car dots) (null? (cdr dots))))])))  

;; this is a hack -- it will only work with right-right-reference
(define (connect-two-circly-dots pict dot1 dot2 arrowhead?)
  (let ([base
         (let*-values ([(sx sy) (cc-find pict dot1)]
                       [(raw-ex ey) (cc-find pict dot2)]
                       [(ex) (if arrowhead?
                                 (+ raw-ex 2)
                                 raw-ex)])
           (cc-superimpose
            (dc 
             (λ (dc dx dy)
               (let ([pen (send dc get-pen)])
                 (send dc set-pen
                       (send pen get-color)
                       (if (is-a? dc post-script-dc%)
                           4
                           2)
                       'dot)
                 (send dc draw-line 
                       (+ dx sx) (+ dy sy)
                       (+ dx ex) (+ dy ey))
                 (send dc set-pen pen)))
             (pict-width pict)
             (pict-height pict))
            pict))])
  (if arrowhead?
      (pin-arrow-line field-arrowhead-size
                      base
                      dot1 (λ (ignored1 ignored2)
                             (let-values ([(x y) (cc-find pict dot2)])
                               (values (+ x 2) y)))
                      dot2 cc-find
                      #f #f #f #f)
      base)))

(define (dotted-right-right-reference p0 a b c d [count 1])
  (right-right-reference p0 a b c d count #:connect-dots connect-circly-dots))

(module+ slideshow
  (require slideshow)
  (define p (mk-diagram))
  (slide (scale p
                (min (/ client-w (pict-width p))
                     (/ client-h (pict-height p))))))
