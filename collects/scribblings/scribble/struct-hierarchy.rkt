#lang racket/base

(require "class-diagrams.rkt"
         (only-in pict pin-arrow-line)
         texpict/mrpict
         (except-in texpict/utils pin-arrow-line)
         racket/system
         racket/class
         racket/draw)

(define (mk-ps-diagram)
  ;; thicken up the lines for postscript
  (linewidth .8 (mk-diagram)))

(provide mk-diagram)

(define (mk-diagram)
  
  (define part-name (class-name "part" #:spacing-word "subparts"))
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
  
  (define string-name (class-name "string"))
  (define string-box (class-box string-name #f #f))

  (define symbol-name (class-name "symbol"))
  (define symbol-box (class-box symbol-name #f #f))

  (define pict-name (class-name "pict"))
  (define pict-box (class-box pict-name #f #f))

  (define convertible-name (class-name "convertible"))
  (define convertible-box (class-box convertible-name #f #f))
  
  (define list-name (class-name "list"))
  (define list-box (class-box list-name #f #f))
  
  (define delayed-element-name (class-name "delayed-\nelement"))
  (define delayed-element-content (field-spec #f "content"))
  (define delayed-element-box (class-box delayed-element-name (list delayed-element-content) #f))
  
  (define render-element-name (class-name "render-\nelement"))
  (define render-element-content (field-spec #f "content"))
  (define render-element-box (class-box render-element-name (list render-element-content) #f))
  
  (define traverse-element-name (class-name "traverse-\nelement"))
  (define traverse-element-content (field-spec #f "content"))
  (define traverse-element-box (class-box traverse-element-name (list traverse-element-content) #f))
  
  (define part-relative-element-name (class-name "part-\nrelative-\nelement"))
  (define part-relative-element-resolve (field-spec #f "resolve"))
  (define part-relative-element-box (class-box part-relative-element-name (list part-relative-element-resolve) #f))
  
  (define element-name (class-name "element"))
  (define element-style (field-spec #f "style"))
  (define element-content (field-spec #f "content"))
  (define element-box (class-box element-name (list element-style element-content) #f))
  
  (define link-element-name (class-name "link-\nelement"))
  (define link-tag (field-spec #f "tag"))
  (define link-element-box (class-box link-element-name
                                      (list link-tag)
                                      #f))
  
  (define collect-element-name (class-name "collect-\nelement"))
  (define collect-element-collect (field-spec #f "collect"))
  (define collect-element-box (class-box collect-element-name (list collect-element-collect) #f))
  
  (define index-element-name (class-name "index-\nelement" #:spacing-word "keywords"))
  (define index-element-tag (field-spec #f "tag"))
  (define index-element-keywords (field-spec #f "keywords"))
  (define index-element-box (class-box index-element-name
                                       (list index-element-tag index-element-keywords)
                                       #f))
  
  (define image-element-name (class-name "image-\nelement" #:spacing-word "suffixes"))
  (define image-element-path (field-spec #f "path"))
  (define image-element-suffixes (field-spec #f "suffixes"))
  (define image-element-scale (field-spec #f "scale"))
  (define image-element-box (class-box image-element-name 
                                       (list image-element-path
                                             image-element-suffixes
                                             image-element-scale)
                                       #f))
  
  (define multiarg-element-name (class-name "multiarg-\nelement"))
  (define multiarg-element-tag (field-spec #f "tag"))
  (define multiarg-element-box (class-box multiarg-element-name (list multiarg-element-tag) #f))

  (define target-element-name (class-name "target-\nelement"))
  (define target-tag (field-spec #f "tag"))
  (define target-element-box (class-box target-element-name 
                                        (list target-tag)
                                        #f))

  (define redirect-target-element-name (class-name "redirect-target-\nelement"))
  (define redirect-target-alt-path (field-spec #f "alt-path"))
  (define redirect-target-alt-anchor (field-spec #f "alt-anchor"))
  (define redirect-target-element-box (class-box redirect-target-element-name 
                                                 (list redirect-target-alt-path redirect-target-alt-anchor)
                                                 #f))
  
  (define toc-target-element-name (class-name "toc-target-\nelement"))
  (define toc-target-element-box (class-box toc-target-element-name (list) #f))
  
  (define page-target-element-name (class-name "page-target-\nelement"))
  (define page-target-element-box (class-box page-target-element-name (list) #f))
  
  
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

  (define target-element-hierarchy
    (hierarchy
     (vc-append target-element-box
                (blank 0 50)
                (ht-append 20 
                           toc-target-element-box
                           page-target-element-box
                           redirect-target-element-box))
     (list target-element-box)
     (list toc-target-element-box
           page-target-element-box
           redirect-target-element-box)))
  
  (define element-hierarchy
    (hierarchy
     (vc-append element-box
                (blank 0 50)
                (inset (ht-append 20 
                                  collect-element-box
                                  multiarg-element-box
                                  (refocus target-element-hierarchy target-element-box)
                                  link-element-box
                                  image-element-box
                                  index-element-box)
                       0 0 -400 0))
     (list element-box)
     (list collect-element-box
           index-element-box
           image-element-box
           target-element-box
           multiarg-element-box
           link-element-box
           )))

    (define render-element-parent-link (blank))
  (define delayed-element-parent-link (blank))
  (define part-relative-element-parent-link (blank))
  (define traverse-element-parent-link (blank))
  (define element-parent-link (blank))
  
  (define (drop-and-link box parent-link i)
    (vc-append
     (blank 0 (+ 40 (* i 20)))
     (refocus (ct-superimpose box parent-link)
              parent-link)))
  
  (define content-hierarchy
    (hierarchy
     (vc-append content-box
                (blank 0 50)
                (ht-append 15
                           (drop-and-link (refocus element-hierarchy element-box)
                                          element-parent-link
                                          4)
                           convertible-box
                           (drop-and-link render-element-box 
                                          render-element-parent-link
                                          4)
                           pict-box
                           (drop-and-link delayed-element-box
                                          delayed-element-parent-link
                                          3)
                           symbol-box
                           (drop-and-link part-relative-element-box
                                          part-relative-element-parent-link
                                          1)
                           string-box
                           (drop-and-link traverse-element-box
                                          traverse-element-parent-link
                                          0)
                           list-box))
     (list content-box)
     (list element-box
           string-box
           symbol-box
           convertible-box
           pict-box
           traverse-element-parent-link
           part-relative-element-parent-link
           delayed-element-parent-link
           render-element-parent-link
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
             (left-left-reference 
              raw
              element-box element-content content-box content-name 1 #:dot-delta -1)
             part-box part-title-field content-box content-name 21)
            part-box part-blocks-field block-box block-name)
           part-box part-subparts-field part-box part-name 2)
          para-box para-content content-box content-name 2)
         compound-para-box compound-para-blocks block-box block-name 3)
        table-box table-cells block-box block-name 2)
       itemization-box itemization-items block-box block-name 10)
      nested-flow-box nested-flow-blocks block-box block-name 1)
     list-box list-box content-box content-name))
  
  (define w/delayed-connections 
    (dotted-right-right-reference
     (dotted-right-right-reference
      (dotted-right-right-reference
       (dotted-right-right-reference
        (dotted-right-right-reference
         (dotted-right-right-reference
          w/connections
          render-element-box render-element-content content-box content-name 30)
         traverse-block-box traverse-block-block block-box block-name 1)
        delayed-block-box delayed-block-block block-box block-name 17)
       traverse-element-box traverse-element-content content-box content-name 3)
      delayed-element-box delayed-element-content content-box content-name 22)
     part-relative-element-box part-relative-element-resolve content-box content-name 12))
  
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
                       type-link-color ;(send pen get-color)
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
                      #:color type-link-color)
      base)))

(define (dotted-right-right-reference p0 a b c d [count 1])
  (right-right-reference p0 a b c d count #:connect-dots connect-circly-dots))

(module+ slideshow
  (require slideshow)
  (define p (inset (mk-diagram) 0 0 0 1))
  (define c (blank client-w client-h))
  (slide (lt-superimpose (t "top") (clip (refocus (ct-superimpose p c) c))))
  (slide (lt-superimpose (t "bottom") (clip (refocus (cb-superimpose p c) c))))
  (slide (lt-superimpose (t "all")
                         (ct-superimpose 
                          c
                          (scale p
                                 (min (/ client-w (pict-width p))
                                      (/ client-h (pict-height p))))))))
