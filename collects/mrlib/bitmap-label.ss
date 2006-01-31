(module bitmap-label mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "etc.ss")
           (lib "contract.ss"))

  (provide/contract
   [make-bitmap-label (opt->
                       (string?
                        (or/c path-string?
                               (is-a?/c bitmap%)))
                       ((is-a?/c font%))
                       (is-a?/c bitmap%))]
   [bitmap-label-maker (string?
			(or/c path-string?
			       (is-a?/c bitmap%))
			. -> .
			(any/c . -> . (is-a?/c bitmap%)))])
  
  (define make-bitmap-label
    (opt-lambda (text filename-or-bitmap [font normal-control-font])
      (let*-values ([(outside-margin) 2]
                    [(img-bitmap-dc img-bitmap img-width img-height)
                     (let ([mdc (make-object bitmap-dc%)]
                           [q (if (filename-or-bitmap . is-a? . bitmap%)
                                  filename-or-bitmap
                                  (make-object bitmap% filename-or-bitmap 'unknown/mask))])
                       (if (send q ok?)
                           (begin (send mdc set-bitmap q)
                                  (values mdc
                                          q
                                          (send q get-width)
                                          (send q get-height)))
                           (let ([b (make-object bitmap% 1 1)])
                             (send mdc set-bitmap b)
                             (send mdc clear)
                             (values mdc q 0 0))))]
                    [(width height descent leading)
                     (send img-bitmap-dc get-text-extent text font)]
                    [(middle-margin) (if (and (zero? img-width)
                                              (zero? img-height))
                                         0
                                         3)]
                    [(new-width) (inexact->exact
                                  (floor
                                   (+ outside-margin
                                      img-width
                                      middle-margin
                                      width
                                      outside-margin)))]
                    [(new-height) (inexact->exact
                                   (floor (+ outside-margin
                                             (max img-height height)
                                             outside-margin)))]
                    [(bitmap-dc) (make-object bitmap-dc%)]
                    [(new-bitmap) (make-object bitmap% new-width new-height)]
                    [(new-bitmap-mask) (make-object bitmap% new-width new-height)])
        (send new-bitmap set-loaded-mask new-bitmap-mask)
        (send img-bitmap-dc set-bitmap #f)
        (send bitmap-dc set-bitmap new-bitmap-mask)
        
        (send bitmap-dc set-font font)
        (send bitmap-dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
        (send bitmap-dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
        
        
        (send bitmap-dc clear)
        (send bitmap-dc draw-text text 
              (+ outside-margin img-width middle-margin)
              (- (/ new-height 2) (/ height 2)))
        
        (cond
          [(send img-bitmap get-loaded-mask)
           (send bitmap-dc draw-bitmap
                 (send img-bitmap get-loaded-mask)
                 outside-margin
                 (- (/ new-height 2) (/ img-height 2)))]
          [else
           (send bitmap-dc draw-rectangle 
                 outside-margin
                 (- (/ new-height 2) (/ img-height 2))
                 img-width
                 img-height)])
        
        (send bitmap-dc set-bitmap new-bitmap)
        (send bitmap-dc clear)
        (send bitmap-dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
        (send bitmap-dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
	;; Black rectangle to be masked by text:
        (send bitmap-dc draw-rectangle 
              (sub1 (+ outside-margin img-width middle-margin))
	      0
              (add1 width) new-height)
        (send bitmap-dc draw-bitmap
              img-bitmap
              outside-margin
              (- (/ new-height 2) (/ img-height 2)))
        (send bitmap-dc set-bitmap #f)
        new-bitmap)))
      
  (define (bitmap-label-maker text filename-or-bitmap)
    (let ([bm (make-bitmap-label text filename-or-bitmap)])
      (lambda (area-container-window)
        bm))))
