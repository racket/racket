#lang racket/base
(require pict
         racket/class
         racket/draw)

(provide pict-diagram)

(define pict-diagram
  (parameterize ([dc-for-text-size (make-object bitmap-dc%
                                                (make-bitmap 1 1))])
    (let ([t (lambda (s)
               (text s `(italic . roman) 12))])
      (let ([top
             (hc-append (vline 0 10)
                        (hline 30 0)
                        (inset (t "w") 1 0)
                        (hline 30 0)
                        (vline 0 10))]
            [right
             (vc-append (hline 10 0)
                        (vline 0 25)
                        (inset (t "h") 0 1)
                        (vline 0 25)
                        (hline 10 0))])
        (inset
         (vl-append
          2
          top
          (hc-append
           2
           (frame (let* ([line (hline (pict-width top) 0 #:segment 5)]
                         [top-line (launder line)]
                         [bottom-line (launder line)]
                         [top-edge (launder (ghost line))]
                         [bottom-edge (launder (ghost line))]
                         [p (vc-append
                             (/ (pict-height right) 4)
                             top-edge
                             top-line
                             (blank)
                             bottom-line
                             bottom-edge)]
                         [p (pin-arrows-line
                             4 p
                             top-edge ct-find 
                             top-line ct-find)]
                         [p (pin-arrows-line
                             4 p
                             bottom-edge ct-find 
                             bottom-line ct-find)]
                         [a (t "a")]
                         [p (let-values ([(dx dy) (ct-find p top-line)])
                              (pin-over p (+ dx 5) (/ (- dy (pict-height a)) 2) a))]
                         [d (t "d")]
                         [p (let-values ([(dx dy) (ct-find p bottom-line)])
                              (pin-over p 
                                        (+ dx 5) 
                                        (+ dy (/ (- (- (pict-height p) dy) (pict-height d)) 2))
                                        d))])
                    p))
           right))
         1)))))

