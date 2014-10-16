#lang racket/base
(require racket/class
         racket/snip
         racket/draw
         racket/format
         (prefix-in racket: racket/base)
         pict
         wxme)

(provide snipclass
         reader
         pict-snip%)

(define pict-snip%
  (class snip%
    (init-field pict)
    (define drawer (make-pict-drawer pict))
    (define/override (get-extent dc x y wb hb db sb lb rb)
      (set-box/f! wb (pict-width pict))
      (set-box/f! hb (pict-height pict))
      (set-box/f! db (pict-descent pict))
      (set-box/f! sb (pict-ascent pict))
      (set-box/f! lb 0)
      (set-box/f! rb 0))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define smoothing (send dc get-smoothing))
      (send dc set-smoothing 'aligned)
      (drawer dc x y)
      (send dc set-smoothing smoothing))
    (define/override (copy)
      (new pict-snip% [pict pict]))
    
    (define/override (write f)
      (define dc (new record-dc% [width (pict-width pict)] [height (pict-height pict)]))
      (send dc set-smoothing 'aligned)
      (drawer dc 0 0)
      (define bp (open-output-bytes))
      (racket:write (list (send dc get-recorded-datum)
                          (pict-width pict)
                          (pict-height pict)
                          (pict-descent pict)
                          (pict-ascent pict))
                    bp)
      (define bts (get-output-bytes bp))
      (send f put (bytes-length bts) bts))
    
    (define/public (get-pict) pict)
    
    (inherit set-snipclass)
    (super-new)
    (set-snipclass snipclass)))

(define snipclass 
  (new (class snip-class%
         (define/override (read f)
           (raw-data->snip (send f get-unterminated-bytes)))
         (super-new))))
(send snipclass set-classname (~s '((lib "snip.rkt" "pict") (lib "snip-wxme.rkt" "pict"))))
(send snipclass set-version 1)
(define (set-box/f! b v) (when (box? b) (set-box! b v)))
(send (get-the-snip-class-list) add snipclass)

(define reader
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream) 
      (cond
        [text-only?
         #"."]
        [else
         (raw-data->snip (send stream read-raw-bytes 'pict-snip%))]))
    (super-new)))

(define (raw-data->snip bts)
  (define p (open-input-bytes bts))
  (define saved (read p))
  (define-values (recorded-datum w h d a) (apply values saved))
  (define drawer (recorded-datum->procedure recorded-datum))
  (new pict-snip%
       [pict (dc (λ (dc dx dy)
                   (define-values (ox oy) (send dc get-origin))
                   (send dc set-origin (+ ox dx) (+ oy dy))
                   (drawer dc)
                   (send dc set-origin ox oy))
                 w h d a)]))

(module+ test
  (require racket/gui/base
           racket/file
           racket/port
           rackunit)
  
  (define tmp-file (make-temporary-file "pict-snip-test-case~a.rkt"))

  (dynamic-wind
   void
   (λ ()
     
     ;; make sure that saving the file in graphical mode works
     (define t1 (new text%))
     (define disk-size 10)
     (define a-pict (disk disk-size))
     (send t1 insert (new pict-snip% [pict a-pict]))
     (unless (send t1 save-file tmp-file 'standard)
       (error 'snip.rkt "'standard didn't save properly"))
     (define t2
       (parameterize ([current-namespace (make-gui-namespace)])
         (eval `(let ()
                  (define t2 (new text%))
                  (send t2 load-file ,tmp-file 'standard)
                  t2))))
     (define pict2 (send (send t2 find-first-snip) get-pict))
     (define bytes1 (pict->argb-pixels a-pict))
     (define bytes2 (pict->argb-pixels pict2))
     (check-equal? bytes1 bytes2)
     
     ;; make sure that saving the file in text mode works
     (unless (send t1 save-file tmp-file 'text) 
       (error 'snip.rkt "'text didn't save properly"))
     (define sp (open-output-string))
     (call-with-input-file tmp-file (λ (port) (copy-port port sp)))
     (check-equal? (get-output-string sp) "."))
   (λ () (delete-file tmp-file))))
