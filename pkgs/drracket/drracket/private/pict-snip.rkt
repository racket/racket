#lang racket/base
(require racket/snip
         racket/class
         racket/match
         racket/draw
         file/convertible
         wxme
         (prefix-in r: racket/base))

(provide pict-snip% snip-class reader)

(define convertible<%>
  (interface* () ([prop:convertible (lambda (v r d)
                                      (send v convert r d))])
              convert))

;; this snip is created on the user's space,
;; but its callbacks are invoked on DrRacket's.
(define pict-snip%
  (class* snip% (convertible<%>)
    (init-field w h d a recorded-datum)
    (define/override (get-extent dc x y [wb #f] [hb #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (set-box/f lspace 0)
      (set-box/f rspace 0)
      (set-box/f wb w)
      (set-box/f hb h)
      (set-box/f descent d)
      (set-box/f space a))
    (define proc #f)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (unless proc
        (set! proc (with-handlers ((exn:fail? mk-error-drawer))
                     (recorded-datum->procedure recorded-datum))))
      (define-values (ox oy) (send dc get-origin))
      (send dc set-origin (+ ox x) (+ oy y))
      (proc dc)
      (send dc set-origin ox oy))
    (define/override (copy) (new pict-snip% [w w] [h h] [d d] [a a] 
                                 [recorded-datum recorded-datum]))
    
    (define/override (write f) 
      (define prt (open-output-bytes))
      (r:write (list w h d a recorded-datum) prt)
      (define bytes (get-output-bytes prt))
      (send f put (bytes-length bytes) bytes))
    (super-new)
    (inherit set-snipclass)
    (set-snipclass snip-class)

    (define/public (convert r d)
      (case r
        [(png-bytes)
         (define bm (make-bitmap (inexact->exact (ceiling w))
                                 (inexact->exact (ceiling h))))
         (define dc (send bm make-dc))
         (draw dc 0 0 0 0 w h 0 0 #f)
         (define b (open-output-bytes))
         (send bm save-file b 'png)
         (get-output-bytes b)]
        [(pdf-bytes)
         (define b (open-output-bytes))
         (define dc (new pdf-dc% 
                         [interactive #f]
                         [width w] [height h]
                         [output b]))
         (send dc start-doc "pict")
         (send dc start-page)
         (draw dc 0 0 0 0 w h 0 0 #f)
         (send dc end-page)
         (send dc end-doc)
         (get-output-bytes b)]
        [else d]))))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define ((mk-error-drawer exn) dc)
  (define clr (send dc get-text-foreground))
  (send dc set-text-foreground "red")
  (send dc draw-text (exn-message exn) 0 0)
  (send dc set-text-foreground clr))

(define snip-class 
  (new (class snip-class%
         (define/override (read f)
           (parse-pict-snip-from-bytes
            (send f get-unterminated-bytes)))
         (super-new))))

(send snip-class set-classname (format "~s" (list '(lib "pict-snip.rkt" "drracket" "private")
                                                  '(lib "pict-snip.rkt" "drracket" "private"))))
(send (get-the-snip-class-list) add snip-class)

(define reader
  (new (class* object% (snip-reader<%>)
         (define/public (read-header version stream) (void))
         (define/public (read-snip text-only? version stream)
           (define bytes (send stream read-raw-bytes 'pict-snip))
           (if text-only?
               #"#<pict-snip>"
               (or (parse-pict-snip-from-bytes bytes) 
                   (error 'pict-snip.rkt "could not read pict-snip from stream"))))
         (super-new))))

;; parse-pict-snip-from-bytes : bytes -> (or/c (is-a?/c pict-snip%) #f)
(define (parse-pict-snip-from-bytes bytes)
  (let/ec escape
    (define prt (open-input-bytes bytes))
    (define sexp (with-handlers ((exn:fail:read? (λ (x) (escape #f))))
                   (read prt)))
    (match sexp
      [`(,(? real? w) ,(? real? h) ,(? real? d) ,(? real? a) ,recorded-datum)
       (new pict-snip% [w w] [h h] [d d] [a a] 
            [recorded-datum recorded-datum])]
      [else
       #f])))
