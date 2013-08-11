#lang racket/unit
  (require mzlib/class
           drracket/private/drsig
           framework)
  
  (import)
  (export drracket:text^)
  (define text<%>
    (interface (racket:text<%>)
      printing-on
      printing-off
      is-printing-on?))
  
  (define text%
    (class* racket:text% (text<%>)
      (define printing? #f)
      (define/public (is-printing-on?) printing?)
      (define/public (printing-on) (set! printing? #t))
      (define/public (printing-off) (set! printing? #f))
      ; (rename [super-on-paint on-paint])
      ; (inherit get-filename)
      ; (override
      ;  [on-paint
      ;   (λ (before? dc left top right bottom dx dy draw-caret)
      ;     (super-on-paint before? dc left top right bottom dx dy draw-caret)
      ;     (let ([str (string-append
      ;                  (mzlib:date:date->string (seconds->date (current-seconds)))
      ;                  " "
      ;                  (if (string? (get-filename))
      ;                      (get-filename)
      ;                      "Untitled"))])
      ;        (send dc draw-text str dx dy)))])
      (super-new)))
