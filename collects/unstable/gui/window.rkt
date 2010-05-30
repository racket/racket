#lang racket/gui

(provide
 locked-text-field-mixin
 locked-text-field%
 locked-combo-field%
 union-container-mixin
 union-pane%
 union-panel%)

;; ======================================================================
;;
;;  LOCKED TEXT FIELD CLASS / MIXIN
;;
;; ======================================================================

(define locked-text-field-mixin
  (mixin [(class->interface text-field%)] []

    (inherit get-editor)

    (define/override (set-value str)
      (send (get-editor) lock #f)
      (super set-value str)
      (send (get-editor) lock #t))

    (super-new)

    (init [undo-history 0])

    (send (get-editor) lock #t)
    (send (get-editor) set-max-undo-history undo-history)))

(define locked-text-field%
  (locked-text-field-mixin text-field%))

(define locked-combo-field%
  (locked-text-field-mixin combo-field%))


;; ======================================================================
;;
;;  UNION PANEL CLASS / MIXIN
;;
;; ======================================================================

(define union-container-mixin
  (mixin [area-container<%>] []

    (super-new)

    (inherit get-children get-alignment)

    (define/public (choose child)
      (for ([child* (get-children)])
        (send child* show (eq? child* child))))

    (define/override (container-size info)
      (match info
        [(list (list w h _ _) ...)
         (values (apply max 0 w)
                 (apply max 0 h))]))

    (define/override (place-children info w0 h0)
      (let*-values ([(ha va) (get-alignment)]
                    [(hp) (horiz->place ha)]
                    [(vp) (vert->place va)])
        (map (lambda (child) (place-child hp vp w0 h0 child)) info)))

    (define/private (place-child hp vp w0 h0 child)
      (match child
        [(list cw ch sw sh)
         (let*-values ([(x w) (place-dim hp w0 cw sw)]
                       [(y h) (place-dim vp h0 ch sh)])
           (list x y w h))]))

    (define/private (place-dim p maximum minimum stretch?)
      (match (list p stretch?)
        [(list _ #t) (values 0 maximum)]
        [(list 'min #f) (values 0 minimum)]
        [(list 'mid #f) (values (floor (/ (- maximum minimum) 2)) minimum)]
        [(list 'max #f) (values (- maximum minimum) minimum)]))

    (define/private horiz->place
      (match-lambda ['left 'min] ['center 'mid] ['right 'max]))

    (define/private vert->place
      (match-lambda ['top 'min] ['center 'mid] ['bottom 'max]))))

(define union-pane% (union-container-mixin pane%))
(define union-panel% (union-container-mixin panel%))

