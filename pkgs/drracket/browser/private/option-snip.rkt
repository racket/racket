#lang racket
(require racket/gui)

(provide option-snip%
         checkbox-snip%)

(define inset 2)
(define arrow-sep 5)
(define arrow-height 5)

(define arrow (list (make-object point% 0 0)
                    (make-object point% arrow-height arrow-height)
                    (make-object point% (* 2 arrow-height) 0)))

(define arrow-cursor (make-object cursor% 'arrow))

(define option-snip%
  (class snip%
    (inherit get-admin set-snipclass set-count get-style get-flags set-flags)
    (init-field [options null])
    (define w #f)
    (define h #f)
    (define d #f)
    (define current-option #f)
    (define look-for-option #f) ; a box when we're looking (in case we're looking for #f)
    
    (define/public (add-option o v)
      (set! options (append options (list (cons o v))))
      (when (and look-for-option
                 (equal? v (unbox look-for-option)))
        (set! current-option (cons o v)))
      (set! w #f)
      (set! h #f)
      (let ([a (get-admin)])
        (when a
          (send a resized this #t))))
    
    (define/public (get-value)
      (with-handlers ([exn:fail? (lambda (x) #f)])
        (cdr (or current-option
                 (car options)))))
    
    (define/public (set-value v)
      (let ([o (ormap (lambda (o) (and (equal? v (cdr o)) o)) options)])
        (if o
            (set! current-option o)
            (set! look-for-option (box v)))))
    
    (override*
     [get-extent  ; called by an editor to get the snip's size
      (lambda (dc x y wbox hbox descentbox spacebox lspacebox rspacebox)
        (unless w
          (let ([font (send (get-style) get-font)])
            (let ([w+h+ds
                   (map (lambda (o)
                          (let-values ([(tw th td ta) (send dc get-text-extent (car o) font)])
                            (list tw th td)))
                        options)])
              (if (null? w+h+ds)
                  (begin
                    (set! w 10)
                    (set! h 10)
                    (set! d 2))
                  (begin
                    (set! w (+ (* 2 inset) arrow-sep 2 (* 2 arrow-height) (apply max (map car w+h+ds))))
                    (set! h (+ (* 2 inset) 1 (apply max arrow-height (map cadr w+h+ds))))
                    (set! d (+ inset 1 (apply max (map caddr w+h+ds)))))))))
        (when hbox
          (set-box! hbox h))
        (when wbox
          (set-box! wbox w))
        (when descentbox
          (set-box! descentbox d))
        (when spacebox
          (set-box! spacebox 0))
        (when rspacebox
          (set-box! rspacebox 0))
        (when lspacebox
          (set-box! lspacebox 0)))]
     [draw  ; called by an editor to draw the snip
      (lambda (dc x y . other)
        (unless w
          (get-extent dc x y #f #f #f #f #f #f))
        (send dc draw-rectangle x y (sub1 w) (sub1 h))
        (send dc draw-line (+ x 1) (+ y h -1) (+ x w -1) (+ y h -1))
        (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -1))
        (let ([pen (send dc get-pen)]
              [brush (send dc get-brush)])
          (send dc set-brush (send the-brush-list find-or-create-brush (send pen get-color) 'solid))
          (send dc draw-polygon arrow 
                (+ x (- w 2 inset (* 2 arrow-height)))
                (+ y (/ (- h arrow-height) 2)))
          (send dc set-brush brush))
        (unless (null? options)
          (send dc draw-text (car (or current-option (car options))) (+ x inset) (+ y inset))))]
     [copy
      (lambda ()
        (make-object option-snip% options))]
     [size-cache-invalid
      (lambda () (set! w #f) (set! h #f))]
     [on-event (lambda (dc x y editorx editory event)
                 (when (send event button-down?)
                   (let ([popup (make-object popup-menu%)])
                     (for-each (lambda (o)
                                 (make-object menu-item% (car o) popup
                                   (lambda (i e)
                                     (set! current-option o)
                                     (let ([a (get-admin)])
                                       (when a
                                         (send a needs-update this 0 0 w h))))))
                               options)
                     (let ([a (get-admin)])
                       (when a
                         (send a popup-menu popup this 0 0))))))]
     [adjust-cursor (lambda (dc x y editorx editory event)
                      arrow-cursor)])
    (super-instantiate ())
    (set-flags (cons 'handles-events (get-flags)))
    (set-count 1)))

(define cb-width 12)
(define cb-height 12)

(define checkbox-snip%
  (class snip%
    (inherit get-admin set-snipclass set-count get-style get-flags set-flags)
    (init-field [checked? #f])
    (define tracking? #f)
    (define hit? #f)
    (define w cb-width)
    (define h cb-height)
    
    (define/public (get-value) checked?)
    
    (override*
     [get-extent  ; called by an editor to get the snip's size
      (lambda (dc x y wbox hbox descentbox spacebox lspacebox rspacebox)
        (when hbox
          (set-box! hbox h))
        (when wbox
          (set-box! wbox w))
        (when descentbox
          (set-box! descentbox 0))
        (when spacebox
          (set-box! spacebox 0))
        (when rspacebox
          (set-box! rspacebox 0))
        (when lspacebox
          (set-box! lspacebox 0)))]
     [draw  ; called by an editor to draw the snip
      (lambda (dc x y . other)
        (send dc draw-rectangle x y w h)
        (when tracking?
          (send dc draw-rectangle (+ x 1) (+ y 1) (- w 2) (- h 2)))
        (when (or (and (not hit?) checked?)
                  (and hit? (not checked?)))
          (send dc draw-line x y (+ x w -1) (+ y h -1))
          (send dc draw-line x (+ y h -1) (+ x w -1) y)))]
     [copy
      (lambda ()
        (make-object checkbox-snip% checked?))]
     [on-event (lambda (dc x y editorx editory event)
                 (when (send event button-down?)
                   (set! tracking? #t)
                   (refresh)
                   (set! hit? #f))
                 (if (or (send event button-down?)
                         (and tracking? (send event dragging?))
                         (and tracking? (send event button-up?)))
                     (if (and (<= 0 (- (send event get-x) x))
                              (<= 0 (- (send event get-y) y)))
                         (when (not hit?)
                           (set! hit? #t)
                           (refresh))
                         (when hit?
                           (set! hit? #f)
                           (refresh)))
                     (when tracking?
                       (set! tracking? #f)
                       (set! hit? #f)
                       (refresh)))
                 (when (and tracking?
                            (and tracking? (send event button-up?)))
                   (set! tracking? #f)
                   (when hit?
                     (set! hit? #f)
                     (set! checked? (not checked?)))
                   (refresh)))]
     [adjust-cursor (lambda (dc x y editorx editory event)
                      arrow-cursor)])
    
    (define/private (refresh)
      (let ([a (get-admin)])
        (when a
          (send a needs-update this 0 0 w h))))
    
    (super-instantiate ())
    (set-flags (cons 'handles-events (get-flags)))
    (set-count 1)))
