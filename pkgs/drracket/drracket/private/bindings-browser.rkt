#lang racket/base
#|

CODE COPIED (with permission ...) from syntax-browser.rkt
desperately seeking abstraction.

Marshalling (and hence the 'read' method of the snipclass omitted for fast prototyping
                 
|#


  (require racket/pretty
           racket/class
           racket/gui/base
           racket/contract)
  
  (provide render-bindings/snip)

  (define (render-bindings/snip stx) (make-object bindings-snip% stx))
  
  (define bindings-snipclass%
    (class snip-class%
      
      ; not overriding read
      (super-instantiate ())))
  
  (define bindings-snipclass (make-object bindings-snipclass%))
  (send bindings-snipclass set-version 1)
  (send bindings-snipclass set-classname "drscheme:bindings-snipclass%")
  (send (get-the-snip-class-list) add bindings-snipclass)
  
  (define bindings-snip%
    (class editor-snip%
      (init-field bindings)
      
      (unless ((flat-contract-predicate (listof (list/c syntax? any/c))) bindings)
        (error 'bindings-snip% "expected bindings association list, given ~v" bindings))
      
      (define/public (get-bindings) bindings)
      
      (define/override (copy) (make-object bindings-snip% bindings))
      (define/override (write stream)
        (error 'bindings-snip "'write' not implemented for bindings-snip"))
      
      (define output-text (make-object text%))
      (define output-port (make-text-port output-text))
      
      (define/private (make-modern text)
        (send text change-style
              (make-object style-delta% 'change-family 'modern)
              0
              (send text last-position)))
      
      (begin (parameterize ([current-output-port output-port]
                            [pretty-print-columns 30])
               (for-each
                (λ (binding-pair)
                  (let* ([stx (car binding-pair)]
                         [value (cadr binding-pair)])
                    ; this totally destroys the 'output-port' abstraction.  I don't know
                    ; how to enrich the notion of an output-port to get 'bold'ing to 
                    ; work otherwise...
                    (let* ([before (send output-text last-position)])
                      (pretty-print (syntax->datum stx))
                      (let* ([post-newline (send output-text last-position)])
                        (send output-text delete post-newline) ; delete the trailing \n. yuck!
                        (send output-text insert " ")
                        (send output-text change-style 
                              (make-object style-delta% 'change-bold)
                              before (- post-newline 1)))
                      (pretty-print value))))
                bindings))
             (send output-text delete (send output-text last-position))  ; delete final trailing \n
             (make-modern output-text))
    
      (define outer-t (make-object text%))
      
      (super-new
       (editor outer-t)
       (with-border? #f)
       (left-margin 3)
       (top-margin 0)
       (right-margin 0)
       (bottom-margin 0)
       (left-inset 1)
       (top-inset 0)
       (right-inset 0)
       (bottom-inset 0))
      
      (define inner-t (make-object text%))
      (define inner-es (instantiate editor-snip% ()
                         (editor inner-t)
                         (with-border? #f)
                         (left-margin 0)
                         (top-margin 0)
                         (right-margin 0)
                         (bottom-margin 0)
                         (left-inset 0)
                         (top-inset 0)
                         (right-inset 0)
                         (bottom-inset 0)))
      
      (define details-shown? #t)
      
      (inherit show-border set-tight-text-fit)
      (define/private (hide-details)
        (when details-shown?
          (send outer-t lock #f)
          (show-border #f)
          (set-tight-text-fit #t)
          (send outer-t release-snip inner-es)
          (send outer-t delete (send outer-t last-position))
          (send outer-t lock #t)
          (set! details-shown? #f)))
      
      (define/private (show-details)
        (unless details-shown?
          (send outer-t lock #f)
          (show-border #t)
          (set-tight-text-fit #f)
          (send outer-t insert #\newline
                (send outer-t last-position)
                (send outer-t last-position))
          (send outer-t insert inner-es
                (send outer-t last-position)
                (send outer-t last-position))
          (send outer-t lock #t)
          (set! details-shown? #t)))
      
      (send outer-t insert (make-object turn-snip% 
                             (λ () (hide-details))
                             (λ () (show-details))))
      (send outer-t insert (format "bindings\n"))
      (send outer-t insert inner-es)
      (make-modern outer-t)
      
      (send inner-t insert (instantiate editor-snip% ()
                             (editor output-text)
                             (with-border? #f)
                             (left-margin 0)
                             (top-margin 0)
                             (right-margin 0)
                             (bottom-margin 0)
                             (left-inset 0)
                             (top-inset 0)
                             (right-inset 0)
                             (bottom-inset 0)))
      (send inner-t change-style (make-object style-delta% 'change-alignment 'top) 0 2)

      (send output-text hide-caret #t)
      (send inner-t hide-caret #t)
      (send outer-t hide-caret #t)
      (send output-text lock #t)
      (send inner-t lock #t)
      (send outer-t lock #t)
      
      (hide-details)
      
      (inherit set-snipclass)
      (set-snipclass bindings-snipclass)))
  
  (define black-style-delta (make-object style-delta% 'change-normal-color))
  (define green-style-delta (make-object style-delta%))
  (void (send green-style-delta set-delta-foreground "forest green"))

  (define turn-snip%
    (class snip%
      
      (init-field on-up on-down)
      
      ;; state : (union 'up 'down 'up-click 'down-click))
      (init-field [state 'up])
      
      (define/override (copy)
        (instantiate turn-snip% ()
          (on-up on-up)
          (on-down on-down)
          (state state)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([bitmap (case state
                        [(up) up-bitmap]
                        [(down) down-bitmap]
                        [(up-click) up-click-bitmap]
                        [(down-click) down-click-bitmap])])
          (cond
            [(send bitmap ok?)
             (send dc draw-bitmap bitmap x y)]
            [(send dc draw-rectangle x y 10 10)
             (send dc drawline x y 10 10)])))
             

      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0)
        (set-box/f! w arrow-snip-width)
        (set-box/f! h arrow-snip-height))
      
      (define/override (on-event dc x y editorx editory evt)
        (let ([snip-evt-x (- (send evt get-x) x)]
              [snip-evt-y (- (send evt get-y) y)])
          (cond
            [(send evt button-down? 'left)
             (set-state (case state
                          [(up) 'up-click]
                          [(down) 'down-click]
                          [else 'down-click]))]
            [(and (send evt button-up? 'left)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 
                           (on-down)
                           'down]
                          [(down down-click)
                           (on-up)
                           'up]
                          [else 'down]))]
            [(send evt button-up? 'left)
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up]))]
            [(and (send evt get-left-down)
                  (send evt dragging?)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 'up-click]
                          [(down down-click) 'down-click]
                          [else 'up-click]))]
            [(and (send evt get-left-down)
                  (send evt dragging?))
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up-click]))]
            [else
             (super on-event dc x y editorx editory evt)])))

      (inherit get-admin)
      (define/private (set-state new-state)
        (unless (eq? state new-state)
          (set! state new-state)
          (let ([admin (get-admin)])
            (when admin
              (send admin needs-update this 0 0 arrow-snip-width arrow-snip-height)))))
      
      (define/override (adjust-cursor dc x y editorx editory event) arrow-snip-cursor)
      
      (super-instantiate ())
      
      (inherit get-flags set-flags)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define down-bitmap (make-object bitmap% (collection-file-path "turn-down.png" "icons")))
  (define up-bitmap (make-object bitmap% (collection-file-path "turn-up.png" "icons")))
  (define down-click-bitmap (make-object bitmap% (collection-file-path "turn-down-click.png" "icons")))
  (define up-click-bitmap (make-object bitmap% (collection-file-path "turn-up-click.png" "icons")))
  (define arrow-snip-height
    (max 10
         (send up-bitmap get-height)
         (send down-bitmap get-height)
         (send up-click-bitmap get-height)
         (send down-click-bitmap get-height)))
  (define arrow-snip-width
    (max 10
         (send up-bitmap get-width)
         (send down-bitmap get-width)
         (send up-click-bitmap get-width)
         (send down-click-bitmap get-width)))
  (define arrow-snip-cursor (make-object cursor% 'arrow))
  
  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (make-output-port #f
                      always-evt
                      (λ (s start end flush?) 
                        (send text insert (substring s start end)
                              (send text last-position)
                              (send text last-position))
                        (- end start))
                      void))

; one trivial test case:
;
;(require bindings-browser)
;
;(let ([es (render-bindings/snip `((,#`a 3) (,#`b 4) (,#`c (1 3 4))))])
;  (define f (make-object frame% "frame" #f 850 500))
;  (define mb (make-object menu-bar% f))
;  (define edit-menu (make-object menu% "Edit" mb))
;  (define t (make-object text%))
;  (define ec (make-object editor-canvas% f t))
;  (append-editor-operation-menu-items edit-menu)
;  (send t insert es)
;  (send f show #t))
