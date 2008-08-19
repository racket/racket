(module size-snip mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "pretty.ss")
           (lib "framework.ss" "framework")
           "matcher.ss")
  
  (provide reflowing-snip<%>
           size-editor-snip%
           default-pretty-printer
           initial-char-width
           resizing-pasteboard-mixin)
  
  (define initial-char-width (make-parameter 30))
  
  (define (default-pretty-printer v port w spec)
    (parameterize ([pretty-print-columns w]
                   [pretty-print-size-hook
                    (λ (val display? op)
                      (cond
                        [(hole? val) 4]
                        [(eq? val 'hole) 6]
                        [else #f]))]
                   [pretty-print-print-hook
                    (λ (val display? op)
                      (cond
                        [(hole? val)
                         (display "hole" op)]
                        [(eq? val 'hole) 
                         (display ",'hole" op)]))])
      (pretty-print v port)))
  
  (define reflowing-snip<%>
    (interface ()
      reflow-program))
  
  (define (resizing-pasteboard-mixin pb%)
    (class pb%
      (init-field shrink-down?)
      
      (define/augment (on-interactive-resize snip)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program))
        (inner (void) on-interactive-resize snip))
      
      (define/augment (after-interactive-resize snip)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program))
        (inner (void) after-interactive-resize snip))
      
      (define/override (interactive-adjust-resize snip w h)
        (super interactive-adjust-resize snip w h)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program)))
      
      (inherit get-snip-location
               begin-edit-sequence
               end-edit-sequence)
      
      (define/augment (on-insert snip before x y)
        (begin-edit-sequence)
        (inner (void) on-insert snip before x y))
      (define/augment (after-insert snip before x y)
        (inner (void) after-insert snip before x y)
        (when (is-a? snip size-editor-snip%)
           (let ([cw (send snip get-char-width)]
                 [woc (send snip get-width-of-char)]
                 [bt (box 0)]
                 [bb (box 0)])
             (get-snip-location snip #f bt #f)
             (get-snip-location snip #f bb #t)
             (send snip resize 
                   (* cw woc)
                   (- (unbox bb) (unbox bt)))
             (when shrink-down?
               (send snip shrink-down))))
        (end-edit-sequence))
      (super-new)))
  
  (define size-editor-snip%
    (class* editor-snip% (reflowing-snip<%>)
      (init-field expr)
      (init pp)
      (init-field char-width)
      (define real-pp
        (if (procedure-arity-includes? pp 4)
            pp
            (lambda (v port w spec) (display (pp v) port))))
      (inherit get-admin)
      (define/public (get-expr) expr)
      (define/public (get-char-width) char-width)
      
      (define/override (resize w h)
        (super resize w h)
        (reflow-program))
      
      (inherit get-editor)
      ;; final
      (define/pubment (reflow-program)
        (let* ([tw (get-width-of-char)]
                [sw (get-snip-width)])
           (when (and tw sw)
             (let ([new-width (max 1 (inexact->exact (floor (/ sw tw))))])
               (unless (equal? new-width char-width)
                 (set! char-width new-width)
                 (format-expr)
                 (on-width-changed char-width))))))
      
      ;; final
      (define/pubment (shrink-down)
        (let ([ed (get-editor)]
              [bx (box 0)]
              [by (box 0)])
          (let ([max-line-width
                 (let loop ([p 0]
                            [max-w 0])
                   (cond
                     [(<= p (send ed last-paragraph))
                      (send ed position-location 
                            (send ed paragraph-end-position p)
                            bx by #t)
                      (let ([this-w (unbox bx)])
                        (loop (+ p 1)
                              (max this-w max-w)))]
                     [else max-w]))])
            (send ed position-location (send ed last-position) bx by #f)
            (let-values ([(hms vms) (get-margin-space)])
              (super resize
                     (+ max-line-width hms)
                     (+ (unbox by) vms))))))
      
      (inherit get-margin)
      (define/public (get-snip-width)
        (let ([admin (get-admin)])
          (and admin
               (let ([containing-editor (send admin get-editor)]
                     [bl (box 0)]
                     [br (box 0)])
                 (send containing-editor get-snip-location this bl #f #f)
                 (send containing-editor get-snip-location this br #f #t)
                 (let ([outer-w (- (unbox br) (unbox bl))])
                   (let-values ([(hms vms) (get-margin-space)])
                     (- outer-w hms)))))))
               
      (define/private (get-margin-space)
        (let ([bl (box 0)]
              [br (box 0)]
              [bt (box 0)]
              [bb (box 0)])
          (get-margin bl bt br bb)
          (values (+ (unbox bl) (unbox br) 2)   ;; not sure what the 2 is for. Maybe caret space?
                  (+ (unbox bt) (unbox bb)))))
      
      (define/public (get-width-of-char)
        (let ([ed (get-editor)])
          (and ed
               (let ([dc (send ed get-dc)]
                     [std-style (send (editor:get-standard-style-list) find-named-style "Standard")])
                 (and dc
                      (let-values ([(tw th _2 _3) (send dc get-text-extent "w"
                                                        (and std-style
                                                             (send std-style get-font)))])
                        tw))))))
      
      (define/public (get-height-of-char)
        (let ([ed (get-editor)])
          (and ed
               (let ([dc (send ed get-dc)]
                     [std-style (send (editor:get-standard-style-list) find-named-style "Standard")])
                 (and dc
                      (let-values ([(tw th _2 _3) (send dc get-text-extent "w"
                                                        (and std-style
                                                             (send std-style get-font)))])
                        th))))))

      (define/pubment (on-width-changed w) (inner (void) on-width-changed w))
      
      (define/public (format-expr)
        (let* ([text (get-editor)]
               [port (open-output-text-editor text)])
          (send text begin-edit-sequence)
          (when (is-a? text color:text<%>)
            (send text thaw-colorer))
          (send text set-styles-sticky #f)
          (send text erase)
          (real-pp expr port char-width text)
          (unless (zero? (send text last-position))
            (when (char=? #\newline (send text get-character (- (send text last-position) 1)))
              (send text delete (- (send text last-position) 1) (send text last-position))))
          (when (is-a? text color:text<%>)
            (send text freeze-colorer))
          (send text end-edit-sequence)))

      (super-new)
      (inherit use-style-background)
      (use-style-background #t))))