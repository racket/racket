#lang racket/base
(require racket/gui/base
         racket/class
         framework
         racket/pretty
         redex/private/lang-struct
         redex/private/matcher)

(provide reflowing-snip<%>
         size-editor-snip%
         size-text%
         default-pretty-printer
         default-pretty-printer-size-hook
         default-pretty-printer-print-hook
         pretty-print-parameters
         initial-char-width
         resizing-pasteboard-mixin
         get-user-char-width
         find-snip-height
         find-snip-width)

(define initial-char-width (make-parameter 30))

;; get-user-char-width : value-bound-to-'initial-char-width'-parameter sexp -> number
(define (get-user-char-width cw/proc expr)
  (cond
    [(number? cw/proc) cw/proc]
    [else (cw/proc expr)]))

(define pretty-print-parameters (make-parameter (λ (thunk) (thunk))))
  
(define (default-pretty-printer v port w spec)
  (parameterize ([pretty-print-columns w]
                 [pretty-print-size-hook default-pretty-printer-size-hook]
                 [pretty-print-print-hook default-pretty-printer-print-hook])
    ((pretty-print-parameters)
     (λ ()
       (pretty-write v port)))))

(define (default-pretty-printer-size-hook val display? op)
  (cond
    [(hole? val) 4]
    [(eq? val 'hole) 6]
    [else #f]))

(define (default-pretty-printer-print-hook val display? op)
  (cond
    [(hole? val)
     (display "hole" op)]
    [(eq? val 'hole) 
     (display ",'hole" op)]
    [else (write val op)]))

(define reflowing-snip<%>
  (interface ()
    reflow-program))

(define (resizing-pasteboard-mixin pb%)
  (class pb%
    
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
             end-edit-sequence
             find-first-snip
             get-dc)
    
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
    (define/public (set-char-width cw) 
      (unless (equal? char-width cw)
        (set! char-width cw)
        (format-expr)
        (on-width-changed char-width)))
    
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
        (values (+ (unbox bl) (unbox br) 6)   ;; not sure what the 2 is for. Maybe caret space?
                (+ (unbox bt) (unbox bb)))))
    
    ;; get-width-of-char : -> number or false
    ;; depends on `dc' field
    (define/public (get-width-of-char)
      (let ([ed (get-editor)])
        (and ed
             (let ([std-style (send (editor:get-standard-style-list) find-named-style "Standard")]
                   [dc (send ed get-dc)])
               (and dc
                    (let-values ([(tw th _2 _3) (send dc get-text-extent "w"
                                                      (and std-style
                                                           (send std-style get-font)))])
                      tw))))))
    
    ;; depends on `dc' field
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
    (use-style-background #t)))

(define size-text%
  (racket:set-mode-mixin
   (racket:text-mixin
    (editor:keymap-mixin
     (color:text-mixin
      (text:autocomplete-mixin
       (mode:host-text-mixin
        (editor:standard-style-list-mixin
         text:basic%))))))))



;; find-snip-height : editor snip -> number
(define (find-snip-height ed snip)
  (let ([bt (box 0)]
        [bb (box 0)])
    (send ed get-snip-location snip #f bt #f)
    (send ed get-snip-location snip #f bb #t)
    (- (unbox bb)
       (unbox bt))))

(define (find-snip-width ed snip)
  (let ([br (box 0)]
        [bl (box 0)])
    (send ed get-snip-location snip br #f #t)
    (send ed get-snip-location snip bl #f #f)
    (- (unbox br)
       (unbox bl))))
