#lang scheme/base

(require string-constants/string-constant
         scheme/unit
         scheme/contract
         scheme/class
         drscheme/tool
         mzlib/list
         syntax/toplevel
         syntax/boundmap
         mrlib/bitmap-label
         (prefix-in drscheme:arrow: drscheme/arrow)
         (prefix-in fw: framework/framework)
         mred/mred
         setup/xref
         scribble/xref
         scribble/manual-struct
         net/url
         net/uri-codec
         browser/external
         (for-syntax scheme/base)
         "extra-stxcase.ss"
         "id-sets.ss"
         "extra-typed.ss")
;; color : syntax[original] str -> void
;; colors the syntax with style-name's style
(define (color stx style-name)
  (let ([source (find-source-editor stx)])
    (when (is-a? source text%)
      (let ([pos (- (syntax-position stx) 1)]
            [span (syntax-span stx)])
        (color-range source pos (+ pos span) style-name)))))

;; color-range : text start finish style-name 
;; colors a range in the text based on `style-name'
(define (color-range source start finish style-name)
  (let ([style (send (send source get-style-list)
                     find-named-style
                     style-name)])
    (add-to-cleanup-texts source)
    (send source change-style style start finish #f)))

   ;; find-source : definitions-text source -> editor or false
    (define (find-source-editor stx)
      (let ([defs-text (get-defs-text)])
        (and defs-text 
             (let txt-loop ([text defs-text])
               (cond
                 [(and (is-a? text fw:text:basic<%>)
                       (send text port-name-matches? (syntax-source stx)))
                  text]
                 [else
                  (let snip-loop ([snip (send text find-first-snip)])
                    (cond
                      [(not snip)
                       #f]
                      [(and (is-a? snip editor-snip%)
                            (send snip get-editor))
                       (or (txt-loop (send snip get-editor))
                           (snip-loop (send snip next)))]
                      [else 
                       (snip-loop (send snip next))]))])))))
 
;; add-to-cleanup-texts : (is-a?/c editor<%>) -> void
    (define (add-to-cleanup-texts ed)
      (let ([ed (find-outermost-editor ed)])
        (when (is-a? ed drscheme:unit:definitions-text<%>)
          (let ([tab (send ed get-tab)])
            (send tab syncheck:add-to-cleanup-texts ed)))))
    
        ;; get-defs-text : -> text or false
    (define (get-defs-text)
      (let ([drs-frame (currently-processing-drscheme-frame)])
        (and drs-frame
             (send drs-frame get-definitions-text))))
    
      (define (find-outermost-editor ed)
      (let loop ([ed ed])
        (let ([admin (send ed get-admin)])
          (if (is-a? admin editor-snip-editor-admin<%>)
              (let* ([enclosing-snip (send admin get-snip)]
                     [enclosing-snip-admin (send enclosing-snip get-admin)])
                (loop (send enclosing-snip-admin get-editor)))
              ed))))