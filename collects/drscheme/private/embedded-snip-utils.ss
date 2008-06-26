#lang scheme/base
(require scheme/class
         framework
         scheme/gui/base)

(provide find-syntax-source-editor
         get-enclosing-editor-frame)

;; find-syntax-source-editor: syntax-source text% -> (or/c editor #f)
;; Looks for an embedded snip editor whose source is the a-stx-source.
;;
;; Note: this is a copy-and-paste from syncheck.
;; I've ripping out the editor caches for now,
;; until I get comments from others about this.
(define (find-syntax-source-editor a-stx-source defs-text)
  (let txt-loop ([text defs-text])
    (cond
      [(and (is-a? text text:basic<%>)
            (send text port-name-matches? a-stx-source))
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
            (snip-loop (send snip next))]))])))



;; get-enclosing-editor-frame: editor<%> -> (or/c frame% #f)
;; Returns the enclosing frame of an-editor, or #f if we can't find it.
(define (get-enclosing-editor-frame an-editor)
  (let ([admin (send an-editor get-admin)])
    (cond
      [(and admin (is-a? admin editor-snip-editor-admin<%>))
       (let* ([enclosing-editor-snip (send admin get-snip)])
         (cond
           [(get-snip-outer-editor enclosing-editor-snip)
            (get-enclosing-editor-frame (get-snip-outer-editor enclosing-editor-snip))]
           [else
            (let ([canvas (send an-editor get-canvas)])
              (and canvas
                   (send canvas get-top-level-window)))]))]
      [else
       (let ([canvas (send an-editor get-canvas)])
         (and canvas
              (send canvas get-top-level-window)))])))



;; get-snip-outer-editor: snip% -> (or/c editor<%> #f)
;; Returns the immediate outer editor enclosing the snip, or false if we
;; can't find it.
(define (get-snip-outer-editor a-snip)
  (cond
    [(send a-snip get-admin)
     (let* ([snip-admin (send a-snip get-admin)]
            [enclosing-editor (send snip-admin get-editor)])
       enclosing-editor)]
    [else
     #f]))