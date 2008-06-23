#lang scheme/base
(require scheme/class
         framework
         scheme/gui/base)

(provide find-syntax-source-editor)

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