#lang racket/base
(require racket/class
         "intf.rkt"
         "local-member-names.rkt")
(provide color color-range
         find-source-editor
         find-source-editor/defs)

;; color : syntax[original] str -> void
;; colors the syntax with style-name's style
(define (color stx style-name mode)
  (let ([source (find-source-editor stx)])
    (when (and (syntax-position stx)
               (syntax-span stx))
      (let ([pos (- (syntax-position stx) 1)]
            [span (syntax-span stx)])
        (color-range source pos (+ pos span) style-name mode)))))

;; color-range : text start finish style-name 
;; colors a range in the text based on `style-name'
(define (color-range source start finish style-name mode)
  (define defs (current-annotations))
  (when defs
    (send defs syncheck:color-range source start finish style-name mode)))

;; find-source-editor : stx -> editor or false
(define (find-source-editor stx)
  (let ([defs-text (current-annotations)])
    (and defs-text 
         (find-source-editor/defs stx defs-text))))

;; find-source-editor : stx text -> editor or false
(define (find-source-editor/defs stx defs-text)
  (send defs-text syncheck:find-source-object stx))

