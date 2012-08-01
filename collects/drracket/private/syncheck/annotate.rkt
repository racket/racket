#lang racket/base
(require racket/class
         "intf.rkt"
         "local-member-names.rkt")
(provide color color-range
         find-source-editor
         find-source-editor/defs
         add-mouse-over
         add-mouse-over/loc)

;; color : syntax[original] str -> void
;; colors the syntax with style-name's style
(define (color stx style-name)
  (let ([source (find-source-editor stx)])
    (when (and (syntax-position stx)
               (syntax-span stx))
      (let ([pos (- (syntax-position stx) 1)]
            [span (syntax-span stx)])
        (color-range source pos (+ pos span) style-name)))))

;; color-range : text start finish style-name 
;; colors a range in the text based on `style-name'
(define (color-range source start finish style-name)
  (define defs (current-annotations))
  (when defs
    (send defs syncheck:color-range source start finish style-name)))

;; add-mouse-over : syntax[original] string -> void
;; registers the range in the editor so that a mouse over
;; this area shows up in the status line.
(define (add-mouse-over stx str)
  (define source (find-source-editor stx))
  (define defs-text (current-annotations))
  (when (and defs-text 
             source
             (syntax-position stx)
             (syntax-span stx))
    (define pos-left (- (syntax-position stx) 1))
    (define pos-right (+ pos-left (syntax-span stx)))
    (send defs-text syncheck:add-mouse-over-status
          source pos-left pos-right str)))

(define (add-mouse-over/loc source pos-left pos-right str)
  (define defs-text (current-annotations))
  (when defs-text
    (send defs-text syncheck:add-mouse-over-status
          source pos-left pos-right str)))

;; find-source-editor : stx -> editor or false
(define (find-source-editor stx)
  (let ([defs-text (current-annotations)])
    (and defs-text 
         (find-source-editor/defs stx defs-text))))

;; find-source-editor : stx text -> editor or false
(define (find-source-editor/defs stx defs-text)
  (send defs-text syncheck:find-source-object stx))

