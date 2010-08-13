#lang racket/base
(require racket/class
         racket/gui/base
         framework
         "intf.rkt")
(provide color color-range
         find-source-editor
         find-source-editor/defs
         get-defs-text)

;; color : syntax[original] str -> void
;; colors the syntax with style-name's style
(define (color stx style-name mode)
  (let ([source (find-source-editor stx)])
    (when (and (is-a? source text%)
               (syntax-position stx)
               (syntax-span stx))
      (let ([pos (- (syntax-position stx) 1)]
            [span (syntax-span stx)])
        (color-range source pos (+ pos span) style-name mode)))))

;; color-range : text start finish style-name 
;; colors a range in the text based on `style-name'
(define (color-range source start finish style-name mode)
  (let ([style (send (send source get-style-list)
                     find-named-style
                     style-name)])
    (apply-style/remember source start finish style mode)))

;; find-source-editor : stx -> editor or false
(define (find-source-editor stx)
  (let ([defs-text (get-defs-text)])
    (and defs-text 
         (find-source-editor/defs stx defs-text))))

;; find-source-editor : stx text -> editor or false
(define (find-source-editor/defs stx defs-text)
  (cond
    [(not (syntax-source stx)) #f]
    [(and (symbol? (syntax-source stx))
          (text:lookup-port-name (syntax-source stx)))
     => values]
    [else
     (let txt-loop ([text defs-text])
       (cond
         [(and (is-a? text text:basic<%>)
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
               (snip-loop (send snip next))]))]))]))
;; get-defs-text : -> text or false
(define (get-defs-text)
  (currently-processing-definitions-text))

;; apply-style/remember : (is-a?/c editor<%>) number number style% symbol -> void
(define (apply-style/remember ed start finish style mode)
  (let ([outermost (find-outermost-editor ed)])
    (and (is-a? outermost syncheck-text<%>)
         (send outermost syncheck:apply-style/remember ed start finish style mode))))

(define (find-outermost-editor ed)
  (let loop ([ed ed])
    (let ([admin (send ed get-admin)])
      (if (is-a? admin editor-snip-editor-admin<%>)
          (let* ([enclosing-snip (send admin get-snip)]
                 [enclosing-snip-admin (send enclosing-snip get-admin)])
            (loop (send enclosing-snip-admin get-editor)))
          ed))))
