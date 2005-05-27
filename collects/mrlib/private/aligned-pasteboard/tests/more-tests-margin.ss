(require
 "../aligned-editor-container.ss"
 "../aligned-pasteboard.ss")

(define pb (new horizontal-pasteboard%))
(send* pb
  (insert (make-object string-snip% "Call") #f)
  (insert (new editor-snip% (editor (new text%))) #f))
(define z (new aligned-editor-snip% (editor pb)))
(define f (new frame% (label "more-tests-text") (width 200) (height 200)))
(define e (new pasteboard%))
(define c (new editor-canvas% (editor e) (parent f)))
(send e insert z)
(send f show #t)

;;;;;;;;;;
;; exploration
(require "../snip-lib.ss")
(define (margin snip)
  (let ([left (box 0)]
        [top (box 0)]
        [right (box 0)]
        [bottom (box 0)])
    (send snip get-margin left top right bottom)
    (list (cons 'left (unbox left))
          (cons 'right (unbox right))
          (cons 'top (unbox top))
          (cons 'bottom (unbox bottom)))))