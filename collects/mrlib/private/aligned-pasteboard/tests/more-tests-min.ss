(require
 "../aligned-editor-container.ss"
 "../aligned-pasteboard.ss")

(define editor (new vertical-pasteboard%))
(define pb (new horizontal-pasteboard%))
(send* pb
  (insert (make-object string-snip% "Call") #f)
  (insert (new editor-snip% (editor (new text%))) #f))
(send editor insert (new aligned-editor-snip% (editor pb)))
(define f (new frame% (label "more-test-jacob") (width 200) (height 200)))
(define e (new vertical-pasteboard%))
(define c (new aligned-editor-canvas% (editor e) (parent f)))
(define t (new aligned-editor-snip%
               (editor editor)
               (stretchable-height #f)
               (stretchable-width #f)))
(send e insert t)
(send f show #t)

;;;;;;;;;;
;; exploration
(require "../snip-lib.ss")
(define t-e (send t get-editor))
(send t-e get-aligned-min-width)
(send t get-aligned-min-width)
(define fs (send t-e find-first-snip))
(define fs (send t-e find-first-snip))
(define fs-e (send fs get-editor))
(send fs-e find-first-snip)
(send fs-e get-aligned-min-width)
(send fs get-aligned-min-width)
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