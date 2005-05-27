;; Note this test case fails when the snip 'y' is stretchable. There is lots of extra space. Finding out
;; why will probably fix the test case's extra space.
(require
 "../aligned-editor-container.ss"
 "../aligned-pasteboard.ss")

(define editor (new vertical-pasteboard%))
(define pb (new horizontal-pasteboard%))
(define z (new editor-snip% (editor (new text%))))
(send* pb
  (insert (make-object string-snip% "Call") #f)
  (insert z #f))
(define y (new aligned-editor-snip%
               (editor pb)
               (stretchable-width #t)
               (stretchable-height #t)))
(send editor insert y)
(define f (new frame% (label "more-tests-text") (width 200) (height 200)))
(define e (new pasteboard%))
(define c (new editor-canvas% (editor e) (parent f)))
(define t (new aligned-editor-snip%
               (editor editor)
               (stretchable-height #f)
               (stretchable-width #f)))
(send e insert t)
(send f show #t)

;;;;;;;;;;
;; exploration
(eq-hash-code y)
(eq-hash-code t)
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