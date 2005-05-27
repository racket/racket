(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 "../aligned-pasteboard.ss"
 "../verthoriz-alignment.ss"
 "../snip-wrapper.ss"
 "../stretchable-editor-snip.ss")

(define actual-show? #f)
(define collapsed? #t)
(define actual (new text%))
(define to-test (new text%))
(define expected (new text%))

(define pb (new aligned-pasteboard%))
(define main (new horizontal-alignment% (parent pb)))
(define left (new vertical-alignment%
                  (parent main)
                  (show? (not collapsed?))))
(define right (new vertical-alignment%
                       (parent main)
                       (show? (not collapsed?))))
(define button-pane (new vertical-alignment% (parent main)))
(define to-test-pane (new vertical-alignment% (parent left)))
(define expected-pane (new vertical-alignment% (parent right)))
(define actual-pane (new vertical-alignment%
                         (parent right)
                         (show? actual-show?)))

(define f (new frame% (label "f") (width 400) (height 500)))
(send f show #t)
(define e (new text%))
(define c (new editor-canvas% (editor e) (parent f)))
(define es (new editor-snip% (editor pb)))

(define (show-actual show?)
  (set! actual-show? show?)
  (send actual-pane show actual-show?))

(define (collapse bool)
  (set! collapsed? bool)
  (send left show (not collapsed?))
  (send right show (not collapsed?)))

(send e insert es)

(define (labeled-field alignment label text)
  ;; I string-append here to give space after the label
  ;; They look a lot better without something right after them.
  (new snip-wrapper%
       (snip (make-object string-snip% (string-append label "     ")))
       (parent alignment))
  (new snip-wrapper%
       (snip (new stretchable-editor-snip%
                  (editor text)
                  (stretchable-height #f)))
       (parent alignment)))
          
(send pb lock-alignment #t)
(labeled-field to-test-pane "Test" to-test)
(labeled-field expected-pane "Expected" expected)
(new snip-wrapper%
     (snip (make-object string-snip% "Actual"))
     (parent actual-pane))
(new snip-wrapper%
     (snip (new stretchable-editor-snip%
                (editor actual)
                (stretchable-height #f)))
     (parent actual-pane))
(send pb lock-alignment #f)
;(collapse #t)
;(collapse #f)