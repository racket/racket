;; some more advanced aligned-pasteboard tests take from the test-case-boxes

(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "etc.ss")
 "../aligned-editor-container.ss"
 "../aligned-pasteboard.ss")

;; a text-case snip
(define test-case-box%
  (class aligned-editor-snip%
    
    ;; these edit-sequences are looping
    (define/public (hide-entries)
      (send* editor
        (begin-edit-sequence)
        (release-snip call-line)
        (release-snip exp-line)
        (release-snip act-line)
        (end-edit-sequence)))
    
    ;; these edit-sequences are looping
    (define/public (show-entries)
      (send* editor
        (begin-edit-sequence)
        (insert call-line false)
        (insert exp-line false)
        (insert act-line false)
        (end-edit-sequence)))
    
    (field
     [editor (new vertical-pasteboard%)]
     [turn-button (new image-snip%)]
     [comment (new text%)]
     [result (new image-snip%)]
     [call (new text%)]
     [expected (new text%)]
     [actual (new text%)]
     [top-line (make-top-line turn-button comment result)]
     [call-line (make-line "Call" call)]
     [exp-line (make-line "Expected" expected)]
     [act-line (make-line "Actual" actual)])
    
    (send editor insert top-line)
    (show-entries)
    
    (super-new
     (editor editor)
     (stretchable-height #f)
     (stretchable-width #f))))

;; the top line of the test-case
(define (make-top-line turn-snip comment result-snip)
  (let ([pb (new horizontal-pasteboard%)])
    (send* pb
      (insert turn-snip false)
      (insert (text-field comment) false)
      (insert result-snip false))
    (new aligned-editor-snip%
         (stretchable-height false)
         (editor pb))))

;; a line labeled with the given string and containing a given text
(define (make-line str text)
  (let ([pb (new horizontal-pasteboard%)])
    (send* pb
      (insert (make-object string-snip% str) false)
      (insert (text-field text) false))
    (new aligned-editor-snip% (editor pb))))

;; a text field fit to be in a test-case (no borders or margins etc.)
;;STATUS: this should really return a stretchable-snip<%> not an editor-snip% of fixed size.
(define (text-field text)
  (new editor-snip% (editor text)))

;; To make case 3 work, I need to send the forward set-aligned-min-sizes
;; from the snip. Currently that call only originates in the on-size of
;; the canvas but in case 3 the canvas does not belong to the aligned-*
;; collection. I think the place to call this forward set-aligned-min-sizes
;; is from within size-cache-invalid of the aligned-editor-snip
(define top
  (case 3
    [(1) (cons vertical-pasteboard% aligned-editor-canvas%)]
    [(2) (cons text% editor-canvas%)]
    [(3) (cons pasteboard% editor-canvas%)]))

(define f (new frame% (label "test") (width 200) (height 250)))
(define e (new (car top)))
(define c (new (cdr top) (editor e) (parent f)))
(define t (new test-case-box%))
(send e insert t)
(send f show #t)
;(send t hide-entries)
;(send t show-entries)