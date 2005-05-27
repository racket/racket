(require
 "../aligned-pasteboard.ss"
 "../aligned-editor-container.ss")

(define (converges? x) #t)

(converges?
 (let* ((f (new frame% (label "test")))
        (e (new text%))
        (c (new editor-canvas% (editor e) (parent f)))
        (pb (new vertical-pasteboard%))
        (actual (new text%))
        (act-line (new aligned-editor-snip% (editor (new vertical-pasteboard%))))
        (t (new aligned-editor-snip% (editor pb))))
   (send e insert t)
   (send* pb (begin-edit-sequence) (insert act-line #f) (end-edit-sequence))))