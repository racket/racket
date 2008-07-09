(require
 mzlib/class
 mred
 mzlib/etc
 mrlib/aligned-pasteboard)

(define frame (instantiate frame% () (label "big-min") (width 400) (height 500)))
(define test-suite (instantiate vertical-pasteboard% ()))
(instantiate aligned-editor-canvas% () (parent frame) (editor test-suite))

(define top-string false)

(define (new)
  (let* ([main-pb (instantiate horizontal-pasteboard% ())]
         [pb (instantiate vertical-pasteboard% ())]
         [snip (instantiate aligned-editor-snip% () (editor pb))]
         [string (make-object string-snip% "Testing String Snip")])
    (set! top-string string)
    (send main-pb insert snip false)
    (send main-pb insert string false)
    (instantiate aligned-editor-snip% () (editor main-pb))))

(send frame show true)
(define (add) (send test-suite insert (new)))
(add)
