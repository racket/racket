(require
 mzlib/class
 mzlib/etc
 mrlib/aligned-pasteboard
 mzlib/class
 mred)

(define f (instantiate frame% () (label "test") (width 400) (height 500)))
(define pb1 (instantiate vertical-pasteboard% ()))
(define ec (instantiate aligned-editor-canvas% () (parent f) (editor pb1) (style '(no-hscroll))))

(define pb2 (instantiate vertical-pasteboard% ()))
(define es2 (instantiate aligned-editor-snip% () (editor pb2)))

(define t (instantiate text% ()))
(define es3 (instantiate editor-snip% () (editor t)))

(send pb1 insert es2)
(send pb2 insert es3)
(send f show true)
