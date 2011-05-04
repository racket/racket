#lang typed/racket

(: unix-path SomeSystemPath)
(define unix-path (string->some-system-path "file.rkt" 'unix))

(: windows-path SomeSystemPath)
(define windows-path (string->some-system-path "file.rkt" 'windows))

(for: ((p : SomeSystemPath (list unix-path windows-path)))
 (let ((long-path (build-path p 'up 'same p)))
   (unless (path-for-some-system? p)
    (error "Predicate failed"))
   (explode-path long-path)
   
   (filename-extension p)
   (path-only long-path)
   (some-system-path->string long-path)
   
   ))


(when (equal? 'unix (system-path-convention-type))
 (find-relative-path (simplify-path (path->complete-path "foo")) (simplify-path (path->complete-path "foo/foo/foo"))))




