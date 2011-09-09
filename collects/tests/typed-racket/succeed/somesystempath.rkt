#lang typed/racket

(: unix-path Path-For-Some-System)
(define unix-path (string->some-system-path "file.rkt" 'unix))

(: windows-path Path-For-Some-System)
(define windows-path (string->some-system-path "file.rkt" 'windows))

(for: ((p : Path-For-Some-System (list unix-path windows-path)))
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




