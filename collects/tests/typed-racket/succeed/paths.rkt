#lang typed/racket

(: foo-path Path)
(define foo-path (build-path "foo" "bar" 'same 'up))

;Check predicates are always true
(+ (if (path? foo-path) 2 'two)
   (if (path-for-some-system? foo-path) 3 'three))

(: current-system (U 'unix 'windows))
(define current-system (system-path-convention-type))

(: other-system (U 'unix 'windows))
(define other-system
 (case (system-path-convention-type)
  ((unix) 'windows)
  ((windows) 'unix)))

(: other-foo-path Path-For-Some-System)
(define other-foo-path
 (build-path/convention-type other-system
  (string->some-system-path "foo" other-system)
  (string->some-system-path "bar" other-system)
  'same
  'up))


(path->string foo-path)
(some-system-path->string other-foo-path)
(path->bytes foo-path)
(path->bytes other-foo-path)

(bytes->path #"foo" other-system)
(string->path "foo")

(bytes->path-element #"foo" other-system)
(string->path-element "foo")


(cleanse-path foo-path)
(cleanse-path other-foo-path)

(expand-user-path foo-path)

(absolute-path? other-foo-path)
(relative-path? other-foo-path)
(complete-path? other-foo-path)




(split-path foo-path)
(split-path other-foo-path)


;Original Soundness bug
(when (path? other-foo-path) (error 'path "This shouldn't be raised"))

