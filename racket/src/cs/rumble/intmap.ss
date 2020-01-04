
;; We have several implementations of immutable hash tables. Pick one...

(include "rumble/patricia.ss")
;;
;; This Patricia-trie implementation is the prettiest and fastest. It
;; uses the most memory, though --- typically much more than the
;; vector-stencil HAMT.

;; (include "rumble/hamt-stencil.ss")
;;
;; This HAMT implementation using stencil vectors tends to use the
;; last memory, often by a lot. It's slower than the Patricia-tree
;; implementation, though, especially for `hash-keys-subset?`.

;; (include "rumble/hamt-vector.ss")
;;
;; This HAMT implementaiton uses plain vectors instead of stencil
;; vectors. Its speed and memory use are intermediate, but its speed
;; is closer to the stencil-vector HAMT implementation, and memory use
;; is closer to the Patrica trie implementation.
