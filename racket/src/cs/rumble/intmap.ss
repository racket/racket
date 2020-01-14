
;; We have several implementations of immutable hash tables. Pick one...

;; (include "rumble/patricia.ss")
;;
;; This Patricia-trie implementation is the prettiest and fastest. It
;; uses the most memory, though --- typically much more than the
;; vector-stencil HAMT.

(include "rumble/hamt-stencil.ss")
;;
;; This HAMT implementation using stencil vectors tends to use the
;; least memory, often 1/3 the space of the Patricia-trie
;; implementation. It's slower than the Patricia-tree implementation
;; for some operations, up to a factor of 2 for `hash-set` or
;; `hash-keys-subset?`.

;; (include "rumble/hamt-vector.ss")
;;
;; This HAMT implementation uses plain vectors instead of stencil
;; vectors. Its speed and memory use are both worse than the
;; stencil-vector HAMT implementation, but it was the original source
;; of the stencil-vector implementation.
