#lang setup/infotab
  (define categories '(media))
  (define can-be-loaded-with 'all)
  (define required-core-version "5.0.0.1")
  (define primary-file "main.rkt")
  (define scribblings '(("picturing-programs.scrbl" () (teaching -21))))
  (define repositories '("4.x"))
  (define compile-omit-paths '("tests"))
  (define blurb
      `("The picturing-programs collection supersedes the tiles and sb-world collections.  It provides functions to rotate, etc. images, as well as a slightly modified version of the universe teachpack."))
  (define release-notes '(
(p "Version 2.5: Re-enabled diagonal reflection.  Moved into the bundle
(so it doesn't require a PLaneT install).  Added some picture variables.
Rewrote a bunch of things for compatibility with 5.1.")
(p "Version 2.4: Added change-to-color and map3-image.  Cleaned up documentation.")
(p "Version 2.3: Renamed files from .ss to .rkt, so they work better with Racket.  Added map-image, build-image, name->color, and friends; re-fixed bug in rotate-cw and rotate-ccw.")
(p "Version 2.2: Fixed bug in rotate-cw and rotate-ccw; restored reflect-vert and reflect-horiz; added with-input-from-url.")
(p "Version 2.1: Added argument type-checking.  And reflection primitives are now present but produce error message, rather than being missing.")
(p "Version 2.0: now fully compatible with 2htdp/image and 2htdp/universe.  No pinholes; temporarily disabled reflection primitives.")
(p "Version 1.6: fixed same transparency bug for 4.2.4")
(p "Version 1.5: fixed same transparency bug for 4.2.3")
(p "Version 1.4: fixed transparency bug for 4.2.2")
(p "Version 1.3: initial release, for DrScheme 4.2.4")
(p "Version 1.2: initial release, for DrScheme 4.2.3")
(p "Version 1.1: initial release, for DrScheme 4.2.2")))
