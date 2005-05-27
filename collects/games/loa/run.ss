#|

Matthew writes about the refreshing problem:

Quoting Robert Bruce Findler:
> It appears that invalidate bitmap cache in a pasteboard invalidates the
> smallest square of the pasteboard that contains all of the snips, which
> might not be the entire visible region of the pasteboard. The
> documentation (and my preferred behavior) would be that it invalidates
> the entire visible region, not just the region with snips.

By default (i.e., unless `set-mininum-{width,height}' is called), the
size of a pasetboard is the size of the smallest square that contains
the pasteboard's snips. So both the documentation and implementation
are correct in this case.

A pasteboard may be displayed in a canvas with extra space around the
pasteboard's area. That area is indeed not invalidated by
`invalidate-bitmap-cache'. Call `refresh' to foce the updating of a
canvas.

I don't think `invalidate-bitmap-cache' should invalidate any area
outside the editor's region; I belive the current behavior is the right
one. I added a note on the difference to the docs.

|#


(require-library "errortrace.ss" "errortrace")

(require-library "sig.ss" "games" "loa")

(invoke-unit/sig
 (compound-unit/sig
  (import (mred : mred^))
  (link
   [core : mzlib:core^ ((require-library "corer.ss"))]
   [utils : loa:utils^ ((require-library "utils.ss" "games" "loa"))]
   [grid : loa:grid^ ((require-library "grid.ss" "games" "loa") (core function) mred utils)]
   [computer : loa:computer-player^ ((require-library "computer.ss" "games" "loa") loa (core function))]
   [loa : loa^ ((require-library "loa.ss" "games" "loa") (core function) mred computer grid)])
  (export))
 mred^)

(yield (make-semaphore))