(require-library "cores.ss")
(require-library "classd.ss")

(define-signature loa:grid^
  (grid-pasteboard%
   grid-canvas%
   grid-snip%))

(define-signature loa^
  (loa-pasteboard%
   loa-canvas%
   loa-checker%

   get-connected-regions))

(define-signature loa:utils^
  (vector-for-each))

(define-signature loa:computer-player^
  (computer-move))

(define-signature loa:move-import^
  (get-color ; : (board num num -> (union 'black 'white #f))
   ))