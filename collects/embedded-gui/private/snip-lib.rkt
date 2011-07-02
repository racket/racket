#| WARNING: DUPLICATED FILE |#

(module snip-lib mzscheme
  
  (require
   mzlib/class
   mzlib/etc
   mred
   mzlib/list
   mzlib/contract
   "interface.rkt")
  
  ;; a snip
  (define snip? (is-a?/c snip%))
  ;; a snip to act as the varying argument to a recursive functions
  (define linked-snip? (union snip? false/c))
  ;; a function to act on snips being mapped
  (define snip-visitor? any/c #;((snip?) (listof any/c) . ->* . (void)))
  ;; the rest of the lists passed to a snip mapping function
  (define rest-lists? (listof (listof any/c)))
  ;; a class that contains a snip
  (define editor? (is-a?/c editor<%>))
  
  (provide/contract
   (snip-width (snip? . -> . number?))
   (snip-height (snip? . -> . number?))
   (snip-min-width (snip? . -> . number?))
   (snip-min-height (snip? . -> . number?))
   (snip-parent (snip? . -> . (union editor? false/c)))
   (fold-snip ((snip? any/c . -> . any/c) any/c linked-snip? . -> . any/c))
   (for-each-snip any/c #;((snip-visitor? linked-snip?) rest-lists? . ->* . (void)))
   (map-snip any/c #;((snip-visitor? linked-snip?) rest-lists? . ->* . ((listof any/c))))
   (stretchable-width? (snip? . -> . boolean?))
   (stretchable-height? (snip? . -> . boolean?)))
  
  ;; the width of a snip in the parent pasteboard
  (define (snip-width snip)
    (let ([left (box 0)]
          [right (box 0)]
          [pasteboard (snip-parent snip)])
      (send pasteboard get-snip-location snip left (box 0) false)
      (send pasteboard get-snip-location snip right (box 0) true)
      (- (unbox right) (unbox left))))
    
  ;; the height of a snip in the parent pasteboard
  (define (snip-height snip)
    (let ([top (box 0)]
          [bottom (box 0)]
          [pasteboard (snip-parent snip)])
      (send pasteboard get-snip-location snip (box 0) top false)
      (send pasteboard get-snip-location snip (box 0) bottom true)
      (- (unbox bottom) (unbox top))))
  
  ;; the minimum width of the snip
  (define (snip-min-width snip)
    (cond
      [(is-a? snip stretchable-snip<%>)
       (send snip get-aligned-min-width)]
      [else (snip-width snip)]))
  
  ;; the minimum height of the snip
  (define (snip-min-height snip)
    (cond
      [(is-a? snip stretchable-snip<%>)
       (send snip get-aligned-min-height)]
      [else (snip-height snip)]))
  
  ;; the pasteboard that contains the snip
  (define (snip-parent snip)
    (let ([admin (send snip get-admin)])
      (if admin
          (send admin get-editor)
          false)))
  
  ;; the application of f on all snips from snip to the end in a foldl foldr mannor
  (define (fold-snip f init-acc snip)
    (let loop ([snip snip]
               [acc init-acc])
      (cond
        [(is-a? snip snip%)
         (loop (send snip next) (f snip acc))]
        [else acc])))
  
  ;; applies the function to all the snips
  (define (for-each-snip f first-snip . init-lists)
    (let loop ([snip first-snip]
               [lists init-lists])
      (cond
        [(is-a? snip snip%)
         (apply f (cons snip (map first lists)))
         (loop (send snip next)
               (map rest lists))]
        [else (void)])))
  
  ;; a list of f applied to each snip
  (define (map-snip f first-snip . init-lists)
    (let loop ([snip first-snip]
               [lists init-lists])
      (cond
        [(is-a? snip snip%)
         (cons (apply f (cons snip (map first lists)))
               (loop (send snip next)
                     (map rest lists)))]
        [else empty])))
  
  ;; true if the snip can be resized in the x dimention
  (define (stretchable-width? snip)
    (cond
      [(is-a? snip stretchable-snip<%>)
       (send snip stretchable-width)]
      [else false]))
  
  ;; true if the snip can be resized in the y dimention
  (define (stretchable-height? snip)
    (cond
      [(is-a? snip stretchable-snip<%>)
       (send snip stretchable-height)]
      [else false]))
  )
