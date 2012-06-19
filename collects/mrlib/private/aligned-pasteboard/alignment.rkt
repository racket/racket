#|
This code computes the sizes for the rectangles in the space using the
on dimension off dimension method of referencing sizes. This means for
example instead of saying width we say off dimension for vertical
alignment. Inorder to consume and return the values in terms of width
and height manipulation had to be done. I chose to create a struct
abs-rect (abstract rectangle) and have code map horizontal and vertical
rect stucts on to them. This code is a bit long but more readable than
the other two options I came up with.
  1) define all functions to be letrec bound functions inside align.
     align then take accessors for the rect struct. The caller of align
     swaps the order of ondimension and off dimension accessors for
     vertical or horizontal code. This method does not allow the use of
     the readable, short, consis pattern matching code. As some of the
     matching code is easily removed this may be a good option but a
     large letrec is harder to write tests for.
  2) define a pattern matcher syntax that will match the struct rect but
     swap the fields based on wich on is the on or off dimension. This
     would have been shorter but much more confusing.
The current implementation requires align to map over the rects and
allocate new stucts for each one on both passing into and returning from
stretch-to-fit; This is not a bottle neck and it is the most readable
solution.
|#

(module alignment mzscheme
  
  (require
   mzlib/match
   mzlib/contract
   mzlib/etc
   mzlib/list)
  
  (define-struct rect (x y))
  (define-struct abs-rect (ondim offdim))
  (define-struct dim (pos size stretchable?))
  
  (define (nonnegative? n)
    (and (number? n)
         (or (positive? n)
             (zero? n))))
  
  (provide/contract
   (struct rect ((x dim?) (y dim?)))
   (struct abs-rect ((ondim dim?) (offdim dim?)))
   (struct dim ((pos nonnegative?) (size nonnegative?) (stretchable? boolean?)))
   (align ((symbols 'horizontal 'vertical)
           positive? positive? (listof rect?)
           . -> . (listof rect?)))
   (rect-print ((listof rect?) . -> . void?)))
  
  ;; align the rectangles within the given space
  (define (align type width height rects)
    (cond
      [(symbol=? type 'horizontal)
       (map abs->horiz (stretch-to-fit width height (map horiz->abs rects)))]
      [(symbol=? type 'vertical)
       (map abs->vert (stretch-to-fit height width (map vert->abs rects)))]))
  
  ;; abs->horiz (abs-rect? . -> . rect?)
  ;; convert an abstract rect to a horizontal rect
  (define abs->horiz
    (match-lambda
      [($ abs-rect ondim offdim)
       (make-rect ondim offdim)]))
  
  ;; abs->vert (abs-rect? . -> . rect?)
  ;; convert an abstract rect to a vertical rect
  (define abs->vert
    (match-lambda
      [($ abs-rect ondim offdim)
       (make-rect offdim ondim)]))
  
  ;; horiz->abs (rect? . -> . abs-rect?)
  ;; convert a horizontal rect to an abstract rect
  (define horiz->abs
    (match-lambda
      [($ rect x y)
       (make-abs-rect x y)]))
  
  ;; vert->abs (rect? . -> . abs-rect?)
  ;; convert a vertical rect to an abstract rect
  (define vert->abs
    (match-lambda
      [($ rect x y)
       (make-abs-rect y x)]))
  
  ;; stretch-to-fit (positive? positive? (listof abs-rect?) . -> (listof abs-rect?))
  ;; stretch the rectangles to fit with the given space
  (define (stretch-to-fit onsize offsize rects)
    (let-values ([(total-unstretchable-size stretchable-sizes)
                  (get-onsizes rects)])
      (let-values ([(extra-div extra-mod)
                    (get-extra/rect
                     (- onsize total-unstretchable-size)
                     (sort stretchable-sizes >))])
        (allocate-evenly/position extra-div extra-mod offsize rects))))
  
  ;; get-onsizes (((listof rect?)) . ->* . (nonnegative? (listof nonnegative?)))
  ;; gets the unstretchable total size and a list of the stretchable sizes
  (define (get-onsizes init-rects)
    (let loop ([extra 0]
               [stretchables empty]
               [rects init-rects])
      (match rects
        [() (values extra stretchables)]
        [(($ abs-rect ($ dim _ onsize #f) _) rest-rects ...)
         (loop (+ onsize extra) stretchables rest-rects)]
        [(($ abs-rect ($ dim _ onsize #t) _) rest-rects ...)
         (loop extra (cons onsize stretchables) rest-rects)])))
  
  ;; get-extra/rect ((nonnegative? (listof nonnegative?)) . ->* . (nonnegative? nonnegative?))
  ;; get the space that each stretchable snip will have
  (define (get-extra/rect init-extra init-sizes)
    (let loop ([sizes init-sizes]
               [extra init-extra]
               [count (length init-sizes)])
      (cond
        [(empty? sizes) (values 0 0)]
        [else
         (let ([extra/rect (quotient (floor extra) count)]
               [onsize (first sizes)])
           (if (> onsize extra/rect)
               (loop (rest sizes) (- extra onsize) (sub1 count))
               (values extra/rect (modulo (floor extra) count))))])))
  
  ;; allocate-evenly/position ((cons/p nonnegative? nonnegative?) positive? (listof abs-rect?) . -> .
  ;;                           (listof abs->rect?))
  ;; allocate the extra per rectangle to the stretchable rects and move them to their positions
  (define (allocate-evenly/position extra-div extra-mod offsize init-rects)
    (let ([mod (waner extra-mod)])
      (let loop ([rects init-rects]
                 [onpos 0])
        (match rects
          [() empty]
          [(($ abs-rect ($ dim _ min-onsize onstretch?)
               ($ dim _ min-offsize offstretch?)) rest-rects ...)
           (let ([onsize (if (and onstretch?
                                  (< min-onsize extra-div))
                             (+ extra-div (mod)) min-onsize)]
                 [offsize (if offstretch? offsize min-offsize)])
             (cons (make-abs-rect (make-dim onpos onsize onstretch?)
                                  (make-dim 0 offsize offstretch?))
                   (loop rest-rects (+ onpos onsize))))]))))
  
  ;; waner (natural-number? . -> . (-> (union 1 0)))
  ;; makes a thunk that returns 1 for its first n applications, zero otherwise
  (define (waner n)
    (lambda ()
      (if (zero? n)
          0
          (begin
            (set! n (sub1 n))
            1))))
  
  (define rect-print
    (match-lambda
      [() (void)]
      [(($ rect
           ($ dim x width stretchable-width?)
           ($ dim y height stretchable-height?))
        others ...)
       (printf "(make-rect (make-dim ~s ~s ~s) (make-dim ~s ~s ~s))\n"
               x width stretchable-width?
               y height stretchable-height?)
       (rect-print others)]))
  )
