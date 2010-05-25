#lang racket/base

(provide define/chk 
         to-img 
         x-place?
         y-place?
         mode?
         angle?
         side-count?
         image-color?
         pen-style? 
         pen-cap?
         pen-join?
         image-snip->image
         bitmap->image
         check-mode/color-combination)

(require htdp/error
         racket/class
         lang/posn
         racket/gui/base
         "../../mrlib/image-core.ss"
         (prefix-in cis: "../../mrlib/cache-image-snip.ss")
         (for-syntax racket/base
                     racket/list))

;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                              ;;                      ;;      ;;                 
;                                              ;;                      ;;      ;;                 
;    ;;;;   ;;;;;;;;;  ;;;;   ;;;;      ;;;;   ;;;;;;   ;;;;    ;;;;   ;; ;;;  ;;  ;; ;;;  ;;;;;; 
;   ;;  ;;  ;;;; ;;;; ;;;;;;  ;;;;     ;;;;;;  ;;;;;;  ;;  ;;  ;;;;;;  ;;;;;   ;;  ;;;;;;  ;;;;;; 
;  ;;;;;;;; ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;;;;;;;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;  ;;;      ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;     ;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;   ;;; ;;  ;;   ;;   ;;;;;;  ;;       ;;;;;;  ;;  ;;  ;;; ;;  ;;;;;;  ;;  ;;  ;;  ;;  ;;  ;;;;;; 
;    ;;;;   ;;   ;;    ;;;;   ;;        ;;;;   ;;  ;;   ;;;;    ;;;;   ;;  ;;; ;;  ;;  ;;   ;;;;; 
;                                                                                          ;; ;;; 
;                                                                                          ;;;;;  
;                                                                                                 
;


(define-syntax define/chk
  (λ (stx)
    (syntax-case stx ()
      [(define/chk (fn-name args ... . final-arg) body ...)
       (identifier? #'final-arg)
       (let ([len (length (syntax->list #'(args ...)))])
         (with-syntax ([(i ...) (build-list len add1)])
           #`(define (fn-name args ... . final-arg)
               (let ([args (check/normalize 'fn-name 'args args i)] ...
                     [final-arg (map/i (λ (x j) (check/normalize 'fn-name 'final-arg x (+ #,len j)))
                                       final-arg)])
                 body ...))))]
      [(define/chk (fn-name args ...) body ...)
       (with-syntax ([(i ...) (build-list (length (syntax->list #'(args ...))) add1)])
         #'(define (fn-name args ...)
             (let ([args (check/normalize 'fn-name 'args args i)] ...)
               body ...)))])))

(define (map/i f l)
  (let loop ([l l]
             [i 0])
    (cond
      [(null? l) null]
      [else (cons (f (car l) i)
                  (loop (cdr l) (+ i 1)))])))

;; check/normalize : symbol symbol any number -> any
;; based on the name of the argument, checks to see if the input
;; is valid and, if so, transforms it to a specific kind of value
;;   width, height -> number
;;   mode -> 'outline 'solid
;;   color -> (is-a?/c color<%>)
(define (check/normalize fn-name argname arg i)
  (case argname
    [(x-place)
     (check-arg fn-name
                (x-place? arg)
                'x-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(y-place) 
     (check-arg fn-name
                (y-place? arg)
                'y-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(image image1 image2 image3) 
     (check-arg fn-name
                (image? arg)
                'image
                i
                arg)
     (to-img arg)]
    [(mode)
     (check-arg fn-name
                (mode? arg)
                'mode
                i
                arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(width height radius side-length side-length1 side-length2)
     (check-arg fn-name
                (and (real? arg)
                     (not (negative? arg)))
                'non-negative-real-number
                i arg)
     arg]
    [(dx dy x1 y1 x2 y2 pull1 pull2)
     (check-arg fn-name
                (real? arg)
                'real\ number
                i arg)
     arg]
    [(factor x-factor y-factor)
     (check-arg fn-name
                (and (real? arg)
                     (positive? arg))
                'positive\ real\ number
                i arg)
     arg]
    [(side-count)
     (check-arg fn-name
                (side-count? arg)
                'side-count
                i arg)
     arg]
    [(step-count)
     (check-arg fn-name
                (step-count? arg)
                'step-count
                i arg)
     arg]
    [(angle angle1 angle2)
     (check-arg fn-name
                (angle? arg)
                'angle\ in\ degrees
                i arg)
     (if (< arg 0)
         (+ arg 360)
         arg)]
    [(color)
     (check-arg fn-name (or (image-color? arg) (pen? arg)) 'image-color-or-pen i arg)
     ;; return either a string, color, or a pen,
     ;; (technically, the string case is redundant,
     ;;  but since there may be saved files that have 
     ;;  strings in the color positions we leave them
     ;;  here too; note that using a pen struct means
     ;;  'smoothed mode, but a color (or string) means
     ;;  'aligned mode, so that's not redundant).
     (cond
       [(color? arg) arg]
       [(pen? arg) arg]
       [else
        (let* ([color-str
                (if (symbol? arg)
                    (symbol->string arg)
                    arg)])
          (if (send the-color-database find-color color-str)
              color-str
              "black"))])]
    [(string)
     (check-arg fn-name (string? arg) 'string i arg)
     arg]
    [(font-size)
     (check-arg fn-name (and (integer? arg) (<= 1 arg 255)) 'font-size i arg)
     (if (exact? arg)
         arg
         (inexact->exact arg))]
    [(face)
     (check-arg fn-name (or (not arg) (string? arg)) 'face i arg)
     arg]
    [(family)
     (check-arg fn-name (memq arg '(default decorative roman script swiss modern symbol system)) 'family i arg)
     arg]
    [(style)
     (check-arg fn-name (memq arg '(normal italic slant)) 'style i arg)
     arg]
    [(weight)
     (check-arg fn-name (memq arg '(normal bold light)) 'weight i arg)
     arg]
    [(underline)
     (and arg #t)]
    [(posns)
     (check-arg fn-name
                (and (list? arg)
                     (andmap posn? arg))
                'list-of-posns
                i arg)
     (check-arg fn-name
                (>= (length arg) 3)
                'list-of-at-least-three-posns
                i arg)
     arg]
    [(int0-255-1 int0-255-2 int0-255-3)
     (check-arg fn-name (and (integer? arg) (<= 0 arg 255)) 
                'integer\ between\ 0\ and\ 255 i arg)
     arg]
    [(real-0-255)
     (check-arg fn-name (and (integer? arg) (<= 0 arg 255)) 
                'real\ number\ between\ 0\ and\ 255 i arg)
     arg]
    
    [(pen-style)
     (check-arg fn-name (pen-style? arg) 'pen-style i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(pen-cap)
     (check-arg fn-name (pen-cap? arg) 'pen-cap i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(pen-join)
     (check-arg fn-name (pen-join? arg) 'pen-join i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [else
     (error 'check "the function ~a has an argument with an unknown name: ~s"
            fn-name
            argname)]))

(define (y-place? arg)
  (member arg '("top" top "bottom" bottom "middle" middle "center" center "baseline" baseline)))
(define (x-place? arg)
  (member arg '("left" left "right" right "middle" middle "center" center)))
(define (mode? arg)
  (member arg '(solid outline "solid" "outline")))
(define (angle? arg)
  (and (real? arg)
       (< -360 arg 360)))
(define (side-count? i)
  (and (integer? i)
       (3 . <= .  i)))
(define (step-count? i)
  (and (integer? i)
       (1 . <= .  i)))
(define (image-color? c) (or (symbol? c) (string? c) (color? c)))
(define (pen-style? arg) 
  (member (if (string? arg) (string->symbol arg) arg)
          '(solid dot long-dash short-dash dot-dash)))
(define (pen-cap? arg)
  (member (if (string? arg) (string->symbol arg) arg)
          '(round projecting butt)))
(define (pen-join? arg)
  (member (if (string? arg) (string->symbol arg) arg)
          '(round bevel miter)))

(define (to-img arg)
  (cond
    [(is-a? arg image-snip%) (image-snip->image arg)]
    [(is-a? arg bitmap%) (bitmap->image arg)]
    [else arg]))

(define (image-snip->image is)
  (let ([bm (send is get-bitmap)])
    (cond
      [(not bm)
       ;; this might mean we have a cache-image-snip% 
       ;; or it might mean we have a useless snip.
       (let-values ([(w h) (if (is-a? is cis:cache-image-snip%)
                               (send is get-size)
                               (values 0 0))])
         (make-image (make-polygon
                      (list (make-point 0 0)
                            (make-point w 0)
                            (make-point w h)
                            (make-point 0 h))
                      'solid "black")
                     (make-bb w h h)
                     #f))]
      [else
       (bitmap->image bm
                      (or (send is get-bitmap-mask)
                          (send bm get-loaded-mask)))])))

(define (bitmap->image bm [mask-bm (send bm get-loaded-mask)])
  (let ([w (send bm get-width)]
        [h (send bm get-height)])
    (make-image (make-translate (/ w 2)
                                (/ h 2)
                                (make-bitmap bm mask-bm 0 1 1 #f #f))
                (make-bb w h h)
                #f)))


;; checks the dependent part of the 'color' specification
(define (check-mode/color-combination fn-name i mode color)
  (cond
    [(eq? mode 'solid)
     (check-arg fn-name (image-color? color) 'image-color i color)]
    [(eq? mode 'outline)
     (void)]))