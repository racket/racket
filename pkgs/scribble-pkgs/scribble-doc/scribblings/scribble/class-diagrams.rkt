#lang racket/base
(require (prefix-in etc: mzlib/etc)
         texpict/mrpict
         (only-in pict pin-line pin-arrow-line)
         (except-in texpict/utils pin-line pin-arrow-line)
         racket/class
         racket/runtime-path
         racket/draw
         racket/contract
         (only-in racket/list last))

(define the-font-size 12)
(define prim-font-family 'swiss)
;; Was 'modern, but we want font smoothing even for small text:
(define font-family (make-object font% the-font-size 'modern 'normal 'normal #f 'smoothed))

(define prim-types '("int" "String" "float" "double" "boolean"))

;; how far a dot is to the right of a class
(define dot-edge-spacing 10)

(define field-arrowhead-size 10)

(define hierarchy-color "navy")
(define type-link-color "firebrick")

#|
(define font-family "Palatino")
(define-runtime-path afm "afm")
(current-ps-afm-file-paths (cons afm (current-ps-afm-file-paths)))

(let ([id (send the-font-name-directory find-or-create-font-id font-family 'default)])
  (send the-font-name-directory set-post-script-name 
        id 'normal 'normal "Palatino-Roman")
  (send the-font-name-directory set-post-script-name 
        id 'bold 'normal "Palatino-Bold")
  (send the-font-name-directory set-post-script-name 
        id 'normal 'italic "Palatino-Italic"))
|#

(define (user-type-font x) (text x font-family the-font-size))
(define (prim-type-font x) (text x  prim-font-family the-font-size))
(define (var-font x) (text x `(bold . ,font-family) the-font-size))
(define (field-name-font x) (text x font-family the-font-size))
(define (comment-font x) (text x font-family the-font-size))
(define (normal-font x) (text x font-family the-font-size))
(define (java-this) (text "this" `(bold . ,font-family) the-font-size))

;; field-spec : string string -> pict
(define (field-spec type fd #:default [default #f] [comment #f])
  (let ([code-line
         (hbl-append (if type 
                         (hbl-append (type-spec type)
                                     (normal-font " "))
                         (blank))
                     (field-name-font fd)
                     (if default
                         (hbl-append (normal-font " = ")
                                     (normal-font default))
                         (blank))
                     #;
                     (normal-font ";"))])
    (if comment
        (hbl-append code-line 
                    (normal-font " ")
                    (comment-font (format "[in ~a]" comment)))
        code-line)))

(define (method-spec range name #:body [body #f] . args)
  (unless (even? (length args))
    (error 'method-spec "expected a list of types and argument names, but found ~a arguments"
           (length args)))
  (let ([first-line
         (hbl-append
          (type-spec range)
          (normal-font " ")
          (var-font name)
          (cond
            [(null? args)
             (normal-font "()")]
            [else
             (hbl-append
              (normal-font "(")
              (let loop ([args args])
                (let* ([type (car args)]
                       [param (cadr args)]
                       [single-arg 
                        (if param 
                            (hbl-append (type-spec type)
                                        (normal-font " ")
                                        (var-font param))
                            (type-spec type))])
                  
                  (cond
                    [(null? (cddr args))
                     (hbl-append single-arg (normal-font ")"))]
                    [else
                     (hbl-append single-arg
                                 (normal-font ", ")
                                 (loop (cddr args)))]))))])
          (if body
              (hbl-append (normal-font " {"))
              (blank)))])
    (if body
        (vl-append first-line
                   (hbl-append (blank 8 0) body (normal-font "}")))
        first-line)))
             
(define (type-spec str)
  (cond
    [(member str prim-types) (prim-type-font str)]
    [else (user-type-font str)]))

;; class-name : string -> pict
(define (class-name txt #:spacing-word [spacing-word txt]) 
  (define p (colorize (lt-superimpose (ghost (var-font spacing-word))
                                      (apply vl-append (map var-font (regexp-split #rx"\n" txt))))
                      "white"))
  (refocus (cc-superimpose (colorize (filled-rectangle (+ class-box-margin class-box-margin (pict-width p))
                                                       (+ class-box-margin class-box-margin (pict-height p)))
                                     "black")
                           p)
           p))

(define class-box-margin 4)

;; class-box : pict (or/c #f (listof pict)) (or/c #f (listof pict)) -> pict
(define (class-box name fields methods)
  (let* ([mk-blank (λ () (blank 0 (+ class-box-margin class-box-margin)))])
    (cond
      [(and methods fields)
       (let* ([top-spacer (mk-blank)]
              [bottom-spacer (mk-blank)]
              [main (vl-append name 
                               top-spacer
                               (if (null? fields)
                                   (blank 0 4)
                                   (apply vl-append fields))
                               bottom-spacer
                               (if (null? methods)
                                   (blank 0 4)
                                   (apply vl-append methods)))])
         (add-hline
          (add-hline (frame (inset main class-box-margin))
                     top-spacer)
          bottom-spacer))]
      [fields
       (let* ([top-spacer (mk-blank)]
              [main (vl-append name 
                               top-spacer
                               (if (null? fields)
                                   (blank)
                                   (apply vl-append fields)))])
         (add-hline (frame (inset main class-box-margin))
                    top-spacer))]
      [methods (class-box name methods fields)]
      [else (frame (inset name class-box-margin))])))

(define (add-hline main sub)
  (let-values ([(x y) (cc-find main sub)])
    (pin-line main
              sub (λ (p1 p2) (values 0 y))
              sub (λ (p1 p2) (values (pict-width main) y)))))

;; hierarchy : pict (cons pict (listof pict)) (cons pict (listof pict)) -> pict
(define (hierarchy main supers subs)
  (let ([supers-bottoms (apply max (map (λ (x) (let-values ([(x y) (cb-find main x)]) y)) supers))]
        [subs-tops (apply min (map (λ (x) (let-values ([(x y) (ct-find main x)]) y)) subs))]
        [sorted-subs (sort subs (λ (x y) (< (left-edge-x main x) (left-edge-x main y))))])
    (unless (< supers-bottoms subs-tops)
      (error 'hierarchy "expected supers to be on top of subs, supers bottom is at ~a, and subs tops is at ~a"
             supers-bottoms
             subs-tops))
    (let* ([main-line-y (max (- subs-tops 20) (/ (+ supers-bottoms subs-tops) 2))]
           [main-line-start-x (center-x  main (car sorted-subs))]
           [main-line-end-x (center-x main (last sorted-subs))]
           [w/main-line
            (pin-line main
                      main (λ (_1 _2) (values main-line-start-x main-line-y))
                      main (λ (_1 _2) (values main-line-end-x main-line-y))
                      #:color hierarchy-color)]
           [super-lines
            (map (λ (super) 
                   (let-values ([(x y) (cb-find main super)])
                     (pin-over 
                      (pin-line (ghost main)
                                super cb-find
                                main (λ (_1 _2) (values x main-line-y)))
                      (- x (/ (pict-width triangle) 2))
                      (- (/ (+ y main-line-y) 2)
                         (/ (pict-height triangle) 2))
                      triangle)))
                 supers)]
           [sub-lines
            (map (λ (sub) 
                   (let-values ([(x y) (ct-find main sub)])
                     (pin-line (ghost main)
                               sub ct-find
                               main (λ (_1 _2) (values x main-line-y))
                               #:color hierarchy-color)))
                 subs)])
      (apply cc-superimpose 
             w/main-line
             (append sub-lines
                     super-lines)))))

(define triangle-width 12)
(define triangle-height 12)
(define triangle
  (let ([points (list (make-object point% (/ triangle-width 2) 0)
                      (make-object point% 0 triangle-height)
                      (make-object point% triangle-width triangle-height))])
    (colorize 
     (dc (λ (dc dx dy)
           (let ([brush (send dc get-brush)])
             (send dc set-brush (send brush get-color) 'solid)
             (send dc draw-polygon points dx dy)
             (send dc set-brush brush)))
         triangle-width
         triangle-height)
     hierarchy-color)))

(define (center-x main pict)
  (let-values ([(x y) (cc-find main pict)])
    x))

(define (left-edge-x main pict)
  (let-values ([(x y) (lc-find main pict)])
    x))


(define (add-dot-right main class field) (add-dot-left-right/offset main class field 0 rc-find))
(define add-dot-right/space 
  (λ (main class field [count 1])
    (add-dot-right/offset main class field (* count dot-edge-spacing))))
(define (add-dot-right/offset main class field offset)
  (add-dot-left-right/offset main class field offset rc-find))

(define (add-dot-left main class field) (add-dot-left-right/offset main class field 0 lc-find))
(define add-dot-left/space
  (λ (main class field [count 1])
    (add-dot-left/offset main class field (* count (- dot-edge-spacing)))))
(define (add-dot-left/offset main class field offset)
  (add-dot-left-right/offset main class field offset lc-find))

(define (add-dot-left-right/offset main class field offset finder)
  (let-values ([(_1 y) (cc-find main field)]
               [(x-edge _2) (finder main class)])
    (add-dot main (+ x-edge offset) y)))

(define add-dot-junction
  (case-lambda
    [(main x-pict y-pict) (add-dot-junction main x-pict cc-find y-pict cc-find)]
    [(main x-pict x-find y-pict y-find)
     (let-values ([(x _1) (x-find main x-pict)]
                  [(_2 y) (y-find main y-pict)])
       (add-dot main x y))]))

(define (add-dot-offset pict dot dx dy)
  (let-values ([(x y) (cc-find pict dot)])
    (add-dot pict (+ x dx) (+ y dy))))

(define dot-δx (make-parameter 0))
(define dot-δy (make-parameter 0))

(define (add-dot pict dx dy)
  (let ([dot (blank)])
    (values (pin-over pict 
                      (+ dx (dot-δx))
                      (+ dy (dot-δy))
                      dot)
            dot)))

(define (connect-dots show-arrowhead? main dot1 . dots)
  (let loop ([prev-dot dot1]
             [dots dots]
             [pict main])
    (cond
      [(null? dots) pict]
      [else 
       (loop (car dots) 
             (cdr dots)
             (connect-two-dots pict prev-dot (car dots) (null? (cdr dots)) show-arrowhead?))])))

(define (connect-two-dots pict dot1 dot2 arrowhead? show-arrowhead?)
  (if arrowhead?
      (pin-arrow-line field-arrowhead-size pict
                      dot1 cc-find
                      dot2 cc-find
                      #:hide-arrowhead? (not show-arrowhead?)
                      #:color type-link-color)
      (pin-line pict
                dot1 cc-find
                dot2 cc-find
                #:color type-link-color)))

(define (hierarchy/layout tops bottoms 
                          #:every-other-space [every-other-space 0]
                          #:top-space [top-space 40]
                          #:bottom-space [bottom-space 40]
                          #:vertical-space [vertical-space 60])
  (hierarchy 
   (vc-append (apply ht-append top-space tops)
              (blank 0 vertical-space)
              (apply ht-append bottom-space 
                     (let loop ([bottoms bottoms]
                                [every-other? #f])
                       (cond
                         [(null? bottoms) '()]
                         [else
                          (cons (if every-other?
                                    (vc-append (blank 0 every-other-space)
                                               (car bottoms))
                                    (car bottoms))
                                (loop (cdr bottoms)
                                      (not every-other?)))]))))
   tops
   bottoms))

(define (add-dot-delta f dx dy)
  (parameterize ([dot-δx dx]
                 [dot-δy dy])
    (f)))


(define (right-right-reference main0 start-class start-field finish-class finish-name
                               [count 1] 
                               #:connect-dots [connect-dots connect-dots]
                               #:dot-delta [dot-delta 0])
  (let ([going-down? (let-values ([(_1 start-y) (find-cc main0 start-field)]
                                  [(_2 finish-y) (find-cc main0 finish-name)])
                       (< start-y finish-y))])
    (define-values (main1 dot1) (add-dot-delta (λ () (add-dot-right main0 start-class start-field))
                                               0
                                               (if going-down? 
                                                   dot-delta
                                                   (- dot-delta))))
    (define-values (main2 dot2) (add-dot-delta (λ () (add-dot-right/space main1 start-class start-field count))
                                               dot-delta
                                               (if going-down? 
                                                   dot-delta
                                                   (- dot-delta))))
    (define-values (main3 dot3) (add-dot-delta (λ () (add-dot-right main2 finish-class finish-name))
                                               0
                                               (if going-down? 
                                                   (- dot-delta)
                                                   dot-delta)))
    (define-values (main4 dot4) (add-dot-delta (λ () (add-dot-junction main3 dot2 dot3))
                                               0
                                               0))
    
    ;; these last two dots are just there for the delta-less arrowhead
    (define-values (main5 dot5) (add-dot-right main4 finish-class finish-name))
    (define-values (main6 dot6) (add-dot-delta (λ () (add-dot-right main5 finish-class finish-name))
                                               1 ;; just enough to get the arrowhead going the right direction; not enough to see the line
                                               0))
    
    (connect-dots
     #t
     (connect-dots #f main6 dot1 dot2 dot4 dot3)
     dot6 
     dot5)))

(define left-left-reference
  (λ (main0 start-class start-field finish-class finish-name [count 1] 
            #:connect-dots [connect-dots connect-dots]
            #:dot-delta [dot-delta 0])
    (let ([going-down? (let-values ([(_1 start-y) (find-cc main0 start-field)]
                                    [(_2 finish-y) (find-cc main0 finish-name)])
                         (< start-y finish-y))])
      (define-values (main1 dot1) (add-dot-delta (λ () (add-dot-left main0 start-class start-field))
                                                 0
                                                 (if going-down?
                                                     dot-delta
                                                     (- dot-delta))))
      (define-values (main2 dot2) (add-dot-delta (λ () (add-dot-left/space main1 start-class start-field count))
                                                 (- dot-delta)
                                                 (if going-down?
                                                     dot-delta
                                                     (- dot-delta))))
      (define-values (main3 dot3) (add-dot-delta (λ () (add-dot-left main2 finish-class finish-name))
                                                 0
                                                 (if going-down?
                                                     (- dot-delta)
                                                     dot-delta)))
      (define-values (main4 dot4) (add-dot-delta (λ () (add-dot-junction main3 dot2 dot3))
                                                 0
                                                 0))
      (define-values (main5 dot5) (add-dot-left main4 finish-class finish-name))
      (define-values (main6 dot6) (add-dot-delta (λ () (add-dot-left main5 finish-class finish-name))
                                                 -1 ;; just enough to get the arrowhead going the right direction; not enough to see the line
                                                 0))
      
      (connect-dots
       #t
       (connect-dots #f main6 dot1 dot2 dot4 dot3)
       dot6 
       dot5))))

(define left-top-reference
  (λ (main0 start-class start-field finish-class [count 1] #:connect-dots [connect-dots connect-dots])
    (define-values (main1 dot1) (add-dot-left main0 start-class start-field))
    (define-values (main2 dot2) (add-dot-left/space main1 start-class start-field count))
    (define-values (main3 dot3) (add-dot-junction main2 dot2 cc-find finish-class ct-find))
    (connect-dots #t main3 dot1 dot2 dot3)))

(define right-left-reference
  (λ (main0 start-class start-field finish-class finish-name 
            [offset 
             (find-middle main0 start-class rc-find finish-class lc-find)]
            #:connect-dots [connect-dots connect-dots])
    (define-values (main1 dot1) (add-dot-right main0 start-class start-field))
    (define-values (main2 dot2) (add-dot-right/offset main1 start-class start-field offset))
    (define-values (main3 dot3) (add-dot-left main2 finish-class finish-name))
    (define-values (main4 dot4) (add-dot-junction main3 dot2 dot3))
    (connect-dots #t main4 dot1 dot2 dot4 dot3)))

(define left-right-reference
  (λ (main0 start-class start-field finish-class finish-name 
            [offset 
             (- (find-middle main0 start-class lc-find finish-class rc-find))]
            #:connect-dots [connect-dots connect-dots])
    (define-values (main1 dot1) (add-dot-left main0 start-class start-field))
    (define-values (main2 dot2) (add-dot-left/offset main1 start-class start-field offset))
    (define-values (main3 dot3) (add-dot-right main2 finish-class finish-name))
    (define-values (main4 dot4) (add-dot-junction main3 dot2 dot3))
    (connect-dots #t main4 dot1 dot2 dot4 dot3)))

(define (find-middle main p1 find1 p2 find2)
  (let-values ([(x1 y1) (find1 main p1)]
               [(x2 y2) (find2 main p2)])
    (- (/ (+ x1 x2) 2) (min x1 x2))))

(define right-top-reference
  (λ (main0 start-class start-field finish-class [count 1] #:connect-dots [connect-dots connect-dots])
    (define-values (main1 dot1) (add-dot-right main0 start-class start-field))
    (define-values (main2 dot2) (add-dot-right/space main1 start-class start-field count))
    (define-values (main3 dot3) (add-dot-junction main2 dot2 cc-find finish-class ct-find))
    (connect-dots #t main3 dot1 dot2 dot3)))

(define connect-dots-contract (->* (boolean? pict? pict?) () #:rest (listof pict?) (values pict?)))

(provide type-link-color)
(provide/contract
 [field-spec (->* ((or/c #f string?) string?) (string? #:default string?) pict?)]
 [class-name (->* (string?) (#:spacing-word string?) pict?)]
 [class-box (-> pict? (or/c false/c (listof pict?)) (or/c false/c (listof pict?)) pict?)]
 [hierarchy/layout 
  (->* ((cons/c pict? (listof pict?)) (cons/c pict? (listof pict?)))
       (#:top-space 
        integer? 
        #:bottom-space integer? 
        #:vertical-space integer?
        #:every-other-space integer?)
       pict?)]
 [user-type-font (-> string? pict?)]
 [prim-type-font (-> string? pict?)]
 [var-font (-> string? pict?)]
 [normal-font (-> string? pict?)]
 [comment-font (-> string? pict?)]
 
 [hierarchy (-> pict? 
                (cons/c pict? (listof pict?)) 
                (cons/c pict? (listof pict?))
                pict?)]
 [right-right-reference (->* (pict? pict? pict? pict? pict?)
                             (number?
                              #:connect-dots connect-dots-contract
                              #:dot-delta number?)
                             pict?)]
 [left-left-reference (->* (pict? pict? pict? pict? pict?)
                           (number?
                            #:connect-dots connect-dots-contract
                            #:dot-delta number?)
                           pict?)]
 [right-left-reference (->* (pict? pict? pict? pict? pict?)
                            (number?
                             #:connect-dots connect-dots-contract)
                            pict?)]
 [left-right-reference (->* (pict? pict? pict? pict? pict?)
                            (number?
                             #:connect-dots connect-dots-contract)
                            pict?)]
 [left-top-reference (->* (pict? pict? pict? pict?)
                          (number?
                           #:connect-dots connect-dots-contract)
                          pict?)]
 [right-top-reference (->* (pict? pict? pict? pict?)
                           (number?
                            #:connect-dots connect-dots-contract)
                           pict?)]
 
 [dot-edge-spacing number?]
 [connect-dots connect-dots-contract]
 [add-dot-right (-> pict? pict? pict? (values pict? pict?))]
 [add-dot-right/space (-> pict? pict? pict? (values pict? pict?))]
 [add-dot-left (-> pict? pict? pict? (values pict? pict?))]
 [add-dot-left/space (-> pict? pict? pict? (values pict? pict?))]
 [add-dot-junction 
  (case->
   (-> pict? pict? pict? (values pict? pict?))
   (-> pict? 
       pict? (-> pict? pict? (values number? number?))
       pict? (-> pict? pict? (values number? number?))
       (values pict? pict?)))]
 [add-dot-offset (-> pict? pict? number? number? (values pict? pict?))]
 [add-dot (-> pict? number? number? (values pict? pict?))]
 [method-spec 
  (->* (string? string?) 
       (#:body (or/c false/c pict?)) 
       #:rest (listof (or/c false/c string?)) 
       pict?)]
 [java-this (-> pict?)]
 [field-arrowhead-size number?])
