#lang racket/base
(require racket/class
         "../syntax.rkt"
         "const.rkt"
         racket/snip/private/snip
         racket/snip/private/snip-flags
         "private.rkt"
         racket/snip/private/private)

(provide create-mline
         (struct-out mline)
         (struct-out paragraph)
         mline-next
         mline-prev
         (prefix-out
          mline-
          (for-meta
           0
           NIL
           clone-paragraph
           get-line-max-width
           adjust-offsets
           deadjust-offsets
           move-parent!
           rotate-left
           rotate-right
           insert
           delete
           find-line
           find-position
           find-scroll
           find-location
           find-paragraph
           get-line
           get-position
           get-scroll
           get-location
           get-paragraph
           get-paragraph-style
           set-length
           set-scroll-length
           set-height
           calc-line-length
           set-starts-paragraph
           starts-paragraph
           adjust-max-width
           set-width
           scroll-offset
           find-extra-scroll
           mark-recalculate
           adjust-need-calc
           mark-check-flow
           adjust-need-flow
           update-flow
           update-graphics
           get-root
           check-consistent
           first
           last
           get-left-location
           get-right-location
           number)))

(define RED #x1)
(define BLACK #x2)
(define MAX-W-HERE #x4)
(define MAX-W-LEFT #x8)
(define MAX-W-RIGHT #x10)
(define CALC-HERE #x20)
(define CALC-LEFT #x40)
(define CALC-RIGHT #x80)
(define FLOW-HERE #x100)
(define FLOW-LEFT #x200)
(define FLOW-RIGHT #x400)
(define STARTS-PARA #x800)

(define MAX-W-MASK (bitwise-ior MAX-W-HERE MAX-W-LEFT MAX-W-RIGHT))
(define COLOR-MASK (bitwise-ior RED BLACK))
(define CALC-MASK (bitwise-ior CALC-HERE CALC-LEFT CALC-RIGHT))
(define FLOW-MASK (bitwise-ior FLOW-HERE FLOW-LEFT FLOW-RIGHT))

(define-struct mline (prev next parent left right

                           flags paragraph
                           
                           ;; relative values:
                           line pos scroll parno y
                           
                           max-width

                           snip last-snip scroll-snip
                           
                           len numscrolls
                           last-h last-w ;; height/width of last snip in line
                           h w ;; height/width of line
                           bottombase topbase ;; bottom baseline, top baseline (relative)
                           )
  #:mutable #:transparent)

(define NIL #f)

(define (create-mline)
  (make-mline #f #f NIL NIL NIL
              (bitwise-ior BLACK MAX-W-HERE CALC-HERE) #f
              0 0 0 0 0.0
              0.0
              #f #f #f
              0 1
              0.0 0.0
              0.0 0.0
              0.0 0.0))

(set! NIL (create-mline))
(set-mline-parent! NIL NIL)
(set-mline-left! NIL NIL)
(set-mline-right! NIL NIL)

(define (mline-destroy! m)
  ;; Doesn't need to to anything, but this may be helpful for debugging
  (begin
    (set-mline-prev! m 'BAD)
    (set-mline-parent! m 'BAD)
    (set-mline-left! m 'BAD)
    (set-mline-right! m 'BAD)
    (set-mline-flags! m 'BAD)
    (set-mline-paragraph! m 'BAD)
    (set-mline-line! m 'BAD)
    (set-mline-pos! m 'BAD)
    (set-mline-scroll! m 'BAD)
    (set-mline-parno! m 'BAD)
    (set-mline-y! m 'BAD)
    (set-mline-max-width! m 'BAD)
    (set-mline-snip! m 'BAD)
    (set-mline-last-snip! m 'BAD)
    (set-mline-scroll-snip! m 'BAD)
    (set-mline-len! m 'BAD)
    (set-mline-numscrolls! m 'BAD)
    (set-mline-last-h! m 'BAD)
    (set-mline-last-w! m 'BAD)
    (set-mline-h! m 'BAD)
    (set-mline-w! m 'BAD)
    (set-mline-bottombase! m 'BAD)
    (set-mline-topbase! m 'BAD))
  (void))

(define (set-red! mline)
  (set-mline-flags! mline (bitwise-ior RED (bitwise-and (mline-flags mline) 
                                                        (bitwise-not COLOR-MASK)))))
(define (set-black! mline)
  (set-mline-flags! mline (bitwise-ior BLACK (bitwise-and (mline-flags mline) 
                                                          (bitwise-not COLOR-MASK)))))

(define (bit-overlap? a b)
  (not (zero? (bitwise-and a b))))

(define (red? mline)
  (bit-overlap? (mline-flags mline) RED))
(define (black? mline)
  (bit-overlap? (mline-flags mline) BLACK))

(define (starts-paragraph mline)
  (if (bit-overlap? STARTS-PARA (mline-flags mline))
      1
      0))

;; ----------------------------------------

(define-struct paragraph (left-margin-first
                          left-margin
                          right-margin
                          alignment)
  #:mutable)

(define plain-paragraph (make-paragraph 0.0 0.0 0.0 'left))

(define (clone-paragraph p)
  (make-paragraph (paragraph-left-margin-first p)
                  (paragraph-left-margin p)
                  (paragraph-right-margin p)
                  (paragraph-alignment p)))

(define (get-line-max-width p max-width first?)
  (if (max-width . <= . 0)
      max-width
      (max 1
           (- max-width
              (if first?
                  (paragraph-left-margin-first p)
                  (paragraph-left-margin p))
              (paragraph-right-margin p)))))

;; ----------------------------------------

(define (adjust-offsets mline newchild)
  (unless (eq? newchild NIL)
    ;; Adjust relative values:
    (set-mline-line! newchild (- (mline-line newchild) (+ (mline-line mline) 1)))
    (set-mline-pos! newchild (- (mline-pos newchild) (+ (mline-pos mline) (mline-len mline))))
    (set-mline-scroll! newchild (- (mline-scroll newchild) (+ (mline-scroll mline) (mline-numscrolls mline))))
    (set-mline-y! newchild (- (mline-y newchild) (+ (mline-y mline) (mline-h mline))))
    (set-mline-parno! newchild (- (mline-parno newchild) (+ (mline-parno mline) (starts-paragraph mline))))))

(define (deadjust-offsets mline oldchild)
  (unless (eq? oldchild NIL)
    ;; Adjust relative values:
    (set-mline-line! oldchild (+ (mline-line oldchild) (+ (mline-line mline) 1)))
    (set-mline-pos! oldchild (+ (mline-pos oldchild) (+ (mline-pos mline) (mline-len mline))))
    (set-mline-scroll! oldchild (+ (mline-scroll oldchild) (+ (mline-scroll mline) (mline-numscrolls mline))))
    (set-mline-y! oldchild (+ (mline-y oldchild) (+ (mline-y mline) (mline-h mline))))
    (set-mline-parno! oldchild (+ (mline-parno oldchild) (+ (mline-parno mline) (starts-paragraph mline))))))

(define (move-parent! v x root-box)
  ;; replace v with x
  (let ([parent (mline-parent v)])
    (set-mline-parent! x parent) ; x can be NIL!
    (cond
     [(eq? parent NIL)
      (set-box! root-box x)]
     [(eq? v (mline-left parent))
      (set-mline-left! parent x)]
     [else
      (set-mline-right! parent x)])))

(define (rotate-left mline root-box)
  (let ([oldright (mline-right mline)])
    (deadjust-offsets mline oldright)
  
    (let ([right (mline-left oldright)])
      (set-mline-right! mline right)
      (unless (eq? right NIL)
        (set-mline-parent! right mline)))
    
    (move-parent! mline oldright root-box)

    (set-mline-left! oldright mline)
    (set-mline-parent! mline oldright)

    (adjust-max-width mline)
    (adjust-need-calc mline)
    (adjust-need-flow mline)
    (adjust-max-width oldright)
    (adjust-need-calc oldright)
    (adjust-need-flow oldright)))

(define (rotate-right mline root-box)
  (let ([oldleft (mline-left mline)])
    (adjust-offsets oldleft mline)
  
    (let ([left (mline-right oldleft)])
      (set-mline-left! mline left)
      (unless (eq? left NIL)
        (set-mline-parent! left mline)))
    
    (move-parent! mline oldleft root-box)

    (set-mline-right! oldleft mline)
    (set-mline-parent! mline oldleft)

    (adjust-max-width mline)
    (adjust-need-calc mline)
    (adjust-need-flow mline)
    (adjust-max-width oldleft)
    (adjust-need-calc oldleft)
    (adjust-need-flow oldleft)))

(define (insert mline root-box before?)
  (let ([newline (create-mline)])
    (if (eq? (unbox root-box) NIL)
        (begin
          (set-box! root-box newline)
          newline)
        (begin
          (set-red! newline)

          (if before?
              (let ([prev (mline-prev mline)])
                (set-mline-prev! newline prev)
                (when prev
                  (set-mline-next! prev newline))
                (set-mline-next! newline mline)
                (set-mline-prev! mline newline))
              (let ([next (mline-next mline)])
                (set-mline-prev! newline mline)
                (set-mline-next! newline next)
                (when next
                  (set-mline-prev! next newline))
                (set-mline-next! mline newline)))

          (let ([node
                 (if before?
                     (let ([left (mline-left mline)])
                       (if (eq? left NIL)
                           (begin
                             (set-mline-left! mline newline)
                             mline)
                           (let loop ([node left])
                             (let ([right (mline-right node)])
                               (if (not (eq? right NIL))
                                   (loop right)
                                   (begin
                                     (set-mline-right! node newline)
                                     node))))))
                     (let ([right (mline-right mline)])
                       (if (eq? right NIL)
                           (begin
                             (set-mline-right! mline newline)
                             mline)
                           (let loop ([node right])
                             (let ([left (mline-left node)])
                               (if (not (eq? left NIL))
                                   (loop left)
                                   (begin
                                     (set-mline-left! node newline)
                                     node)))))))])
            (set-mline-parent! newline node)
            (adjust-need-calc node #t))

          (let loop ([node newline])
            (let ([parent (mline-parent node)])
              (unless (eq? parent NIL)
                (when (eq? node (mline-left parent))
                  (deadjust-offsets newline parent))
                (loop parent))))

          (let loop ([node newline])
            (when (and (not (eq? node (unbox root-box)))
                       (red? (mline-parent node)))
              (let ([parent (mline-parent node)])
                (if (eq? parent (mline-left (mline-parent parent)))
                    (let ([v (mline-right (mline-parent parent))])
                      (if (red? v)
                          (begin
                            (set-black! parent)
                            (set-black! v)
                            (let ([node (mline-parent parent)])
                              (set-red! node)
                              (loop node)))
                          (let* ([node (if (eq? node (mline-right parent))
                                           (begin
                                             (rotate-left parent root-box)
                                             parent)
                                           node)]
                                 [parent (mline-parent node)])
                            (set-black! parent)
                            (let ([node (mline-parent parent)])
                              (set-red! node)
                              (rotate-right node root-box)
                              (loop node)))))
                    (let ([v (mline-left (mline-parent parent))])
                      (if (red? v)
                          (begin
                            (set-black! parent)
                            (set-black! v)
                            (let ([node (mline-parent parent)])
                              (set-red! node)
                              (loop node)))
                          (let* ([node (if (eq? node (mline-left parent))
                                           (begin
                                             (rotate-right parent root-box)
                                             parent)
                                           node)]
                                 [parent (mline-parent node)])
                            (set-black! parent)
                            (let ([node (mline-parent parent)])
                              (set-red! node)
                              (rotate-left node root-box)
                              (loop node)))))))))

          (set-black! (unbox root-box))

          newline))))

(define (delete mline root-box)

  ;; adjust ancestor offsets
  (let ([len (mline-len mline)]
        [numscrolls (mline-numscrolls mline)]
        [h (mline-h mline)])
    (let loop ([v mline])
      (let ([parent (mline-parent v)])
        (unless (eq? parent NIL)
          (if (eq? v (mline-right parent))
              (loop parent)
              (let ([v parent])
                (set-mline-line! v (- (mline-line v) 1))
                (set-mline-pos! v (- (mline-pos v) len))
                (set-mline-scroll! v (- (mline-scroll v) numscrolls))
                (set-mline-y! v (- (mline-y v) h))
                (set-mline-parno! v (- (mline-parno v) (starts-paragraph mline)))
                (loop v)))))))

  (let ([v (if (or (eq? (mline-left mline) NIL)
                   (eq? (mline-right mline) NIL))
               mline
               (let ([v (mline-next mline)])
                 (let loop ([x v])
                   (unless (eq? mline (mline-parent x))
                     (let ([parent (mline-parent x)])
                       (if (eq? x (mline-right parent))
                           (loop parent)
                           (let ([x parent])
                             (set-mline-line! x (- (mline-line x) 1))
                             (set-mline-pos! x (- (mline-pos x) (mline-len v)))
                             (set-mline-scroll! x (- (mline-scroll x) (mline-numscrolls v)))
                             (set-mline-y! x (- (mline-y x) (mline-h v)))
                             (set-mline-parno! x (- (mline-parno x) (starts-paragraph v)))
                             (loop x))))))
                 v))])

    (let ([x (if (eq? (mline-left v) NIL)
                 (mline-right v)
                 (mline-left v))])
      (move-parent! v x root-box)

      (let ([was-black? (black? v)])
        
        (if (not (eq? v mline))
            (let ([oldparent (mline-parent v)])
              (if (black? mline)
                  (set-black! v)
                  (set-red! v))
              
              (let ([left (mline-left mline)])
                (set-mline-left! v left)
                (unless (eq? left NIL)
                  (set-mline-parent! left v)))
              (let ([right (mline-right mline)])
                (set-mline-right! v right)
                (unless (eq? right NIL)
                  (set-mline-parent! right v)))
              (move-parent! mline v root-box)
              (let ([prev (mline-prev mline)])
                (set-mline-prev! v prev)
                (when prev
                  (set-mline-next! prev v)))

              (set-mline-line! v (mline-line mline))
              (set-mline-pos! v (mline-pos mline))
              (set-mline-scroll! v (mline-scroll mline))
              (set-mline-y! v (mline-y mline))
              (set-mline-parno! v (mline-parno mline))

              (adjust-max-width oldparent #t)
              (adjust-need-calc oldparent #t)
              (adjust-need-flow oldparent #t)
              
              (adjust-max-width v #t)
              (adjust-need-calc v #t)
              (adjust-need-flow v #t)

              (when (eq? (mline-parent x) mline)
                (set-mline-parent! x v)))
            (begin
              (let ([prev (mline-prev mline)]
                    [next (mline-next mline)])
                (when prev
                  (set-mline-next! prev next))
                (when next
                  (set-mline-prev! next prev)))))

        (when was-black?
          ;; fixup 
          (let loop ([x x])
            (if (and (not (eq? x (unbox root-box)))
                     (black? x))
                (let ([parent (mline-parent x)])
                  (if (eq? x (mline-left parent))
                      (let* ([z (mline-right parent)]
                             [z (if (red? z)
                                    (begin
                                      (set-black! z)
                                      (set-red! parent)
                                      (rotate-left parent root-box)
                                      (mline-right (mline-parent x)))
                                    z)]
                             [x (if (and (black? (mline-left z))
                                         (black? (mline-right z)))
                                    (begin
                                      (set-red! z)
                                      (mline-parent x))
                                    (let ([z (if (black? (mline-right z))
                                                 (begin
                                                   (set-black! (mline-left z))
                                                   (set-red! z)
                                                   (rotate-right z root-box)
                                                   (mline-right (mline-parent x)))
                                                 z)])
                                      (if (red? (mline-parent x))
                                          (set-red! z)
                                          (set-black! z))
                                      (set-black! (mline-parent x))
                                      (set-black! (mline-right z))
                                      (rotate-left (mline-parent x) root-box)
                                      (unbox root-box)))])
                        (loop x))
                      (let* ([z (mline-left parent)]
                             [z (if (red? z)
                                    (begin
                                      (set-black! z)
                                      (set-red! parent)
                                      (rotate-right parent root-box)
                                      (mline-left (mline-parent x)))
                                    z)]
                             [x (if (and (black? (mline-right z))
                                         (black? (mline-left z)))
                                    (begin
                                      (set-red! z)
                                      (mline-parent x))
                                    (let ([z (if (black? (mline-left z))
                                                 (begin
                                                   (set-black! (mline-right z))
                                                   (set-red! z)
                                                   (rotate-left z root-box)
                                                   (mline-left (mline-parent x)))
                                                 z)])
                                      (if (red? (mline-parent x))
                                          (set-red! z)
                                          (set-black! z))
                                      (set-black! (mline-parent x))
                                      (set-black! (mline-left z))
                                      (rotate-right (mline-parent x) root-box)
                                      (unbox root-box)))])
                        (loop x))))
                (set-black! x)))))))

  ;; In case we set the parent of NIL:
  (set-mline-parent! NIL NIL)

  (mline-destroy! mline))

;; ----------------------------------------

(define (search mline v v-sel size-sel)
  (let loop ([v v][node mline][prev #f])
    (if (not (eq? node NIL))
        (let ([v2 (v-sel node)]
              [size (size-sel node)])
          (cond
           [(v . < . v2)
            (loop v (mline-left node) node)]
           [(v . >= . (+ v2 size))
            (loop (- v (+ v2 size))
                  (mline-right node) node)]
           [else node]))
        prev)))

 (define (find-line mline line)
  (search mline line mline-line (lambda (mline) 1)))

(define (find-position mline pos)
  (search mline pos mline-pos mline-len))

(define (find-scroll mline scroll)
  (search mline scroll mline-scroll mline-numscrolls))

(define (find-location mline y)
  (search mline y mline-y mline-h))

(define (find-paragraph mline parno)
  (search mline parno mline-parno starts-paragraph))

;; ----------------------------------------

(define (sum mline v-sel size-sel)
  (let loop ([node mline][v (v-sel mline)])
    (let ([parent (mline-parent node)])
      (if (not (eq? parent NIL))
          (if (eq? node (mline-left parent))
              (loop parent v)
              (loop parent (+ v (v-sel parent) (size-sel parent))))
          v))))

(define (get-line mline)
  (sum mline mline-line (lambda (mline) 1)))

(define (get-position mline)
  (sum mline mline-pos mline-len))

(define (get-scroll mline)
  (sum mline mline-scroll mline-numscrolls))

(define (get-location mline)
  (sum mline mline-y mline-h))

(define (get-paragraph mline)
  (+ (sum mline mline-parno starts-paragraph)
     (sub1 (starts-paragraph mline))))

(define (get-paragraph-style mline [first-box #f])
  (if (bit-overlap? (mline-flags mline) STARTS-PARA)
      (begin
        (when first-box (set-box! first-box #t))
        (mline-paragraph mline))
      (begin
        (when first-box (set-box! first-box #f))
        (let ([root (get-root mline)]
              [p (get-paragraph mline)])
          (let ([pstart (find-paragraph root p)])
            (mline-paragraph pstart))))))

;; ----------------------------------------

(define (adjust mline new-val val-sel val-mut! sel mut!)
  (define delta (- new-val (val-sel mline)))
  (define val-changed? 
    (cond
      [(= (val-sel mline) new-val) #f]
      [else
       (val-mut! mline new-val)
       #t]))
  (or (let loop ([node mline])
        (let ([parent (mline-parent node)])
          (cond
            [(eq? parent NIL) #f]
            [else
             (if (eq? node (mline-left parent))
                 (cond
                   [(= delta 0)
                    (loop parent)]
                   [else
                    (mut! parent (+ delta (sel parent)))
                    (loop parent)
                    #t])
                 (loop parent))])))
      val-changed?))

(define (set-length mline len)
  (adjust mline
          len mline-len set-mline-len! 
          mline-pos set-mline-pos!))

(define (set-scroll-length mline numscrolls)
  (adjust mline
          numscrolls mline-numscrolls set-mline-numscrolls!
          mline-scroll set-mline-scroll!))

(define (set-height mline h)
  (adjust mline
          h mline-h set-mline-h!
          mline-y set-mline-y!))

(define (calc-line-length mline)
  (let ([l
         (let ([nexts (snip->next (mline-last-snip mline))])
           (let loop ([asnip (mline-snip mline)][l 0])
             (if (eq? asnip nexts)
                 l
                 (let ([l (+ l (snip->count asnip))])
                   (when (has-flag? (snip->flags asnip) WIDTH-DEPENDS-ON-X)
                     (send asnip size-cache-invalid))
                   (loop (snip->next asnip) l)))))])

    (when (not (= l (mline-len mline)))
      (set-length mline l)))

  (let ([next (mline-next mline)])
    (cond
     [(and next
           (has-flag? (snip->flags (mline-last-snip mline))
                      HARD-NEWLINE))
      (when (zero? (starts-paragraph next))
        (set-starts-paragraph next #t))]
     [next
      (when (starts-paragraph next)
        (set-starts-paragraph next #f))]))
  
  (let ([prev (mline-prev mline)])
    (cond
     [(or (not prev)
          (has-flag? (snip->flags (mline-last-snip prev))
                     HARD-NEWLINE))
      (when (zero? (starts-paragraph mline))
        (set-starts-paragraph mline #t))]
     [(positive? (starts-paragraph mline))
      (set-starts-paragraph mline #f)])))

(define (set-starts-paragraph mline starts?)
  (unless (= (if starts? 1 0) (starts-paragraph mline))
    (if starts?
        (begin
          (set-mline-flags! mline
                            (bitwise-ior (mline-flags mline) STARTS-PARA))
          (unless (mline-paragraph mline)
            (set-mline-paragraph! mline plain-paragraph)))
        (begin
          (set-mline-flags! mline (- (mline-flags mline) STARTS-PARA))
          (set-mline-paragraph! mline #f)))

    (let loop ([node mline])
      (let ([parent (mline-parent node)])
        (unless (eq? parent NIL)
          (when (eq? node (mline-left parent))
            (set-mline-parno! parent (+ (mline-parno parent)
                                        (if starts? 1 -1))))
          (loop parent))))))

;; ------------------------------------------------------------

(define (adjust-max-width mline [recur? #f])
  (define anything-changed? #f)
  (unless (eq? mline NIL)
    (let loop ([node mline])
      (define old (bitwise-and (mline-flags node) MAX-W-MASK))
      (define-values (new-max-width which)
        (cond
          [(and (not (eq? (mline-right node) NIL))
                ((mline-max-width (mline-right node)) . > . (mline-w node))
                (or (eq? (mline-left node) NIL)
                    ((mline-max-width (mline-right node)) . > . (mline-max-width (mline-left node)))))
           (values (mline-max-width (mline-right node)) MAX-W-RIGHT)]
          [(and (not (eq? (mline-left node) NIL))
                ((mline-max-width (mline-left node)) . > . (mline-w node)))
           (values (mline-max-width (mline-left node)) MAX-W-LEFT)]
          [else
           (values (mline-w node) MAX-W-HERE)]))
      (unless (= (mline-max-width node) new-max-width)
        (set! anything-changed? #t)
        (set-mline-max-width! node new-max-width))
      (unless (= old which)
        (set-mline-flags! node
                          (bitwise-ior
                           (bitwise-and (mline-flags node)
                                        (bitwise-not MAX-W-MASK))
                           which)))
      (when recur?
        (let ([parent (mline-parent node)])
          (unless (eq? parent NIL)
            (loop parent))))))
  anything-changed?)

(define (set-width mline w)
  (define w-same? (= (mline-w mline) w))
  (unless w-same? (set-mline-w! mline w))
  (or (adjust-max-width mline #t) (not w-same?)))

;; ----------------------------------------

(define (scroll-offset mline p)
  (let ([scroll-snip (mline-scroll-snip mline)])
    (cond
     [(not scroll-snip)
      0.0]
     [(p . >= . (mline-numscrolls mline))
      (mline-h mline)]
     [else
      (send scroll-snip get-scroll-step-offset p)])))

(define (find-extra-scroll mline y)
  (cond
   [(y . >= . (mline-h mline))
    (mline-numscrolls mline)]
   [(y . <= . 0)
    0]
   [else
    (let ([scroll-snip (mline-scroll-snip mline)])
      (if (not scroll-snip)
          0
          (send scroll-snip find-scroll-step y)))]))

;; ----------------------------------------

(define (mark-need mline HERE recur)
  (unless (bit-overlap? (mline-flags mline) HERE)
    (set-mline-flags! mline (bitwise-ior (mline-flags mline) HERE))
    (let ([parent (mline-parent mline)])
      (unless (eq? parent NIL)
        (recur parent #t)))))

(define (adjust-need-flag mline MASK HERE RIGHT LEFT recur?)
  (let loop ([node mline])
    (let ([old (bitwise-and (mline-flags node) MASK)])
      (let* ([which (bitwise-and old HERE)]
             [which (if (and (not (eq? (mline-right node) NIL))
                             (bit-overlap? (mline-flags (mline-right node)) MASK))
                        (bitwise-ior which RIGHT)
                        which)]
             [which (if (and (not (eq? (mline-left node) NIL))
                             (bit-overlap? (mline-flags (mline-left node)) MASK))
                        (bitwise-ior which LEFT)
                        which)])
        (when (not (= old which))
          (set-mline-flags! node
                            (bitwise-ior
                             (bitwise-and (mline-flags node)
                                          (bitwise-not MASK))
                             which))
          (when recur?
            (let ([parent (mline-parent node)])
              (unless (eq? parent NIL)
                (loop parent)))))))))

(define (mark-recalculate mline)
  (mark-need mline CALC-HERE adjust-need-calc))

(define (adjust-need-calc mline [recur? #f])
  (adjust-need-flag mline CALC-MASK CALC-HERE CALC-RIGHT CALC-LEFT recur?))

(define (mark-check-flow mline)
  (mark-need mline FLOW-HERE adjust-need-flow))

(define (adjust-need-flow mline [recur? #f])
  (adjust-need-flag mline FLOW-MASK FLOW-HERE FLOW-RIGHT FLOW-LEFT recur?))

;; ----------------------------------------

(define (get-root mline)
  (let ([parent (mline-parent mline)])
    (if (not (eq? parent NIL))
        (get-root parent)
        mline)))

;; ----------------------------------------

(define (check-consistent root)
  (unless (black? root)
    (error "root is not black"))
  (let ([l1 (let loop ([mline root])
              (if (eq? mline NIL)
                  null
                  (begin
                    (when (red? mline)
                      (unless (black? (mline-left mline))
                        (error "red left child is not black"))
                      (unless (black? (mline-right mline))
                        (error "red right child is not black")))
                    (unless (or (eq? (mline-left mline) NIL)
                                (eq? (mline-parent (mline-left mline)) mline))
                      (error "left and up doesn't work"))
                    (unless (or (eq? (mline-right mline) NIL)
                                (eq? (mline-parent (mline-right mline)) mline))
                      (error "right and up doesn't work"))
                    (append
                     (loop (mline-left mline))
                     (list mline)
                     (loop (mline-right mline))))))]
        [l2 (let loop ([mline root])
              (let ([prev (mline-prev mline)])
                (if prev
                    (begin
                      (unless (eq? (mline-next prev) mline)
                        (error "back doesn't go forward"))
                      (loop prev))
                    (let loop ([mline mline])
                      (if mline
                          (cons mline (loop (mline-next mline)))
                          null)))))])
    (unless (= (length l1) (length l2))
      (error 'check-consistent "different lengths: ~s ~s" (length l1) (length l2)))
    (unless (andmap eq? l1 l2)
      (error "different elems")))
  (let loop ([mline root])
    (if (eq? mline NIL)
        0
        (let ([left (loop (mline-left mline))]
              [right (loop (mline-right mline))])
          (unless (= left right)
            (error "different black counts:" left right))
          (if (black? mline)
              (+ 1 left)
              left))))
  (unless (eq? (mline-parent root) NIL)
    (error "root has non-NIL parent"))
  (unless (black? NIL)
    (error "NIL is non-black"))
  (unless (eq? NIL (mline-parent NIL))
    (error "NIL parent changed"))
  (unless (eq? NIL (mline-left NIL))
    (error "NIL left changed"))
  (unless (eq? NIL (mline-left NIL))
    (error "NIL right changed")))

#|

Debugging tools:

(define (draw p)
  (for-each (lambda (l)
              (display l)
              (newline))
            (paint p)))

(define (paint p)
  (if (eq? p NIL)
      '("*")
      (let ([l (paint (mline-left p))]
            [r (paint (mline-right p))])
        (let ([ll (string-length (car l))]
              [rl (string-length (car r))]
              [s ((if (red? p) string-upcase values) (format "~s" (mline-sym p)))])
          (cons
           (string-append (make-string ll #\space)
                          s
                          (make-string rl #\space))
           (let loop ([l l][r r])
             (cond
              [(null? l) (if (null? r)
                             null
                             (map (lambda (r)
                                    (string-append 
                                     (make-string (+ ll (string-length s)) #\space)
                                     r))
                                  r))]
              [(null? r) (map (lambda (l)
                                    (string-append 
                                     l
                                     (make-string (+ rl (string-length s)) #\space)))
                                  l)]
              [else (cons (string-append (car l)
                                         (make-string (string-length s) #\space)
                                         (car r))
                          (loop (cdr l) (cdr r)))])))))))

(define (find? root m)
  (or (eq? root m)
      (if (eq? root NIL)
          #f
          (or (find? (mline-left root) m)
              (find? (mline-right root) m)))))

|#

;; ------------------------------------------------------------

(define (update-flow mline root-box media max-width dc notify-delete notify-insert)
  (define (flow-left)
    (if (bit-overlap? (mline-flags mline) FLOW-LEFT)
        (if (and (not (eq? (mline-left mline) NIL))
                 (update-flow (mline-left mline) root-box media max-width dc
                              notify-delete notify-insert))
            #t
            (begin
              (set-mline-flags! mline (- (mline-flags mline) FLOW-LEFT))
              (flow-here)))
        (flow-here)))
  (define (flow-here)
    (if (bit-overlap? (mline-flags mline) FLOW-HERE)
        (begin
          (set-mline-flags! mline (- (mline-flags mline) FLOW-HERE))
          (let* ([first-line (box #f)]
                 [para (get-paragraph-style mline first-line)]
                 [line-max-width (get-line-max-width para max-width (unbox first-line))])
            (assert (send media consistent-snip-lines 'pre-check-flow))
            (if (send media check-flow line-max-width dc (get-location mline) (get-position mline) (mline-snip mline))
                (do-flow)
                (flow-right))))
        (flow-right)))
  (define (flow-right)
    (if (bit-overlap? (mline-flags mline) FLOW-RIGHT)
        (if (and (not (eq? (mline-right mline) NIL))
                 (update-flow (mline-right mline) root-box media max-width dc
                              notify-delete notify-insert))
            #t
            (begin
              (set-mline-flags! mline (- (mline-flags mline) FLOW-RIGHT))
              #f))
        #f))
  (define (do-flow)
    (let loop ([asnip (mline-snip mline)])
      (if (eq? asnip (mline-last-snip mline))
          (begin
            (do-extend-line mline asnip)
            (assert (send media consistent-snip-lines 'post-do-extend-line))
            #t)
          (if (has-flag? (snip->flags asnip) NEWLINE)
              (begin
                (do-new-line asnip)
                (assert (send media consistent-snip-lines 'post-do-new-line))
                #t)
              (begin
                (set-snip-line! asnip mline)
                (loop (snip->next asnip)))))))
  (define (do-new-line asnip)
    ;; items pushed to next line or new line was inserted;
    ;; current line now ends with ansip (which used to be in the middle of the current line)
    (let ([next (mline-next mline)])
      (let ([nextsnip (if next
                          (let loop ([nextsnip (snip->next asnip)])
                            (if (and nextsnip
                                     (not (eq? nextsnip (mline-last-snip next)))
                                     (not (has-flag? (snip->flags nextsnip) NEWLINE)))
                                (loop (snip->next nextsnip))
                                nextsnip))
                          #f)])
        (if (or (not next)
                (not (eq? nextsnip (mline-last-snip next))))
            ;; it was a new line
            (let ([newline (insert mline root-box #f)])
              (set-mline-snip! newline (snip->next asnip))
              (set-mline-last-snip! newline (mline-last-snip mline))
              (set-mline-last-snip! mline asnip)

              (snips-to-line! newline)

              (notify-insert newline))
            ;; some of this line pushed to next line --- or maybe multiple lines pushed
            ;;  together into a later line
            (begin
              (set-mline-last-snip! mline asnip)
              (set-snip-line! asnip mline)
              
              (let ([nextsnip (snip->next asnip)])
                (set-mline-snip! next nextsnip)
                (do-extend-line next nextsnip))))

        (calc-line-length mline)
        (mark-recalculate mline))))
  (define (snips-to-line! next)
    (let ([nextsnip (snip->next (mline-last-snip next))])
      (let loop ([asnip (mline-snip next)])
        (unless (eq? asnip nextsnip)
          (set-snip-line! asnip next)
          (loop (snip->next asnip)))))
    (mark-check-flow next)
    (mark-recalculate next)
    (calc-line-length next))
  (define (maybe-delete-line! asnip mline)
    (if (and (mline-next mline)
             (eq? asnip (mline-last-snip (mline-next mline))))
        ;; a line was deleted
        (let ([next (mline-next mline)])
          (delete next root-box) 
          (notify-delete next)
          #t)
        #f))
  (define (do-extend-line mline asnip)
    ;; this line was extended
    (let ([asnip
           (if asnip
               (let loop ([asnip asnip])
                 (if (and (snip->next asnip)
                          (not (has-flag? (snip->flags asnip) NEWLINE)))
                     (begin
                       (set-snip-line! asnip mline)
                       (maybe-delete-line! asnip mline)
                       (loop (snip->next asnip)))
                     (begin
                       (maybe-delete-line! asnip mline)
                       (set-mline-last-snip! mline asnip)
                       asnip)))
               (begin
                 (set-mline-last-snip! mline (send media get-s-last-snip))
                 (let loop ()
                   (let ([next (mline-next mline)])
                     (when next
                       (delete next root-box)
                       (notify-delete delete)
                       (loop))))
                 #f))])

      (set-snip-line! (mline-last-snip mline) mline)

      (when (mline-next mline)
        (let ([asnip (snip->next asnip)]
              [next (mline-next mline)])
          (when (or (not (eq? (mline-snip next) asnip))
                    (not (has-flag? (snip->flags (mline-last-snip next)) NEWLINE)))
            ;; Effect can propagate to more lines, merging the
            ;; next several. (Handle prefixing the remains of the source of
            ;; the extension to this line onto the next line. Implemented
            ;; as the next line eating the next->next line.) 
            (set-mline-snip! next asnip)
            (let ([asnip
                   (let loop ([asnip asnip])
                     (if (and (snip->next asnip)
                              (not (has-flag? (snip->flags asnip) NEWLINE)))
                         (begin
                           (maybe-delete-line! asnip next)
                           (set-snip-line! asnip next)
                           (loop (snip->next asnip)))
                         asnip))])
              (set-snip-line! asnip next)
              (set-mline-last-snip! next asnip)
              (when (mline-next next)
                (unless (maybe-delete-line! asnip next)
                  (set-mline-snip! (mline-next next) (snip->next asnip))))
              (calc-line-length next)
              (mark-recalculate next)
              (mark-check-flow next)))))

      (calc-line-length mline)
      (mark-recalculate mline)))
  ;; Try left first....
  (flow-left))

;; ----------------------------------------
(define (update-graphics mline media dc padding-l padding-t max-line-width)
  (define (update-left)
    (cond
      [(and (bit-overlap? (mline-flags mline) CALC-LEFT)
            (not (eq? (mline-left mline) NIL)))
       (update-graphics (mline-left mline) media dc padding-l padding-t max-line-width)]
      [else (values #f #f)]))
  (define (update-here)
    (cond
      [(bit-overlap? (mline-flags mline) CALC-HERE)
       (let ([y (+ (get-location mline) padding-t)]
             [nextsnip (snip->next (mline-last-snip mline))]
             [sizing-changed? #f])
         (let loop ([asnip (mline-snip mline)]
                    [maxbase 0.0]
                    [maxdescent 0.0]
                    [maxspace 0.0]
                    [maxantidescent 0.0]
                    [maxantispace 0.0]
                    [totalwidth padding-l]
                    [maxscroll 1]
                    [scroll-snip #f]
                    [last-w 0.0]
                    [last-h 0.0])
           (if (not (eq? asnip nextsnip))
               (let-boxes ([w 0.0]
                           [h 0.0]
                           [descent 0.0]
                           [space 0.0])
                          (send asnip get-extent dc totalwidth y w h descent space #f #f)
                          (let* ([align (send (snip->style asnip) get-alignment)]
                                 [scroll (send asnip get-num-scroll-steps)]
                                 [maxbase (max maxbase (- h descent space))]
                                 [maxdescent (if (eq? align 'bottom)
                                                 (max maxdescent descent)
                                                 maxdescent)]
                                 [maxantispace (if (eq? align 'bottom)
                                                   maxantispace
                                                   (max maxantispace (- h space)))]
                                 [maxspace (if (eq? align 'top)
                                               (max maxspace space)
                                               maxspace)]
                                 [maxantidescent (if (eq? align 'top)
                                                     maxantidescent
                                                     (max maxantidescent (- h descent)))]
                                 [scroll-snip (if (scroll . > . maxscroll)
                                                  asnip
                                                  scroll-snip)]
                                 [maxscroll (max maxscroll scroll)]
                                 [totalwidth (+ w totalwidth)])
                            (loop (snip->next asnip)
                                  maxbase maxdescent maxspace maxantidescent maxantispace
                                  totalwidth maxscroll scroll-snip
                                  w h)))
               (let ([maxspace (max maxspace (- maxantidescent maxbase))]
                     [maxdescent (max maxdescent (- maxantispace maxbase))])
                 (unless (eq? (mline-scroll-snip mline) scroll-snip)
                   (set! sizing-changed? #t)
                   (set-mline-scroll-snip! mline scroll-snip))
                 (unless (= (mline-last-h mline) last-h)
                   (set! sizing-changed? #t)
                   (set-mline-last-h! mline last-h))
                 (unless (= (mline-last-w mline) last-w)
                   (set! sizing-changed? #t)
                   (set-mline-last-w! mline last-w))
                 (unless (= (mline-topbase mline) maxspace)
                   (set! sizing-changed? #t)
                   (set-mline-topbase! mline maxspace))
                 (let ([bottombase (+ maxspace maxbase)])
                   (unless (= (mline-bottombase mline) bottombase)
                     (set! sizing-changed? #t)
                     (set-mline-bottombase! mline bottombase)))
                 (let ([maxh (+ maxbase 
                                maxdescent
                                maxspace
                                (send media get-s-line-spacing))]
                       [bigwidth (+ (if ((mline-w mline) . > . totalwidth)
                                        (mline-w mline)
                                        totalwidth)
                                    CURSOR-WIDTH
                                    (let-boxes ([is-first? #f]
                                                [para #f])
                                               (set-box! para (get-paragraph-style mline is-first?))
                                               (if is-first?
                                                   (paragraph-left-margin-first para)
                                                   (paragraph-left-margin para))))])
                   (when (set-width mline (- totalwidth padding-l))
                     (set! sizing-changed? #t))
                   (unless (= maxscroll (mline-numscrolls mline))
                     (when (set-scroll-length mline maxscroll)
                       (set! sizing-changed? #t)))
                   (define (send-refresh-box w h)
                     (define x-delta
                       (case (if max-line-width
                                 (let-boxes ([is-first? #f]
                                             [para #f])
                                            (set-box! para (get-paragraph-style mline is-first?))
                                            (paragraph-alignment para))
                                 'left)
                         [(left) 0]
                         [(right) (max 0 (- max-line-width w))]
                         [else (max 0 (/ (- max-line-width w) 2))]))
                     (send media refresh-box (+ padding-l x-delta) y w h))
                   (if (= maxh (mline-h mline))
                       (send-refresh-box bigwidth maxh)
                       (begin
                         (when (set-height mline maxh)
                           (set! sizing-changed? #t))
                         (let ([bigwidth (max 1e5 ;; really want viewable width, but > ok
                                              (send media get-s-total-width))]
                               [bigheight (+ maxh (send media get-s-total-height))])
                           (send-refresh-box bigwidth bigheight))))))))
         (values sizing-changed? #t))]
      [else (values #f #f)]))
  (define (update-right)
    (cond
      [(and (bit-overlap? (mline-flags mline) CALC-RIGHT)
            (not (eq? (mline-right mline) NIL)))
       (update-graphics (mline-right mline) media dc padding-l padding-t max-line-width)]
      [else
       (values #f #f)]))

  (define-values (sizing-left? left?) (update-left))
  (define-values (sizing-here? here?) (update-here))
  (define-values (sizing-right? right?) (update-right))
  (set-mline-flags! mline (bitwise-and
                           (mline-flags mline)
                           (bitwise-not CALC-MASK)))
  (values (or sizing-left? sizing-here? sizing-right?)
          (or left? here? right?)))

;; ------------------------------------------------------------

(define (number mline)
  (add1 (get-line (last mline))))

(define (first mline)
  (let ([left (mline-left mline)])
    (if (eq? left NIL)
        mline
        (first left))))
    
(define (last mline)
  (let ([right (mline-right mline)])
    (if (eq? right NIL)
        mline
        (last right))))

;; ------------------------------------------------------------

(define (get-left-location mline max-width)
  (let-values ([(para left)
                (if (bit-overlap? (mline-flags mline) STARTS-PARA)
                    (let ([para (mline-paragraph mline)])
                      (values para
                              (paragraph-left-margin-first para)))
                    (let ([para (get-paragraph-style mline)])
                      (values para
                              (paragraph-left-margin para))))])
    (if (and (max-width . > . 0)
             (not (eq? (paragraph-alignment para) 'left)))
        (let ([delta (max 0 (- max-width (mline-w mline)))])
          (if (eq? (paragraph-alignment para) 'right)
              (+ left delta)
              (+ left (/ delta 2))))
        left)))

(define (get-right-location mline max-width)
  (+ (get-left-location mline max-width) (mline-w mline)))
