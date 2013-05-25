#lang racket/base
(require racket/class
         racket/file
         (for-syntax racket/base)
         racket/draw
         racket/draw/private/syntax
         racket/draw/private/font-syms
         racket/snip/private/private
         "prefs.rkt")

(provide mult-color<%>
         add-color<%>
         style-delta%
         style<%>
         style-list%
         the-style-list
         setup-style-reads-writes
         done-style-reads-writes
         read-styles-from-file
         write-styles-to-file)

;; for contracts
(define editor-stream-out% object%)

(define default-size 
  (or (get-preference* 'GRacket:default-font-size)
      (case (system-type)
        [(windows) 10]
        [else 12])))

(define black-color (make-object color% 0 0 0))

(defclass mult-color% object%
  (define r 0.0)
  (define g 0.0)
  (define b 0.0)

  (super-new)

  (def/public (get [box? rb] [box? gb] [box? bb])
    (set-box! rb r)
    (set-box! gb g)
    (set-box! bb b))

  (def/public (set [real? rf] [real? gf] [real? bf])
    (set! r rf)
    (set! g gf)
    (set! b bf))

  (def/public (get-r) r)
  (def/public (get-g) g)
  (def/public (get-b) b)

  (def/public (set-r [real? v])
    (set! r v))
  (def/public (set-g [real? v])
    (set! g v))
  (def/public (set-b [real? v])
    (set! b v)))

(define mult-color<%> (class->interface mult-color%))


(defclass add-color% object%
  (define r 0)
  (define g 0)
  (define b 0)

  (super-new)

  (def/public (get [box? rb] [box? gb] [box? bb])
    (set-box! rb r)
    (set-box! gb g)
    (set-box! bb b))

  (def/public (set [exact-integer? rf] [exact-integer? gf] [exact-integer? bf])
    (set! r rf)
    (set! g gf)
    (set! b bf))

  (def/public (get-r) r)
  (def/public (get-g) g)
  (def/public (get-b) b)

  (def/public (set-r [exact-integer? v])
    (set! r v))
  (def/public (set-g [exact-integer? v])
    (set! g v))
  (def/public (set-b [exact-integer? v])
    (set! b v)))

(define add-color<%> (class->interface add-color%))

(define-syntaxes (-on -off -set-on! -set-off! -don -doff -d -send-get define-delta)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-case stx () 
                  [(_ fld)
                   (datum->syntax #'fld
                                  (string->symbol (format form (syntax-e #'fld)))
                                  #'fld)])))]
        [mk-set (lambda (mk-id)
                  (lambda (stx)
                    (syntax-case stx ()
                      [(_ fld val)
                       #`(set! #,(mk-id #'(_ fld)) val)])))])
    (values (mk "~a-on")
            (mk "~a-off")
            (mk-set (mk "~a-on"))
            (mk-set (mk "~a-off"))
            (mk "style-delta-~a-on")
            (mk "style-delta-~a-off")
            (mk "style-delta-~a")
            (let ([mk-id (mk "get-~a")])
              (lambda (stx)
                (syntax-case stx ()
                  [(_ fld obj)
                   #`(send obj #,(mk-id #'(* fld)))])))
            (let ([mk-id (mk "style-delta-~a")])
              (lambda (stx)
                (syntax-case stx ()
                  [(_ fld)
                   #`(define #,(mk-id stx)
                       (class-field-accessor style-delta% fld))]))))))

;; style-delta fields directly accessible only within this module:
(define-local-member-name 
  family
  face
  size-mult
  size-add
  weight-on
  weight-off
  style-on
  style-off
  smoothing-on
  smoothing-off
  underlined-on
  underlined-off
  size-in-pixels-on
  size-in-pixels-off
  transparent-text-backing-on
  transparent-text-backing-off
  foreground-mult
  background-mult
  foreground-add
  background-add
  alignment-on
  alignment-off)

(define (family-delta? s)
  (or (eq? s 'base) (family-symbol? s)))
(define (style-delta? s)
  (or (eq? s 'base) (style-symbol? s)))
(define (weight-delta? s)
  (or (eq? s 'base) (weight-symbol? s)))
(define (smoothing-delta? s)
  (or (eq? s 'base) (smoothing-symbol? s)))
(define (alignment-delta? s)
  (memq s '(base top bottom center)))

(define not-supplied (string->uninterned-symbol "[not-supplied]"))

(defclass style-delta% object%
  (field-properties [[family-delta? family] 'base]
                    [[(make-or-false string?) face] #f]
                    [[real? size-mult] 1.0]
                    [[exact-integer? size-add] 0]
                    [[weight-delta? weight-on] 'base]
                    [[weight-delta? weight-off] 'base]
                    [[style-delta? style-on] 'base]
                    [[style-delta? style-off] 'base]
                    [[smoothing-delta? smoothing-on] 'base]
                    [[smoothing-delta? smoothing-off] 'base]
                    [[bool? underlined-on] #f]
                    [[bool? underlined-off] #f]
                    [[bool? size-in-pixels-on] #f]
                    [[bool? size-in-pixels-off] #f]
                    [[bool? transparent-text-backing-on] #f]
                    [[bool? transparent-text-backing-off] #f]
                    [[alignment-delta? alignment-on] 'base]
                    [[alignment-delta? alignment-off] 'base])

  (field [foreground-mult (new mult-color%)]
         [background-mult (new mult-color%)]
         [foreground-add (new add-color%)]
         [background-add (new add-color%)])

  (def/public (get-foreground-mult) foreground-mult)
  (def/public (get-background-mult) background-mult)
  (def/public (get-foreground-add) foreground-add)
  (def/public (get-background-add) background-add)

  (send background-mult set 1.0 1.0 1.0)
  (send foreground-mult set 1.0 1.0 1.0)

  (init [change-command 'change-nothing]
        [param not-supplied])
  (super-new)
  (do-set-delta change-command param
                (lambda () (init-name 'style-delta%)))

  (def/public (set-delta [symbol? [change-command 'change-nothing]]
                         [any? [param not-supplied]])
    (do-set-delta change-command param (lambda () (method-name 'style-delta% 'set-delta))))

  (define/private (do-set-delta change-command param get-who)
    (define (check-no-param)
      (unless (eq? param not-supplied)
        (raise-mismatch-error (get-who)
                              (format "no extra argument expected for '~a command: " change-command)
                              param)))
    (define (check-param pred)
      (unless (pred param)
        (if (eq? param not-supplied)
            (raise-mismatch-error (get-who)
                                  "missing argument for command: "
                                  change-command)
            (raise-mismatch-error (get-who)
                                  (format "bad argument for '~a command: " change-command)
                                  param))))
    (case change-command
      [(change-nothing)
       (check-no-param)
       (set! family 'base)
       (set! face #f)
       (set! size-mult 1)
       (set! size-add 0)
       (set! weight-on 'base)
       (set! weight-off 'base)
       (set! style-on 'base)
       (set! style-off 'base)
       (set! smoothing-on 'base)
       (set! smoothing-off 'base)
       (set! underlined-on #f)
       (set! underlined-off #f)
       (set! size-in-pixels-off #f)
       (set! size-in-pixels-on #f)
       (set! transparent-text-backing-off #f)
       (set! transparent-text-backing-on #f)
       (set! foreground-mult (new mult-color%))
       (send foreground-mult set 1 1 1)
       (set! foreground-add (new add-color%))
       (set! background-mult (new mult-color%))
       (send background-mult set 1 1 1)
       (set! background-add (new add-color%))
       (set! alignment-on 'base)
       (set! alignment-off 'base)]
      [(change-style)
       (check-param style-delta?)
       (set! style-on param)
       (set! style-off 'base)]
      [(change-weight)
       (check-param weight-delta?)
       (set! weight-on param)
       (set! weight-off 'base)]
      [(change-smoothing)
       (check-param smoothing-delta?)
       (set! smoothing-on param)
       (set! smoothing-off 'base)]
      [(change-underline)
       (check-param bool?)
       (set! underlined-on param)
       (set! underlined-off (not param))]
      [(change-size-in-pixels)
       (check-param bool?)
       (set! size-in-pixels-on param)
       (set! size-in-pixels-off (not param))]
      [(change-size)
       (check-param exact-integer?)
       (set! size-mult 0)
       (set! size-add param)]
      [(change-family)
       (check-param family-delta?)
       (set! family param)
       (set! face #f)]
      [(change-alignment)
       (check-param alignment-delta?)
       (set! alignment-on param)
       (set! alignment-off 'base)]
      [(change-bold)
       (check-no-param)
       (set! weight-on 'bold)
       (set! weight-off 'base)]
      [(change-italic)
       (check-no-param)
       (set! style-on 'italic)
       (set! style-off 'base)]
      [(change-toggle-style)
       (check-param style-delta?)
       (set! style-on param)
       (set! style-off param)]
      [(change-toggle-weight)
       (check-param weight-delta?)
       (set! weight-on param)
       (set! weight-off param)]
      [(change-toggle-smoothing)
       (check-param smoothing-delta?)
       (set! smoothing-on param)
       (set! smoothing-off param)]
      [(change-toggle-underline)
       (check-no-param)
       (set! underlined-on #t)
       (set! underlined-off #t)]
      [(change-toggle-size-in-pixels)
       (check-no-param)
       (set! size-in-pixels-on #t)
       (set! size-in-pixels-off #t)]
      [(change-bigger)
       (check-param exact-integer?)
       (set! size-mult 1)
       (set! size-add param)]
      [(change-smaller)
       (check-param exact-integer?)
       (set! size-mult 1)
       (set! size-add (- param))]
      [(change-normal)
       (check-no-param)
       (set! family 'default)
       (set! face #f)
       (set! size-mult 0)
       (set! size-add default-size)
       (set! weight-on 'normal)
       (set! weight-off 'base)
       (set! style-on 'normal)
       (set! style-off 'base)
       (set! smoothing-on 'default)
       (set! smoothing-off 'base)
       (set! underlined-on #f)
       (set! underlined-off #t)
       (set! size-in-pixels-on #f)
       (set! size-in-pixels-off #t)
       (set! alignment-on 'bottom)
       (set! alignment-off 'base)
       (set-delta 'change-normal-color)]
      [(change-normal-color)
       (check-no-param)
       (send foreground-mult set 0 0 0)
       (send foreground-add set 0 0 0)
       (send background-mult set 0 0 0)
       (send background-add set 255 255 255)]
      [else
       (raise-type-error (get-who)
                         "change-command symbol"
                         change-command)])
    this)

  (def/public (set-delta-face [string? name] [symbol? [fam 'default]])
    (set! face (and name (string->immutable-string name)))
    (set! family fam)
    this)

  (def/public (set-delta-background [(lambda (x) (or (string? x) (x . is-a? . color%))) col])
    (let ([col (if (string? col)
                   (or (send the-color-database find-color col)
                       black-color)
                   col)])
      (set! transparent-text-backing-on #f)
      (set! transparent-text-backing-off #t)
      (send background-mult set 0 0 0)
      (send background-add set (send col red) (send col green) (send col blue))
      this))

  (def/public (set-delta-foreground [(lambda (x) (or (string? x) (x . is-a? . color%))) col])
    (let ([col (if (string? col)
                   (or (send the-color-database find-color col)
                       black-color)
                   col)])
      (send foreground-mult set 0 0 0)
      (send foreground-add set (send col red) (send col green) (send col blue))
      this))

  (def/public (collapse [style-delta% delta-in])
    (define-syntax noncollapsable?
      (syntax-rules ()
        [(_ fld base)
         (not (or (and (eq? (-on fld) ((-don fld) delta-in)) (eq? (-off fld) ((-doff fld) delta-in)))
                  (and (eq? (-on fld) base) (eq? (-off fld) base))
                  (and (eq? ((-don fld) delta-in) base) (eq? ((-doff fld) delta-in) base))
                  (and (eq? (-on fld) base) (not (eq? (-off fld) base)))
                  (and (eq? (-off fld) base) (not (eq? (-on fld) base)))))]))

    ;; is collapsing possible?
    ;; it may not be if add & multiply sequence occurs,
    ;; or certain toggling settings conflict
    (if (and (not (zero? size-mult))
             (not (= size-mult 1.0))
             (not (zero? (style-delta-size-add delta-in))))
        #f ; no collapse
        (let-boxes ([ambr 0] [ambb 0] [ambg 0]
                    [amfr 0] [amfb 0] [amfg 0]
                    [babr 0] [babb 0] [babg 0]
                    [bafr 0] [bafb 0] [bafg 0])
            (begin
              (send foreground-mult get amfr amfb amfg)
              (send background-mult get ambr ambb ambg)
              (send (style-delta-foreground-add delta-in) get bafr bafb bafg)
              (send (style-delta-background-add delta-in) get babr babb babg))
          (cond
           [(or (and (not (zero? amfr)) (not (= amfr 1.0)) (not (zero? bafr)))
                (and (not (zero? amfg)) (not (= amfg 1.0)) (not (zero? bafg)))
                (and (not (zero? amfb)) (not (= amfb 1.0)) (not (zero? bafb)))
                (and (not (zero? ambr)) (not (= ambr 1.0)) (not (zero? babr)))
                (and (not (zero? ambg)) (not (= ambg 1.0)) (not (zero? babg)))
                (and (not (zero? ambb)) (not (= ambb 1.0)) (not (zero? babb))))
            #f] ; no collapse
           ;; cases: simple or double toggle
           ;;        no further change
           ;;        formerly no change
           ;;        style definitely on
           ;;        style definitely off
           [(noncollapsable? style 'base) #f]
           [(noncollapsable? weight 'base) #f]
           [(noncollapsable? smoothing 'base) #f]
           [(noncollapsable? alignment 'base) #f]
           [(noncollapsable? underlined #f) #f]
           [(noncollapsable? size-in-pixels #f) #f]
           [(noncollapsable? transparent-text-backing #f) #f]
           [else
            ;; collapsing is possible
            (let-boxes ([bmbr 0] [bmbb 0] [bmbg 0]
                        [bmfr 0] [bmfb 0] [bmfg 0]
                        [aabr 0] [aabb 0] [aabg 0]
                        [aafr 0] [aafb 0] [aafg 0])
                (begin (send (style-delta-foreground-mult delta-in) get bmfr bmfb bmfg)
                       (send (style-delta-background-mult delta-in) get bmbr bmbb bmbg)
                       (send foreground-add get aafr aafb aafg)
                       (send background-add get aabr aabb aabg))
              
              (set! size-add (+ size-add
                                (->long (* size-mult (style-delta-size-add delta-in)))))
              (set! size-mult (* size-mult (style-delta-size-mult delta-in)))

              (send foreground-mult set (* amfr bmfr) (* amfb bmfb) (* amfg bmfg))
              (send background-mult set (* ambr bmbr) (* ambb bmbb) (* ambg bmbg))
              (send foreground-add set 
                    (+ aafr (->long (* amfr bafr)))
                    (+ aafb (->long (* amfb bafb)))
                    (+ aafg (->long (* amfg bafg))))
              (send background-add set 
                    (+ aabr (->long (* ambr babr)))
                    (+ aabb (->long (* ambb babb)))
                    (+ aabg (->long (* ambg babg))))

              (when (eq? family 'base)
                (set! family (style-delta-family delta-in))
                (when (not face)
                  (set! face (style-delta-face delta-in))))

              (let-syntax ([update! 
                            (syntax-rules ()
                              [(_ fld base)
                               (cond
                                [(and (eq? (-on fld) base) (eq? (-off fld) base))
                                 (-set-on! fld ((-don fld) delta-in))
                                 (-set-off! fld ((-doff fld) delta-in))]
                                [(and (not (eq? (-on fld) base)) (not (eq? (-off fld) base)))
                                 (when (and (eq? (-on fld) (-off fld))
                                            (or (not (eq? ((-don fld) delta-in) 'base))
                                                (not (eq? ((-doff fld) delta-in) 'base))))
                                   ;; double toggle
                                   (-set-on! fld 'base)
                                   (-set-off! fld 'base))])])])
                (update! style 'base)
                (update! weight 'base)
                (update! smoothing 'base)
                (update! alignment 'base)
                (update! underlined #f)
                (update! size-in-pixels #f)
                (update! transparent-text-backing #f)
                
                #t))]))))

  (def/public (equal? [style-delta% delta-in])
    (define-syntax-rule (same? fld)
      (and (eq? (-on fld) ((-don fld) delta-in))
           (eq? (-off fld) ((-doff fld) delta-in))))
    (define-syntax-rule (same-color? fld)
      (let-boxes ([r1 0] [g1 0] [b1 0]
                  [r2 0] [g2 0] [b2 0])
          (begin
            (send fld get r1 g1 b1)
            (send ((-d fld) delta-in) get r2 g2 b2))
        (and (= r1 r2) (= g1 g2) (= b1 b2))))
    (and (eq? family (style-delta-family delta-in))
         (or (eq? face (style-delta-face delta-in))
             (and (string? face)
                  (string? (style-delta-face delta-in))
                  (string=? face (style-delta-face delta-in))))
         (= size-mult (style-delta-size-mult delta-in))
         (= size-add (style-delta-size-add delta-in))
         (same? weight)
         (same? style)
         (same? smoothing)
         (same? alignment)
         (same? underlined)
         (same? size-in-pixels)
         (same? transparent-text-backing)
         (same-color? foreground-mult)
         (same-color? background-mult)
         (same-color? foreground-add)
         (same-color? background-add)))

  (def/public (copy [style-delta% in])
    (define-syntax-rule (DCOPY fld)
      (set! fld ((-d fld) in)))
    (define-syntax-rule (DCOPY/c fld)
      (let-boxes ([r 0][g 0][b 0])
          (send ((-d fld) in) get r g b)
        (send fld set r g b)))
    (DCOPY family)
    (DCOPY face)
    (DCOPY size-mult)
    (DCOPY size-add)
    (DCOPY weight-on)
    (DCOPY weight-off)
    (DCOPY smoothing-on)
    (DCOPY smoothing-off)
    (DCOPY style-on)
    (DCOPY style-off)
    (DCOPY underlined-on)
    (DCOPY underlined-off)
    (DCOPY size-in-pixels-on)
    (DCOPY size-in-pixels-off)
    (DCOPY transparent-text-backing-on)
    (DCOPY transparent-text-backing-off)
    (DCOPY/c foreground-mult)
    (DCOPY/c foreground-add)
    (DCOPY/c background-mult)
    (DCOPY/c background-add)
    (DCOPY alignment-on)
    (DCOPY alignment-off)))

(define-delta family)
(define-delta face)
(define-delta size-mult)
(define-delta size-add)
(define-delta weight-on)
(define-delta weight-off)
(define-delta style-on)
(define-delta style-off)
(define-delta smoothing-on)
(define-delta smoothing-off)
(define-delta  underlined-on)
(define-delta underlined-off)
(define-delta size-in-pixels-on)
(define-delta size-in-pixels-off)
(define-delta transparent-text-backing-on)
(define-delta transparent-text-backing-off)
(define-delta foreground-mult)
(define-delta background-mult)
(define-delta foreground-add)
(define-delta background-add)
(define-delta alignment-on)
(define-delta alignment-off)

;; ------------------------------------------------------------

(define-local-member-name
  s-add-child
  s-remove-child
  s-set-as-basic
  s-update
  get-s-font
  get-s-pen
  get-s-brush
  get-s-alignment
  get-s-trans-text?
  get-s-foreground
  get-s-background
  get-s-base-style
  get-s-join-shift-style
  get-s-nonjoin-delta
  get-s-name
  set-s-font
  set-s-alignment
  set-s-style-list
  set-s-base-style
  set-s-join-shift-style
  set-s-nonjoin-delta
  set-s-name
  set-s-cached-sizes
  set-s-pen
  set-s-brush
  set-s-shift-style)

(defclass style% object%
  (super-new)

  (define style-list #f) ;; points back to the list owning the style
  (define/public (set-s-style-list sl) (set! style-list sl))

  (define name #f)

  (define base-style #f)

  (define join-shift-style #f)
  (define nonjoin-delta #f)

  (define/public (get-s-name) name)
  (define/public (set-s-name v) (set! name v))
  (define/public (get-s-base-style) base-style)
  (define/public (set-s-base-style v) (set! base-style v))
  (define/public (get-s-join-shift-style) join-shift-style)
  (define/public (get-s-nonjoin-delta) nonjoin-delta)
  (define/public (set-s-join-shift-style v) (set! join-shift-style v))
  (define/public (set-s-nonjoin-delta v) (set! nonjoin-delta v))

  ;; cache computation:
  (define trans-text? #f)
  (define foreground (new color%))
  (define background (new color%))
  (define font #f)
  (define pen #f)
  (define brush #f)
  (define alignment 'bottom)

  (define cached-sizes 0)
  (define/public (set-s-cached-sizes v) (set! cached-sizes v))
  (define text-width 0.0)
  (define text-height 0.0)
  (define text-descent 0.0)
  (define text-space 0.0)

  (define children null)

  (define/public (s-set-as-basic slist)
    (set! style-list slist)

    (set! name "Basic")
    (set! base-style #f)

    (set! nonjoin-delta (new style-delta%))
    (send nonjoin-delta set-delta 'change-normal)

    (set! font (send the-font-list find-or-create-font
                     default-size 'default 'normal 'normal))
    (send foreground set 0 0 0)
    (send background set 255 255 255)
    (set! pen (send the-pen-list find-or-create-pen foreground 0 'solid))
    (set! brush (send the-brush-list find-or-create-brush background 'solid))
    (set! alignment 'bottom)
    (set! trans-text? #t))

  (define/public (s-update basic target propagate? top-level? send-notify?)
    (let ([base (if basic
                    (if (or (not style-list)
                            (eq? base-style (send style-list basic-style)))
                        basic
                        (begin
                          (send base-style s-update basic target #f #f #t)
                          target))
                    base-style)]
          [target (or target this)])

      (if join-shift-style

          ;; join style
          (when style-list
            (if (not (eq? join-shift-style
                          (send style-list basic-style)))
                (send join-shift-style s-update base target #f top-level? #t)
                (begin
                  (send target set-s-alignment (send base get-s-alignment))
                  (send target set-s-font (send base get-s-font))
                  (send target set-s-pen (send base get-s-pen))
                  (send target set-s-brush (send base get-s-brush))
                  (send target set-s-cached-sizes 0)
                  (send (send target get-s-foreground) copy-from (send base get-s-foreground))
                  (send (send target get-s-background) copy-from (send base get-s-background))
                  
                  (send style-list style-was-changed target)
                  (when top-level?
                    (send style-list style-was-changed #f)))))
          
          ;; not a join style
          (let ()
            (define-syntax-rule (match-field* fld default fld-src)
              (let* ([v (-send-get fld fld-src)]
                     [match? (eq? v ((-doff fld) nonjoin-delta))]
                     [v (if match? default v)])
                (if (or (not match?)
                        (and match? 
                             (not (eq? ((-don fld) nonjoin-delta)
                                       ((-doff fld) nonjoin-delta)))))
                    (if (not (eq? ((-don fld) nonjoin-delta) 'base))
                        ((-don fld) nonjoin-delta)
                        v)
                    v)))
            (define-syntax-rule (match-field fld default)
              (match-field* fld default (send base get-s-font)))
            (define-syntax-rule (match-bool fld orig)
              (cond
               [(and ((-doff fld) nonjoin-delta) 
                     ((-don fld) nonjoin-delta))
                (not orig)]
               [((-doff fld) nonjoin-delta)
                #f]
               [((-don fld) nonjoin-delta)
                #t]
               [else orig]))
            
            (let ([size (min 255
                             (max 1
                                  (+ (->long (* (style-delta-size-mult nonjoin-delta)
                                                (send (send base get-s-font) get-point-size)))
                                     (style-delta-size-add nonjoin-delta))))]
                  [fam+face (if (and (not (style-delta-face nonjoin-delta))
                                     (eq? (style-delta-family nonjoin-delta) 'base))
                                (let ([font (send base get-s-font)])
                                  (cons (send font get-family)
                                        (send font get-face)))
                                (let ([fam (style-delta-family nonjoin-delta)])
                                  (cons (if (eq? fam 'base)
                                            (send (send base get-s-font) get-family)
                                            fam)
                                        (style-delta-face nonjoin-delta))))]
                  [style (match-field style 'normal)]
                  [weight (match-field weight 'normal)]
                  [smoothing (match-field smoothing 'default)]
                  [alignment (match-field* alignment 'bottom target)]
                  [underlined (match-bool underlined (-send-get underlined (send base get-s-font)))]
                  [size-in-pixels (match-bool size-in-pixels (-send-get size-in-pixels (send base get-s-font)))])
              
              (send target set-s-alignment alignment)

              (let ([font (if (cdr fam+face)
                              (send the-font-list
                                    find-or-create-font
                                    size (cdr fam+face) (car fam+face) 
                                    style weight underlined smoothing size-in-pixels)
                              (send the-font-list
                                    find-or-create-font
                                    size (car fam+face) 
                                    style weight underlined smoothing size-in-pixels))])
                (send target set-s-font font)
                (send target set-s-cached-sizes 0)

                (set! trans-text? (match-bool transparent-text-backing
                                              (send base get-s-trans-text?)))

                (let ([combine-colors! (lambda (src-col src-mul src-add dest)
                                         (let ([r (send src-col red)]
                                               [g (send src-col green)]
                                               [b (send src-col blue)])
                                           (let-boxes ([rm 0.0] [gm 0.0] [bm 0.0]
                                                       [rp 0] [gp 0] [bp 0])
                                               (begin
                                                 (send src-mul get rm gm bm)
                                                 (send src-add get rp gp bp))
                                             (let ([->color
                                                    (lambda (v)
                                                      (max (min 255 (->long v)) 0))])
                                               (send dest set
                                                     (->color (+ (* r rm) rp))
                                                     (->color (+ (* g gm) gp))
                                                     (->color (+ (* b bm) bp)))))))])
                  (combine-colors! (send base get-s-foreground)
                                   (style-delta-foreground-mult nonjoin-delta)
                                   (style-delta-foreground-add nonjoin-delta)
                                   (send target get-s-foreground))
                  (combine-colors! (send base get-s-background)
                                   (style-delta-background-mult nonjoin-delta)
                                   (style-delta-background-add nonjoin-delta)
                                   (send target get-s-background))

                  (send target set-s-pen 
                        (send the-pen-list find-or-create-pen foreground 0 'solid))
                  (send target set-s-brush
                        (send the-brush-list find-or-create-brush background 'solid))

                  (when propagate?
                    (for-each (lambda (child)
                                (send child s-update #f #f #t #f #t))
                              children))

                  (when send-notify?
                    (when style-list
                      (send style-list style-was-changed target)
                      (when top-level?
                        (send style-list style-was-changed #f)))))))))))
  
  (def/public (get-name) name)
  (def/public (get-family) (send font get-family))
  (def/public (get-face) (send font get-face))
  (def/public (get-font) font)
  (def/public (get-size) (send font get-point-size))
  (def/public (get-weight) (send font get-weight))
  (def/public (get-style) (send font get-style))
  (def/public (get-smoothing) (send font get-smoothing))
  (def/public (get-underlined) (send font get-underlined))
  (def/public (get-size-in-pixels) (send font get-size-in-pixels))
  (def/public (get-transparent-text-backing) trans-text?)
  (def/public (get-foreground) (make-object color% foreground))
  (def/public (get-background) (make-object color% background))
  (def/public (get-alignment) alignment)
  (def/public (is-join?) (and join-shift-style #t))

  (def/public (get-delta [style-delta% d])
    (if join-shift-style
        (send d set-delta 'change-nothing)
        (send d copy nonjoin-delta)))

  (def/public (set-delta [style-delta% d])
    (unless (or join-shift-style
                (and style-list
                     (eq? this (send style-list basic-style))))
      (send nonjoin-delta copy d)
      (s-update #f #f #t #t #t)))

  (def/public (get-shift-style)
    (or join-shift-style
        (and style-list
             (send style-list basic-style))
        (send the-style-list basic-style)))

  (def/public (set-shift-style [style<%> style])
    (unless (or (not join-shift-style)
                (not style-list)
                (not (send style-list style-to-index style))
                (send style-list check-for-loop this style))
      (when join-shift-style
        (send join-shift-style s-remove-child this))
      (send style s-add-child this)

      (set! join-shift-style style)
      (send style-list style-has-new-child style this)
      (s-update #f #f #t #t #t)
      
      ;; Why twice? Was this a typo in the original code?
      (set! join-shift-style style)
      (s-update #f #f #t #t #t)))

  (define/public (set-s-shift-style s)
    (set! join-shift-style s))

  (def/public (get-base-style)
    base-style)

  (def/public (set-base-style [(make-or-false style<%>) style])
    (when (and style-list
               (not (eq? this (send style-list basic-style))))
      (let ([style (or style
                       (send style-list basic-style))])
        (unless (not (send style-list style-to-index style))
          (unless (send style-list check-for-loop this style)
            (when base-style
              (send base-style s-remove-child this))
            
            (set! base-style style)
            (send style s-add-child this)

            (send style-list style-has-new-child style this)

            (s-update #f #f #t #t #t))))))

  (define/private (color->rgb c)
    (values (send c red) (send c green) (send c blue)))

  (def/public (switch-to [dc<%> dc] [(make-or-false style<%>) old-style])
    (let-values ([(afr afg afb) (if old-style (color->rgb (send old-style get-s-foreground)) (values 0 0 0))]
                 [(bfr bfg bfb) (color->rgb foreground)]
                 [(abr abg abb) (if old-style (color->rgb (send old-style get-s-background)) (values 0 0 0))]
                 [(bbr bbg bbb) (color->rgb background)])
      (when (or (not old-style)
                (not (eq? (send old-style get-s-font) font)))
        (send dc set-font font))
      (when (or (not old-style)
                (not (= afr bfr))
                (not (= afb bfb))
                (not (= afg bfg)))
        (send dc set-text-foreground foreground))
      (when (or (not old-style)
                (not (= abr bbr))
                (not (= abb bbb))
                (not (= abg bbg)))
        (send dc set-text-background background))
      (when (or (not old-style)
                (not (eq? (send old-style get-s-pen) pen)))
        (send dc set-pen pen))
      (when (or (not old-style)
                (not (eq? (send old-style get-s-trans-text?) trans-text?)))
        (send dc set-text-mode (if trans-text? 'transparent 'solid)))))

  (def/public (reset-text-metrics [dc<%> dc])
    (let ([can-cache (send dc cache-font-metrics-key)])
      (unless (and (not (zero? cached-sizes))
                   (eq? cached-sizes can-cache))
        (let-values ([(w h d s) (send dc get-text-extent " " font)])
          (set! text-width w)
          (set! text-height h)
          (set! text-descent d)
          (set! text-space s)
          (set! cached-sizes can-cache)))))

  (def/public (get-text-width [dc<%> dc])
    (reset-text-metrics dc)
    text-width)

  (def/public (get-text-height [dc<%> dc])
    (reset-text-metrics dc)
    text-height)

  (def/public (get-text-descent [dc<%> dc])
    (reset-text-metrics dc)
    text-descent)

  (def/public (get-text-space [dc<%> dc])
    (reset-text-metrics dc)
    text-space)

  (define/public (s-add-child c)
    (set! children (cons c children)))

  (define/public (s-remove-child c)
    (set! children (remq c children)))

  (define/public (get-s-font) font)
  (define/public (set-s-font v) (set! font v))
  (define/public (get-s-pen) pen)
  (define/public (set-s-pen v) (set! pen v))
  (define/public (get-s-brush) brush)
  (define/public (set-s-brush v) (set! brush v))
  (define/public (get-s-alignment) alignment)
  (define/public (set-s-alignment v) (set! alignment v))
  (define/public (get-s-trans-text?) trans-text?)
  (define/public (get-s-foreground) foreground)
  (define/public (get-s-background) background))

(define style<%> (class->interface style%))

;; ----------------------------------------

(define-local-member-name
  do-named-style
  check-for-loop
  get-s-members)

(define-struct notify-key (f))

(defclass style-list% object%

  (super-new)

  (define notifications (make-weak-hash))

  (define basic (new style%))
  ;; note: the file-reader relies on having a new `basic' when the
  ;; list is cleared:

  (send basic s-set-as-basic this)

  ;; children are before parents (reverse order used for reading
  ;; and writing a style list to a stream):
  (define members (list basic))
  (define member-count 1)
  (define/public (get-s-members) members)

  (define (add-member s)
    (set! members (cons s members))
    (set! member-count (add1 member-count)))

  (def/public (copy [style-list% other])
    (map (lambda (k) (convert k #t))
         (send other get-s-members)))
  
  (define/public-final (basic-style) basic)

  (def/public (find-or-create-style [(make-or-false style<%>) base-style]
                                    [style-delta% deltain])
    (let ([base-style
           (if (or (not base-style)
                   (not (style-to-index base-style)))
               basic
               base-style)])

      ;; collapse deltas:
      (let ([delta (new style-delta%)])
        (send delta copy deltain)
        (let loop ([base-style base-style])
          (if (and (not (send base-style get-s-name))
                   (not (send base-style get-s-join-shift-style))
                   (send delta collapse (send base-style get-s-nonjoin-delta)))
              (loop (send base-style get-s-base-style))
              
              (or
               ;; Find existing style that matches:
               (for/or ([s (in-list members)])
                 (and (not (send s get-s-name))
                      (not (send s get-s-join-shift-style))
                      (eq? (send s get-s-base-style) base-style)
                      (send delta equal? (send s get-s-nonjoin-delta))
                      s))
               
               ;; Create style
               (let ([s (new style%)])
                 (send s set-s-style-list this)
                 (send s set-s-name #f)
                 (send s set-s-nonjoin-delta delta)
                 (send s set-s-base-style base-style)
                 (send base-style s-add-child s)
                 (send s s-update #f #f #f #f #f) ;; no need to propagate/notify, because we just created it
                 (add-member s)
                 s)))))))

  (def/public (find-or-create-join-style [style% base-style]
                                         [style% shift-style])
    (let ([base-style (if (or (not base-style)
                              (not (style-to-index base-style)))
                          basic
                          base-style)]
          [shift-style (if (or (not shift-style)
                               (not (style-to-index shift-style)))
                           basic
                           shift-style)])

      (or (for/or ([s (in-list members)])
            (and (not (send s get-s-name))
                 (eq? (send s get-s-base-style) base-style)
                 (eq? (send s get-s-join-shift-style) shift-style)
                 s))
          (let ([s (new style%)])
            (send s set-s-style-list this)
            (send s set-s-name #f)
            (send s set-s-shift-style shift-style)
            (send shift-style s-add-child s)
            (send s set-s-base-style base-style)
            (send base-style s-add-child s)
            (send s s-update #f #f #t #t #t)
            (add-member s)
            s))))

  (def/public (find-named-style [string? name])
    (for/or ([s (in-list members)])
      (and (equal? name (send s get-s-name))
           s)))

  (define/public (do-named-style name plain-style replac?)
    (let ([plain-style (if (or (not plain-style)
                               (not (style-to-index plain-style)))
                           basic
                           plain-style)]
          [name (string->immutable-string name)])
      
      (let ([style (for/or ([s (in-list members)])
                     (and (equal? name (send s get-s-name))
                          s))])
        (if (or (and style (not replac?))
                ;; can't replace basic style:
                (eq? style basic))
            style

            (let ([found-style style]
                  [style (or style
                             (let ([s (new style%)])
                               (send s set-s-name name)
                               (send s set-s-style-list this)
                               (send s set-s-base-style basic)
                               s))])
              
              ;; plain-style must not depend on this style
              ;; (otherwise, we'd create a dependency cycle)
              (if (check-for-loop style plain-style)
                  style

                  (begin
                    (let ([base (send style get-s-base-style)])
                      (send base s-remove-child style))
                    (let ([shift (send style get-s-join-shift-style)])
                      (when shift
                        (send shift s-remove-child style)))
                    
                    (let ([shift (send plain-style get-s-join-shift-style)])
                      (if shift
                          (begin
                            (send style set-s-join-shift-style shift)
                            (send shift s-add-child style))
                          (let ([delta (new style-delta%)])
                            (send style set-s-nonjoin-delta delta)
                            (unless (eq? plain-style basic)
                              (send delta copy (send plain-style get-s-nonjoin-delta))))))

                    (let ([base (if (eq? plain-style basic)
                                    basic
                                    (send plain-style get-s-base-style))])
                      (send style set-s-base-style base)
                      (send base s-add-child style))

                    (send style s-update #f #f #t #t #t)

                    (unless found-style
                      (add-member style))
                    
                    style)))))))

  (def/public (new-named-style [string? name] [style<%> plain-style])
    (do-named-style name plain-style #f))

  (def/public (replace-named-style [string? name] [style<%> plain-style])
    (do-named-style name plain-style #t))

  (def/public (convert [style% style] [any? [overwrite? #f]])
    (or
     (and (style-to-index style)
          style)
     (and (send style get-s-name)
          (not overwrite?)
          (find-named-style (send style get-s-name)))

     (let ([base (or (let ([s (send style get-s-base-style)])
                       (and s (convert s)))
                     (basic-style))])
       
       (let ([newstyle
              (let ([shift (send style get-s-join-shift-style)])
                (if shift
                    (find-or-create-join-style
                     base
                     (convert shift))
                    (find-or-create-style base (send style get-s-nonjoin-delta))))])

         (let ([name (send style get-s-name)])
           (if name
               (if overwrite?
                   (replace-named-style name newstyle)
                   (new-named-style name newstyle))
               newstyle))))))

  (def/public (style-was-changed [(make-or-false style%) which])
    (for ([k (in-hash-keys notifications)])
      (k which)))

  (def/public (notify-on-change [procedure? f])
    (hash-set! notifications f 
               ;; In case `f' has a contract, retain `f' as long
               ;; as the pre-contract value is reachable:
               (impersonator-ephemeron f))
    (make-notify-key f))

  (def/public (forget-notification [notify-key? id])
    (hash-remove! notifications (notify-key-f id)))

  (def/public (check-for-loop [style<%> s] [style<%> p])
    (or (eq? s p)
        (let ([base (send p get-s-base-style)])
          (cond
           [(not base) #f]
           [(send p get-s-join-shift-style)
            => (lambda (j)
                 (or (check-for-loop s (send p get-s-base-style))
                     (check-for-loop s j)))]
           [else (check-for-loop s base)]))))

  (def/public (style-has-new-child [style<%> s] [style<%> c])
    ;; need to maintain the invariant that children are in the list
    ;; before parents...
    (let ([new-members
           (let loop ([members members][insert? #f])
             (let ([m (car members)])
               (cond
                [(eq? m c) (if insert?
                               (cons c (cons s (cdr members)))
                               #f)]
                [(eq? m s) (loop (cdr members) #t)]
                [else (let ([rest (loop (cdr members) insert?)])
                        (and rest (cons m rest)))])))])
      (when new-members
        (set! members new-members)
        ;; May have moved parent after its own parents
        (style-has-new-child (send s get-s-base-style) s)
        (let ([join (send s get-s-join-style)])
          (when join
            (style-has-new-child join s))))))
  
  (def/public (number) member-count)

  (def/public (index-to-style [exact-nonnegative-integer? i])
    (and (i . < . member-count)
         (list-ref members (- member-count i 1))))

  (def/public (style-to-index [style<%> s])
    (let loop ([members members][i (sub1 member-count)])
      (cond
       [(null? members) #f]
       [(eq? (car members) s) i]
       [else (loop (cdr members) (sub1 i))])))


  (def/public (write-to-file [editor-stream-out% f])
    (write-styles-to-file this f))

  (define/public (map-index-to-style s i list-id)
    (let loop ([sll (send s get-s-sll)])
      (cond
       [(null? sll)
        (error 'map-index-to-style "bad style list index for snip")]
       [(= (style-list-link-list-id (car sll)) list-id)
        (if (eq? (style-list-link-basic (car sll)) basic)
            ;; if basic changes, that means list was cleared
            (if (and (style-list-link-style-map (car sll))
                     (i . < . (style-list-link-num-mapped-styles (car sll))))
                (vector-ref (style-list-link-style-map (car sll)) i)
                (error 'map-index-to-style "bad style index for snip"))
            basic)]
       [else (loop (cdr sll))])))

  (define/public (read-style-list f)
    (read-styles-from-file (new style-list%) f 0 (box 0))))

(define the-style-list (new style-list%))

;; ----------------------------------------

(define-struct style-list-link (style-list
                                list-id
                                basic
                                num-mapped-styles
                                style-map))

(define (setup-style-reads-writes s)
  (send s set-s-sll null))

(define (done-style-reads-writes s)
  (send s set-s-sll null))

(define (invert ht)
  (make-immutable-hasheq
   (hash-map ht (lambda (k v) (cons v k)))))

(define family-ints
  #hasheq((base . -1)
          (decorative . 71)
          (roman . 72)
          (script . 73)
          (swiss . 74)
          (modern . 75)
          (teletype . 76)
          (system . 77)
          (symbol . 78)
          (default . 70)))
(define int-families (invert family-ints))

(define (family-standard-to-this v)
  (hash-ref int-families v 'default))
(define (family-this-to-standard v)
  (hash-ref family-ints v 70))

(define weight-ints
  #hasheq((base . -1)
          (light . 91)
          (bold . 92)
          (normal . 90)))
(define int-weights (invert weight-ints))

(define (weight-standard-to-this v)
  (hash-ref int-weights v 'normal))
(define (weight-this-to-standard v)
  (hash-ref weight-ints v 90))

(define style-ints
  #hasheq((base . -1)
          (italic . 93)
          (slant . 94)
          (normal . 90)))
(define int-styles (invert style-ints))

(define (style-standard-to-this v)
  (hash-ref int-styles v 'normal))
(define (style-this-to-standard v)
  (hash-ref style-ints v 90))

(define smoothing-ints
  #hasheq((base . -1)
          (partly-smoothed . 0)
          (smoothed . 1)
          (unsmoothed . 2)
          (default . 3)))
(define int-smoothings (invert smoothing-ints))

(define (smoothing-standard-to-this v)
  (hash-ref int-smoothings v 'default))
(define (smoothing-this-to-standard v)
  (hash-ref smoothing-ints v 3))

(define align-ints
  #hasheq((base . -1)
          (top . 0)
          (bottom . 1)
          (center . 2)))
(define int-aligns (invert align-ints))

(define (align-standard-to-this v)
  (hash-ref int-aligns v 'bottom))
(define (align-this-to-standard v)
  (hash-ref align-ints v 1))

(define (read-styles-from-file style-list f overwritename? _list-id)
  (let-boxes ([list-id 0])
      (send f get list-id)
    (set-box! _list-id list-id)

    (or
     (ormap (lambda (sll)
              (and (= (style-list-link-list-id sll) list-id)
                   (style-list-link-style-list sll)))
            (send f get-s-sll))

     (let ([nms (send f get-exact)])

       (let* ([map-vec (make-vector nms)]
              [sll (make-style-list-link style-list
                                         list-id
                                         (send style-list basic-style)
                                         nms
                                         map-vec)])
         (send f set-s-sll (cons sll (send f get-s-sll)))

         (vector-set! map-vec 0 (send style-list basic-style))

         (for ([i (in-range 1 nms)])
           (let ([base-index (send f get-exact)])

             (when (base-index . >= . i)
               (error 'map-index-to-style "bad style index"))

             (let ([name (bytes->string/utf-8 (send f get-bytes))])
               (let ([is-join (send f get-exact)])
                 (if (positive? is-join)
                     (let ([shift-index (send f get-exact)])
                       (when (shift-index . >= . i)
                         (error 'map-index-to-style "bad shift-style index"))
                       (let ([js
                              (send style-list
                                    find-or-create-join-style
                                    (vector-ref map-vec base-index)
                                    (vector-ref map-vec shift-index))])
                         (vector-set! map-vec i js)))
                     (let ([delta (new style-delta%)]
                           [get-float (lambda (f) (send f get-inexact))]
                           [get-int (lambda (f) (send f get-exact))])
                       (let ([fam (send f get-exact)])
                         
                         (send delta set-family (family-standard-to-this fam))
                         (let ([name (bytes->string/utf-8 (send f get-bytes))])
                           (when (not (equal? name ""))
                             (send delta set-face (string->immutable-string name))))
                         
                         (send delta set-size-mult (get-float f))
                         (send delta set-size-add (get-int f))
                         (send delta set-weight-on (weight-standard-to-this (get-int f)))
                         (send delta set-weight-off (weight-standard-to-this (get-int f)))
                         (send delta set-style-on (style-standard-to-this (get-int f)))
                         (send delta set-style-off (style-standard-to-this (get-int f)))
                         (unless (<= 1 (send f get-wxme-version) 4)
                           (send delta set-smoothing-on (smoothing-standard-to-this (get-int f)))
                           (send delta set-smoothing-off (smoothing-standard-to-this (get-int f))))
                         (send delta set-underlined-on (positive? (get-int f)))
                         (send delta set-underlined-off (positive? (get-int f)))
                         (unless (<= 1 (send f get-wxme-version) 5)
                           (send delta set-size-in-pixels-on (positive? (get-int f)))
                           (send delta set-size-in-pixels-off (positive? (get-int f))))
                         (unless (<= 1 (send f get-wxme-version) 2)
                           (send delta set-transparent-text-backing-on (positive? (get-int f)))
                           (send delta set-transparent-text-backing-off (positive? (get-int f))))

                         (let ([r (send f get-inexact)]
                               [g (send f get-inexact)]
                               [b (send f get-inexact)])
                           (send (send delta get-foreground-mult) set r g b))
                         (let ([r (send f get-inexact)]
                               [g (send f get-inexact)]
                               [b (send f get-inexact)])
                           (send (send delta get-background-mult) set r g b))
                         (let ([r (send f get-exact)]
                               [g (send f get-exact)]
                               [b (send f get-exact)])
                           (send (send delta get-foreground-add) set r g b))
                         (let ([r (send f get-exact)]
                               [g (send f get-exact)]
                               [b (send f get-exact)])
                           (send (send delta get-background-add) set r g b)

                           (when (<= 1 (send f get-wxme-version) 2)
                             (when (or (positive? r) (positive? g) (positive? b))
                               (send delta set-transparent-text-backing-off #t))))
                         
                         (send delta set-alignment-on (align-standard-to-this (get-int f)))
                         (send delta set-alignment-off (align-standard-to-this (get-int f)))
                         
                         (vector-set! map-vec i (send style-list find-or-create-style 
                                                      (vector-ref map-vec base-index)
                                                      delta))))))

               (when (not (equal? name ""))
                 (let ([ns
                        (if overwritename?
                            (send style-list replace-named-style name (vector-ref map-vec i))
                            (send style-list new-named-style name (vector-ref map-vec i)))])
                   (vector-set! map-vec i ns)))))))
       style-list))))

(define (write-styles-to-file style-list f)
  (or
   (ormap (lambda (sll)
            (and (eq? (style-list-link-style-list sll) style-list)
                 (begin
                   (send f put (style-list-link-list-id sll))
                   #t)))
          (send f get-s-sll))
   (let ([lid (send f get-s-style-count)])
     (send f set-s-style-count (add1 lid))

     (let ([sll (make-style-list-link style-list
                                      lid
                                      #f
                                      #f
                                      #f)])
       (send f set-s-sll (cons sll (send f get-s-sll)))

       (send f put lid)

       (let ([count (send style-list number)])
         (send f put count)

         (for ([i (in-range 1 count)])
           (let ([style (send style-list index-to-style i)])

             (send f put (send style-list style-to-index (send style get-base-style)))

             (send f put (let ([name (send style get-name)])
                           (if name
                               (string->bytes/utf-8 name)
                               #"")))

             (if (send style is-join?)
                 (begin
                   (send f put 1)
                   (send f put (send style-list style-to-index (send style get-shift-style))))
                 (let ([delta (new style-delta%)])
                   (send style get-delta delta)
      
                   (send f put 0)

                   (send f put (family-this-to-standard (style-delta-family delta)))
                   (send f put (let ([face (style-delta-face delta)])
                                 (if face
                                     (string->bytes/utf-8 face)
                                     #"")))
                   
                   (send f put (style-delta-size-mult delta))
                   (send f put (style-delta-size-add delta))
                   (send f put (weight-this-to-standard (style-delta-weight-on delta)))
                   (send f put (weight-this-to-standard (style-delta-weight-off delta)))
                   (send f put (style-this-to-standard (style-delta-style-on delta)))
                   (send f put (style-this-to-standard (style-delta-style-off delta)))
                   (send f put (smoothing-this-to-standard (style-delta-smoothing-on delta)))
                   (send f put (smoothing-this-to-standard (style-delta-smoothing-off delta)))
                   (send f put (if (style-delta-underlined-on delta) 1 0))
                   (send f put (if (style-delta-underlined-off delta) 1 0))
                   (send f put (if (style-delta-size-in-pixels-on delta) 1 0))
                   (send f put (if (style-delta-size-in-pixels-off delta) 1 0))
                   (send f put (if (style-delta-transparent-text-backing-on delta) 1 0))
                   (send f put (if (style-delta-transparent-text-backing-off delta) 1 0))

                   (let-boxes ([r 0.0][g 0.0][b 0.0])
                       (send (style-delta-foreground-mult delta) get r g b)
                     (send f put r) (send f put g) (send f put b))
                   (let-boxes ([r 0.0][g 0.0][b 0.0])
                       (send (style-delta-background-mult delta) get r g b)
                     (send f put r) (send f put g) (send f put b))
                   (let-boxes ([r 0][g 0][b 0])
                       (send (style-delta-foreground-add delta) get r g b)
                     (send f put r) (send f put g) (send f put b))
                   (let-boxes ([r 0][g 0][b 0])
                       (send (style-delta-background-add delta) get r g b)
                     (send f put r) (send f put g) (send f put b))

                   (send f put (align-this-to-standard (style-delta-alignment-on delta)))
                   (send f put (align-this-to-standard (style-delta-alignment-off delta)))))))

         #t)))))
