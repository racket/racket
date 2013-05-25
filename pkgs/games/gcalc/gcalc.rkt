;;;============================================================================
;;; GCalc
;;;   based on http://www.grame.fr/Research/GCalcul/Graphic_Calculus.html
;;;   implemented by Eli Barzilay.

#lang racket

(require racket/gui "../show-scribbling.rkt" racket/unit)
(provide game@)

(define customs '())
(define (add-custom! name get set type desc)
  (set! customs (append customs (list (make-custom name get set type desc)))))
(define-struct custom (name getter setter type description))
(define-syntax defcustom
  (syntax-rules ()
    [(_ var default type description)
     (begin (define var default)
            (add-custom! 'var (λ() var) (λ(v) (set! var v))
                         type description))]))
(define game@ (unit (import) (export)

;;;============================================================================
;;; Customizations etc

(defcustom EVAL-NOW    #t 'bool      "Evaluate immediately on application")
(defcustom EVAL-DEPTH  18 '(int 100) "Evaluation depth limit")
(defcustom DRAW-CUTOFF  8 '(int 50)  "Cutoff evaluation when smaller")
(defcustom SPLIT-ARGS  #f 'bool      "Split arg by function body structure")
(defcustom COLOR-OPS   #f 'bool      "Use colors as functions")
(defcustom NOBMP-PRINT #f 'bool      "Never use bitmaps to print")

(define DK-PEN (instantiate pen% ["BLACK" 1 'solid]))
(define LT-PEN (instantiate pen% ["WHITE" 1 'solid]))

(define COLORS
  (let ([cs '((transparent)
              (black         0   0   0)
              (dk-gray      64  64  64)
              (gray        128 128 128)
              (lt-gray     192 192 192)
              (white       255 255 255)
              (dk-red      128   0   0)
              (red         255   0   0)
              (dk-green      0 128   0)
              (green         0 255   0)
              (dark-blue     0   0 128)
              (blue          0   0 255)
              (dk-yellow   128 128   0)
              (yellow      255 255   0)
              (dk-cyan       0 128 128)
              (cyan          0 255 255)
              (dk-magenta  128   0 128)
              (magenta     255   0 255))])
    (for/list ([c (in-list cs)])
      (list
       (car c)
       (cond [(null? (cdr c)) (instantiate pen% ["WHITE" 1 'transparent])]
             [(eq? (car c) 'black) LT-PEN]
             [else DK-PEN])
       (if (null? (cdr c))
         (instantiate brush% ["BLACK" 'transparent])
         (instantiate brush% [(apply make-object color% (cdr c)) 'solid]))))))

(define COLOR-CELL-ROW 2)
(define COLOR-CELL-COL (/ (length COLORS) COLOR-CELL-ROW))
(define OPERS-CELL-ROW 3)
(define OPERS-CELL-COL 3)
(define STORE-CELL-ROW 5)
(define STORE-CELL-COL 8)
(define GLOBAL-HEIGHT (* 7 (lcm COLOR-CELL-COL OPERS-CELL-COL STORE-CELL-COL)))

(define COLOR-CELL-SIZE (/ GLOBAL-HEIGHT COLOR-CELL-COL))
(define OPERS-CELL-SIZE (/ GLOBAL-HEIGHT OPERS-CELL-COL))
(define STORE-CELL-SIZE (/ GLOBAL-HEIGHT STORE-CELL-COL))

(define SHOW-CELL-SIZE 600)

(define BG-PEN/BRUSH
  (make-parameter (list (instantiate pen%   ["BLACK" 1 'solid])
                        (instantiate brush% ["GRAY" 'solid]))))
(define HIGHLIGHT-WIDTH 4)
(define HIGHLIGHT-PEN/BRUSH
  (list (instantiate pen%   ["BLACK" HIGHLIGHT-WIDTH 'solid])
        (instantiate brush% ["BLACK" 'transparent])))

(define DOUBLE-MILISECS 250)

(define CELL-BORDER 6)

(define CELL-FONT (instantiate font% [10 'decorative 'normal 'normal]))
(define SHOW-FONT (instantiate font% [32 'decorative 'normal 'normal]))

(define APPLY-SIZE  0.40)
(define ABSTR-GAP   0.1)
(define ABSTR-SIZE  0.3)
(define 3D-DX       0.4)
(define 3D-DY       0.5)
(define 3D-OFFSET   (+ CELL-BORDER 2))

;;;============================================================================
;;; Utilities

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define (read-from-string str)
  (with-handlers ([void (λ(x) #f)])
    (define obj (read (open-input-string (string-append "(" str ")"))))
    (and (list? obj) (= 1 (length obj)) (car obj))))

(define (write-to-string obj)
  (format "~s" obj))

(define (set-pen/brush dc p/b)
  (send dc set-pen   (1st p/b))
  (send dc set-brush (2nd p/b)))

;;;============================================================================
;;; GCalc definition

(define var-num 0)
(define (get-next-vnum)
  (set! var-num (add1 var-num))
  var-num)
(define (make-var var)
  (list 'var var
        ;;(get-next-vnum) - no need for this since we only use closed terms
        ))
(define (var-val var)
  (2nd var))
(define (var-expr? expr)
  (and (pair? expr) (eq? (1st expr) 'var)))

(define null-expr #f)
(define (null-expr? expr)
  (eq? expr null-expr))

(define make-expr list)
(define expr-op   1st)
(define expr-1st  2nd)
(define expr-2nd  3rd)

(define (simple-expr? expr)
  (or (not (pair? expr)) (eq? (car expr) 'var)))
(define (composite-expr? expr . type)
  (and (pair? expr)
       (if (null? type)
         (not (eq? (car expr) 'var))
         (eq? (expr-op expr) (car type)))))

(define (make-left-right expr1 expr2)
  (make-expr '\| expr1 expr2))
(define (make-right-left expr1 expr2)
  (make-left-right expr2 expr1))
(define (left-right-expr? expr)
  (composite-expr? expr '\|))

(define (make-top-bottom expr1 expr2)
  (make-expr '- expr1 expr2))
(define (make-bottom-top expr1 expr2)
  (make-top-bottom expr2 expr1))
(define (top-bottom-expr? expr)
  (composite-expr? expr '-))

(define (make-front-back expr1 expr2)
  (make-expr '/ expr1 expr2))
(define (make-back-front expr1 expr2)
  (make-front-back expr2 expr1))
(define (front-back-expr? expr)
  (composite-expr? expr '/))

(define (make-application1 expr1 expr2)
  (make-expr 'apply expr1 expr2))
(define (make-application2 expr1 expr2)
  (make-expr 'apply expr2 expr1))
(define (application-expr? expr)
  (composite-expr? expr 'apply))

(define (make-abstraction expr1 expr2)
  (if (simple-expr? expr1)
    (let ([var (make-var expr1)])
      (make-expr 'lambda var (substitute var expr1 expr2)))
    null-expr))
(define (abstraction-expr? expr)
  (composite-expr? expr 'lambda))

(define (expr-size expr)
  (if (composite-expr? expr)
    (+ 1 (expr-size (expr-1st expr)) (expr-size (expr-2nd expr)))
    1))

(define (recolor-expr color expr)
  (define (get-rgb color)
    (cond [(or (eq? color 'transparent) (null-expr? color)) color]
          [(symbol? color)
           (define c (send (3rd (assq color COLORS)) get-color))
           (list (send c red) (send c green) (send c blue))]
          [else (vector->list color)]))
  (define (mix rgb1 rgb2)
    (cond [(equal? rgb1 rgb2) (list->vector rgb1)]
          [(or (null-expr? rgb1) (null-expr? rgb2)) null-expr]
          [(or (eq? rgb1 'transparent) (eq? rgb2 'transparent)) 'transparent]
          [else (for/vector ([c1 (in-list rgb1)] [c2 (in-list rgb2)])
                  (inexact->exact (round (/ (+ c1 c2) 2))))]))
  (define rgb (get-rgb color))
  (let loop ([expr expr])
    (cond [(or (var-expr? expr) (null-expr? expr)) expr]
          [(simple-expr? expr) (mix rgb (get-rgb expr))]
          [else (make-expr (expr-op expr)
                           (loop (expr-1st expr))
                           (loop (expr-2nd expr)))])))

(define (reduce-application expr . level)
  (define (reduce expr level)
    (if (and level (<= level 0))
      expr
      (let ([1st (let ([e (expr-1st expr)])
                   (if (application-expr? e)
                     (reduce-application e (and level (sub1 level)))
                     e))]
            [2nd (expr-2nd expr)])
        (cond [(and COLOR-OPS (simple-expr? 1st) (not (var-expr? 1st)))
               (recolor-expr 1st 2nd)]
              [(or (simple-expr? 1st) (application-expr? 1st)) expr]
              [(abstraction-expr? 1st)
               (substitute 2nd (expr-1st 1st) (expr-2nd 1st))]
              [else (let ([2nd (split-expr 2nd (expr-op 1st))])
                      (make-expr (expr-op 1st)
                                 (make-application1 (expr-1st 1st)
                                                    (expr-1st 2nd))
                                 (make-application1 (expr-2nd 1st)
                                                    (expr-2nd 2nd))))]))))
  (let* ([level (if (null? level) EVAL-DEPTH (car level))]
         [new   (reduce expr level)])
    (if (and (application-expr? new) (not (equal? new expr))
             (or (not level) (> level 0)))
      (reduce-application new (and level (if (> level 0) (sub1 level) 0)))
      new)))

#; ;; No need for this now
(define (eval-expr expr)
  (define (eval-expr expr level)
    (cond [(zero? level) null-expr]
          [(or (simple-expr? expr) (abstraction-expr? expr)) expr]
          [(application-expr? expr)
           (define 1st (eval-expr (expr-1st expr) (sub1 level)))
           (define 2nd (eval-expr (expr-2nd expr) (sub1 level)))
           (cond [(and COLOR-OPS (simple-expr? 1st) (not (var-expr? 1st)))
                  (recolor-expr 1st 2nd)]
                 [(or (simple-expr? 1st) (application-expr? 1st)) expr]
                 [(abstraction-expr? 1st)
                  (eval-expr (substitute 2nd (expr-1st 1st) (expr-2nd 1st))
                             (sub1 level))]
                 [else (define 2nd (split-expr (eval-expr 2nd (sub1 level))
                                               (expr-op 1st)))
                       (make-expr (expr-op 1st)
                                  (eval-expr (make-application (expr-1st 1st)
                                                               (expr-1st 2nd))
                                             (sub1 level))
                                  (eval-expr (make-application (expr-2nd 1st)
                                                               (expr-2nd 2nd))
                                             (sub1 level)))])]
          [else (make-expr (expr-op expr)
                           (eval-expr (expr-1st expr) (sub1 level))
                           (eval-expr (expr-2nd expr) (sub1 level)))]))
  (dynamic-wind
    begin-busy-cursor
    (λ() (eval-expr expr (or EVAL-DEPTH -1)))
    end-busy-cursor))

(define (split-expr expr op)
  (cond
   [(or (simple-expr? expr) (abstraction-expr? expr))
    (make-expr op expr expr)]
   [(eq? (expr-op expr) op) expr]
   [else (define e1   (split-expr (expr-1st expr) op))
         (define e2   (split-expr (expr-2nd expr) op))
         (define e11  (expr-1st e1))
         (define e12  (expr-2nd e1))
         (define e21  (expr-1st e2))
         (define e22  (expr-2nd e2))
         (define e-op (expr-op expr))
         (make-expr op (make-expr e-op e11 e21) (make-expr e-op e12 e22))]))

(define (substitute new old expr)
  (cond
   [(composite-expr? expr)
    (if (and (abstraction-expr? expr) (equal? (expr-1st expr) old))
      expr
      (let-values ([(new1 new2) (if SPLIT-ARGS
                                  (let ([x (split-expr new (expr-op expr))])
                                    (values (expr-1st x) (expr-2nd x)))
                                  (values new new))])
        (make-expr (expr-op expr)
                   (substitute new1 old (expr-1st expr))
                   (substitute new2 old (expr-2nd expr)))))]
   [(equal? expr old) new]
   [else expr]))

(define (valid-expr? obj)
  (cond [(null-expr? obj) #t]
        [(list? obj)   (case (length obj)
                         [(2) (and (eq? (1st obj) 'var)
                                   (not (list? (2nd obj)))
                                   (valid-expr? (2nd obj)))]
                         [(3 4) (case (car obj)
                                  [(\| - / apply lambda)
                                   (and (valid-expr? (expr-1st obj))
                                        (valid-expr? (expr-2nd obj)))]
                                  [else #f])])]
        [(symbol? obj) (and (assq obj COLORS) #t)]
        [(vector? obj) (and (= (vector-length obj) 3)
                            (let ([ok? (λ(x) (and (integer? x) (<= 0 x 255)))])
                              (and (ok? (vector-ref obj 0))
                                   (ok? (vector-ref obj 1))
                                   (ok? (vector-ref obj 2)))))]
        [else #f]))
(define (valid-contents? obj)
  (and (pair? obj)
       (or (not (car obj)) (string? (car obj)))
       (valid-expr? (cdr obj))))
(define (validate-contents obj)
  (if (valid-contents? obj)
    obj
    (begin (eprintf "~s is not a valid contents.\n" obj)
           (error 'validate-contents "~s is not a valid contents." obj))))

;;;============================================================================
;;; GCalc drawing

(define transparent?-cache (make-weak-hash))

(define (expr-contains-transparent? expr)
  (if (simple-expr? expr)
    (or (null-expr? expr) (eq? expr 'transparent)
        (and (var-expr? expr) (eq? (var-val expr) 'transparent)))
    (let ([v (hash-ref transparent?-cache expr 'unknown)])
      (if (eq? v 'unknown)
        (let ([v (cond [(abstraction-expr? expr)
                        (expr-contains-transparent? (expr-2nd expr))]
                       [(application-expr? expr)
                        #t]
                       [else (or (expr-contains-transparent? (expr-1st expr))
                                 (expr-contains-transparent? (expr-2nd expr)))])])
          (hash-set! transparent?-cache expr v)
          v)
        v))))

;; Draw an exprression - the smart way.
(define (draw-expr dc expr name . r)
  (define size
    (let-values ([(sx sy) (send dc get-size)]) (inexact->exact (min sx sy))))
  (define eval? (if (null? r) #f (car r)))
  (define (avg x y) (/ (+ x y) 2))
  (define (rnd x) (inexact->exact (round x)))
  (define dc-ofs 3D-OFFSET)
  (define dc-size (- size dc-ofs dc-ofs))
  (define draw-polygon ; efficient (could be more if it was global)
    (let* ([p1 (instantiate point% [])] [p2 (instantiate point% [])]
           [p3 (instantiate point% [])] [p4 (instantiate point% [])]
           [points (list p1 p2 p3 p4)])
      (λ(x1 y1 x2 y2 x3 y3 x4 y4)
        (send* p1 (set-x x1) (set-y y1)) (send* p2 (set-x x2) (set-y y2))
        (send* p3 (set-x x3) (set-y y3)) (send* p4 (set-x x4) (set-y y4))
        (send dc draw-polygon points))))
  (define tmp-color (instantiate color% [])) ; reused for drawing colors
  (define (draw expr level left? top? front? x1 y1 z1 x2 y2 z2 abstr)
    (define eval? (or (not level) (> level 0)))
    (define lev1  (and level (if (> level 0) (sub1 level) 0)))
    (when (and eval? (application-expr? expr))
      (set! expr (reduce-application expr level)))
    (cond
      [(null-expr? expr) expr]
      [(composite-expr? expr)
       (define 1st (expr-1st expr))
       (define 2nd (expr-2nd expr))
       (cond
         [(left-right-expr? expr)
          (define cut? (and DRAW-CUTOFF (< (* (- x2 x1) size) DRAW-CUTOFF)))
          (define x12  (avg x1 x2))
          (define t?   (expr-contains-transparent? 1st))
          (make-right-left
           (if (and (not cut?) (or top? front? t?))
             (draw 2nd lev1 t? top? front? x12 y1 z1 x2 y2 z2 .0)
             2nd)
           (if cut?
             (draw 1st lev1 left? top? front? x1 y1 z1 x2 y2 z2 abstr)
             (draw 1st lev1 left? top? front? x1 y1 z1 x12 y2 z2 .0)))]
         [(top-bottom-expr? expr)
          (define cut? (and DRAW-CUTOFF (< (* (- y2 y1) size) DRAW-CUTOFF)))
          (define y12  (avg y1 y2))
          (define t?   (expr-contains-transparent? 1st))
          (make-bottom-top
           (if (and (not cut?) (or left? front? t?))
             (draw 2nd lev1 left? t? front? x1 y12 z1 x2 y2 z2 .0)
             2nd)
           (if cut?
             (draw 1st lev1 left? top? front? x1 y1 z1 x2 y2 z2 abstr)
             (draw 1st lev1 left? top? front? x1 y1 z1 x2 y12 z2 .0)))]
         [(front-back-expr? expr)
          (define cut? (and DRAW-CUTOFF (< (* (- z2 z1) size) DRAW-CUTOFF)))
          (define z12  (avg z1 z2))
          (define t?   (expr-contains-transparent? 1st))
          (make-back-front
           (if (and (not cut?) (or left? top? t?))
             (draw 2nd lev1 left? top? t? x1 y1 z1 x2 y2 z12 .0)
             2nd)
           (if cut?
             (draw 1st lev1 left? top? front? x1 y1 z1 x2 y2 z2 abstr)
             (draw 1st lev1 left? top? front? x1 y1 z12 x2 y2 z2 .0)))]
         [(abstraction-expr? expr)
          (draw 2nd 0 left? top? front? x1 y1 z1 x2 y2 z2 (+ abstr ABSTR-GAP))
          (set-pen/brush dc (cdr (assq (var-val 1st) COLORS)))
          (define xx (+ (rnd (* (+ x1 (* (- x2 x1) abstr) (* 3D-DX z2))
                                (/ dc-size (+ 1 3D-DX))))
                        dc-ofs))
          (define yy (+ (rnd (* (+ y1 (* 3D-DY z2)) (/ dc-size (+ 1 3D-DY))))
                        dc-ofs))
          (define dx (rnd (* ABSTR-SIZE (- x2 x1) (/ dc-size (+ 1 3D-DX)))))
          (define dy (rnd (* ABSTR-SIZE (- y2 y1) (/ dc-size (+ 1 3D-DY)))))
          (send dc draw-ellipse xx yy dx dy)
          expr]
         [(application-expr? expr)
          (define x12 (avg x1 x2))
          (define y12 (avg y1 y2))
          (define dx  (* (- x2 x1) 1/2 APPLY-SIZE))
          (define dy  (* (- y2 y1) 1/2 APPLY-SIZE))
          (define xx1 (- x12 dx))
          (define yy1 (- y12 dy))
          (define xx2 (+ x12 dx))
          (define yy2 (+ y12 dy))
          (define zz  (* (- z2 z1) APPLY-SIZE))
          (define z11 (+ z1 zz))
          (define z22 (- z2 zz))
          (make-application1
           (draw 1st lev1 left? top? front? xx1 yy1 z1 xx2 yy2 z11 .0)
           (draw 2nd lev1 left? top? front? xx1 yy1 z22 xx2 yy2 z2 .0))]
         [else (error 'draw-expr "Unknown composite expr -- ~s." expr)])]
     [(simple-expr? expr)
      (unless (eq? 'transparent (if (var-expr? expr) (var-val expr) expr))
        (let* (;;
               ;; Calculate points:
               ;;
               ;;  xx0xx1    xx2 xx3
               ;;  |  |      |   |
               ;;  P---------P   -- yy0
               ;;  |\         \
               ;;  | \         \
               ;;  |  P---------P-- yy1
               ;;  |  |         |
               ;;  P  |         |-- yy2
               ;;   \ |         |
               ;;    \|         |
               ;;     P---------P-- yy3
               ;;
               ;;  (xx1 - x1) = (x2 - xx2) = (3D-DX * (x2 - x1))
               ;;   and the same for y values
               ;;
               [dx  (* 3D-DX (- z2 z1))]
               [xx0 (+ x1 (* z1 3D-DX))]
               [xx1 (+ xx0 dx)]
               [xx2 (+ x2 (* z1 3D-DX))]
               [xx3 (+ xx2 dx)]
               [dy  (* 3D-DY (- z2 z1))]
               [yy0 (+ y1 (* z1 3D-DY))]
               [yy1 (+ yy0 dy)]
               [yy2 (+ y2 (* z1 3D-DY))]
               [yy3 (+ yy2 dy)]
               [xx00 (+ (rnd (* (/ xx0 (+ 1 3D-DX)) dc-size)) dc-ofs)]
               [xx11 (+ (rnd (* (/ xx1 (+ 1 3D-DX)) dc-size)) dc-ofs)]
               [xx22 (+ (rnd (* (/ xx2 (+ 1 3D-DX)) dc-size)) dc-ofs)]
               [xx33 (+ (rnd (* (/ xx3 (+ 1 3D-DX)) dc-size)) dc-ofs)]
               [yy00 (+ (rnd (* (/ yy0 (+ 1 3D-DY)) dc-size)) dc-ofs)]
               [yy11 (+ (rnd (* (/ yy1 (+ 1 3D-DY)) dc-size)) dc-ofs)]
               [yy22 (+ (rnd (* (/ yy2 (+ 1 3D-DY)) dc-size)) dc-ofs)]
               [yy33 (+ (rnd (* (/ yy3 (+ 1 3D-DY)) dc-size)) dc-ofs)])
          (set-pen/brush
           dc
           (cond
            [(var-expr? expr) (cdr (assq (var-val expr) COLORS))]
            [(symbol? expr)   (cdr (assq expr COLORS))]
            [else ; explicit color
             (send tmp-color set
                   (vector-ref expr 0) (vector-ref expr 1) (vector-ref expr 2))
             (list DK-PEN (send the-brush-list find-or-create-brush
                                tmp-color 'solid))]))
          (draw-polygon xx11 yy11 xx00 yy00 xx22 yy00 xx33 yy11)
          (draw-polygon xx11 yy11 xx00 yy00 xx00 yy22 xx11 yy33)
          (draw-polygon xx11 yy11 xx33 yy11 xx33 yy33 xx11 yy33)))
      expr]
     [else (error 'draw-expr "Unknown expr -- ~s." expr)]))
  (dynamic-wind
    begin-busy-cursor
    (λ() (send dc clear)
         (set-pen/brush dc (BG-PEN/BRUSH))
         (send dc draw-rectangle 1 1 size size)
         (begin0 (draw expr (if eval? EVAL-DEPTH 0)
                       #t #t #t .0 .0 .0 1.0 1.0 1.0 .0)
           (when name
             (define-values [tw th d a] (send dc get-text-extent name))
             (define tw* (min tw (- size 6)))
             (set-pen/brush dc (BG-PEN/BRUSH))
             (send dc draw-rectangle (- size tw* 3) 1 (+ 3 tw*) (+ 2 th))
             (send dc draw-text name (max 0 (- size tw* 1)) 2))))
    end-busy-cursor))

;;;============================================================================
;;; GUI

(define gcalc-frame
  (instantiate
   (class frame%
     (define/augment (on-close) (maybe-save-and-exit))
     (define/public (open-file file) (open file))
     (super-instantiate ["GCalc"] [style '(no-resize-border)])
     (send this stretchable-width #f)
     (send this stretchable-height #f))
   []))

(define main-pane
  (instantiate horizontal-pane% [gcalc-frame]))

(define help
  (show-scribbling
   '(lib "games/scribblings/games.scrbl")
   "gcalc"))

(define file-name #f)
(define modified? #f)
(define (set-file-name! name)
  (set! file-name name)
  (send gcalc-frame set-label (string-append "GCalc: " name)))
(define (save)
  (if file-name
    (begin
      (when (file-exists? file-name) (delete-file file-name))
      (with-output-to-file file-name
        (λ() (dynamic-wind
               begin-busy-cursor
               (λ() (define (out x) (write x) (newline))
                    (out "GCALC")
                    (for ([c (in-list customs)]) (out ((custom-getter c))))
                    (out (send main-cell get-contents))
                    (out (get-storage-contents))
                    (set! modified? #f))
               end-busy-cursor)
             (message-box "Save" (format "~s saved." file-name)
                          gcalc-frame '(ok)))))
    (save-as)))
(define (open-examples)
  (open (path->string (collection-file-path "gcalc-examples.rktd"
                                            "games" "gcalc"))))

(define (open [file (void)])
  (maybe-save)
  (define f
    (if (not (void? file))
      file
      (cond [(get-file "Open" gcalc-frame) => path->string] [else #f])))
  (when f
    (if (file-exists? f)
      (with-input-from-file f
        (λ() (dynamic-wind
               begin-busy-cursor
               (λ() (with-handlers
                        ([exn:fail?
                          (λ(x) (message-box
                                 "Open" (format "~s is not a GCalc file." f)
                                 gcalc-frame '(ok)))])
                      (or (equal? "GCALC" (read)) (error "gcalc"))
                      (set-file-name! f)
                      (for ([c (in-list customs)]) ((custom-setter c) (read)))
                      (send main-cell set-contents! (validate-contents (read)))
                      (set-storage-contents! (map validate-contents (read)))
                      (set! modified? #f)))
               end-busy-cursor)))
      (message-box "Open" (format "~s does not exists." f)
                   gcalc-frame '(ok)))))
(define (save-as)
  (define f (get-file "Save-as" gcalc-frame))
  (when f
    (if (directory-exists? f)
      (message-box "Save-as" (format "\"~a\" is a directory." f)
                   gcalc-frame '(ok))
      (when (or (not (file-exists? f))
                (eq? 'yes (message-box "Save-as"
                                       (format "\"~a\" exists, overwrite?" f)
                                       gcalc-frame '(yes-no))))
        (set-file-name! (path->string f))
        (save)))))
(define (maybe-save)
  (when (and modified?
             (begin (bell)
                    (eq? 'yes (message-box "GCalc" "Save modifications?"
                                           gcalc-frame '(yes-no)))))
    (save)))
(define (maybe-save-and-exit)
  (maybe-save)
  (set! modified? #f) ; can appear again from drracket
  (send gcalc-frame show #f))

(define set-options
  (let ([dlg (instantiate dialog% ["GCalc Expression" gcalc-frame])])
    (define ok? #f)
    (define inits  (λ() (set! ok? #f)))
    (define finals (λ() (set! modified? #t)))
    (define (add-init/final initializer finalizer)
      (let ([c inits])  (set! inits  (λ() (initializer) (c))))
      (let ([c finals]) (set! finals (λ() (finalizer)   (c)))))
    (define (new-row . a)
      (define p (instantiate horizontal-pane% [dlg]))
      (send p set-alignment (if (null? a) 'left (car a)) 'center)
      p)
    (define (make-check-box getter setter title)
      (define cb (instantiate check-box% [title (new-row) void]))
      (add-init/final (λ() (send cb set-value (getter)))
                      (λ() (setter (send cb get-value)))))
    (define (make-check/slide getter setter title range)
      (define row (new-row))
      (define toggle
        (instantiate check-box%
          [title row
           (let ([saved 0])
             (λ(this e)
               (if (send this get-value)
                 (send slider set-value saved)
                 (begin (set! saved (send slider get-value))
                        (send slider set-value 0)))))]))
      (define slider
        (instantiate slider%
          ["" 0 range row
           (λ(this e)
             (send toggle set-value (not (zero? (send this get-value)))))]))
      (add-init/final (λ() (define val (getter))
                           (send slider set-value (or val 0))
                           (send toggle set-value (and val #t)))
                      (λ() (setter (and (send toggle get-value)
                                        (send slider get-value))))))
    (define (make-ok-cancel)
      (let ([row (new-row 'center)])
        (instantiate button%
          ["&OK" row (λ(this e) (set! ok? #t) (send dlg show #f))]
          [style '(border)])
        (instantiate button%
          ["&Cancel" row (λ(this e) (send dlg show #f))])))
    ;; Dialog components
    (for ([c (in-list customs)])
      (define type   (custom-type        c))
      (define getter (custom-getter      c))
      (define setter (custom-setter      c))
      (define desc   (custom-description c))
      (cond [(eq? type 'bool) (make-check-box getter setter desc)]
            [(and (pair? type) (eq? (1st type) 'int))
             (make-check/slide getter setter desc (2nd type))]))
    (make-ok-cancel)
    ;; Main
    (λ() (inits) (send dlg show #t) (when ok? (finals)))))

(define cell-menu-items   `((#\x      "C&ut"           cut:)
                            (#\c      "&Copy"          copy:)
                            (#\v      "Pas&te"         paste:)
                            (#\r      "Clea&r"         clear:)
                            (#\e      "&Eval"          eval:)
                            (#\n      "Re&name"        rename:)
                            (#\space  "Sho&w"          show:)
                            (#\p      "&Print"         print:)))
(define global-menu-items `((#\h      "&Help"          ,help)
                            (#\o      "&Open"          ,open)
                            (#\m      "Open-Exa&mples" ,open-examples)
                            (#\s      "&Save"         ,save)
                            (#\a      "Save-&as"      ,save-as)
                            (#\return "Pre&ferences"  ,set-options)
                            (#\q      "&Quit"         ,maybe-save-and-exit)))

(define popup-cell-menu
  (let ([menu (instantiate popup-menu% ("GCalc"))]
        [this #f]
        [cell-items '()])
    (define (make-item mi)
      (cons
       (instantiate menu-item%
        [(string-append "[" (case (1st mi)
                              [(#\space) "SPC"] [(#\return) "RET"]
                              [else (string (1st mi))])
                        "]  " (2nd mi))
         menu
         (λ(i e)
           (define f (3rd mi))
           ((if (symbol? f) (λ() ((send this get-cell-op f) e)) f)))])
       mi))
    (set! cell-items (map make-item cell-menu-items))
    (instantiate separator-menu-item% [menu])
    (for-each make-item global-menu-items)
    (λ(cell e x y)
      (set! this cell)
      (define ok? (not (null-expr? (send this get-expr))))
      (for ([ci (in-list cell-items)])
        (send (1st ci) enable ((send cell get-cell-op (4th ci)) 'enabled? e)))
      (send cell popup-menu menu x y))))

(define cells '())
(define (find-cell x y)
  (for/or ([c (in-list cells)])
    (define-values [x* y*] (send c screen->client x y))
    (and (< -1 x* (send c get-width)) (< -1 y* (send c get-height)) c)))

(define current-cell #f)

(define cell%
  (class canvas%
    (init-field name expr draggable? dropper alt-func size parent)
    (inherit get-dc)
    (define bitmap (instantiate bitmap% [size size]))
    (define dc     (instantiate bitmap-dc% [bitmap]))
    ;; general operations
    (define evaluate-next #f)
    (define/private (draw-contents)
      (unhighlight!)
      (set! expr (draw-expr dc expr name evaluate-next))
      (set! evaluate-next #f)
      (on-paint))
    (define/public (get-expr) expr)
    (define/public (set-expr! e) (set-contents! (cons #f e)))
    (define/public (get-contents) (cons name expr))
    (define/public (set-contents! n/e)
      (cond [(eq? dropper 'copy)
             (set! modified? #t)
             (set! name (car n/e))
             (set! expr (cdr n/e))
             (draw-contents)]
            [dropper (dropper n/e)]
            [else #f]))
    (define/public (eval-next-expr) (set! evaluate-next #t))
    (define/public (get-dropper) dropper)
    ;; highlighting
    (define highlighted? #f)
    (define/public (highlight!)
      (unless highlighted? (set! highlighted? #t) (on-paint)))
    (define/public (unhighlight!)
      (when highlighted? (set! highlighted? #f) (on-paint)))
    ;; cell operations
    (define (make-cell-op: op . enabled?)
      (let ([enabled?
             (cond [(null? enabled?) (λ(e) (not (null-expr? expr)))]
                   [(not (procedure? (car enabled?)))
                    (λ(e) (and (car enabled?) (not (null-expr? expr))))]
                   [else (car enabled?)])])
        (λ(e . more)
          (let ([enabled? (enabled? (if (eq? e 'enabled?) (car more) e))])
            (cond [(eq? e 'enabled?) enabled?] [enabled? (op e)])))))
    (define cut:
      (make-cell-op: (λ(e) (copy: e) (clear: e)) (and dropper #t)))
    (define copy:
      (make-cell-op: (λ(e) (send the-clipboard set-clipboard-string
                                 (write-to-string (get-contents))
                                 (send e get-time-stamp)))))
    (define paste:
      (make-cell-op: (λ(e) (set-contents!
                            (read-from-string
                             (send the-clipboard get-clipboard-string
                                   (send e get-time-stamp)))))
                     (λ(e) (and dropper
                                (valid-contents?
                                 (read-from-string
                                  (send the-clipboard get-clipboard-string
                                        (send e get-time-stamp))))))))
    (define clear:
      (make-cell-op: (λ(e) (set-contents! (cons #f null-expr)))
                     (and dropper #t)))
    (define show:
      (make-cell-op: (λ(e) (unhighlight!) (show-expr expr name))))
    (define print:
      (make-cell-op: (λ(e) (unhighlight!) (print-expr expr name))))
    (define eval:
      (make-cell-op: (λ(e) (eval-next-expr) (draw-contents))
                     (and dropper #t)))
    (define rename:
      (make-cell-op: (λ(e) (define new (get-text-from-user
                                        "GCalc" "Enter a new name" gcalc-frame
                                        (or name "")))
                           (when new
                             (set! modified? #t)
                             (set! name new)
                             (draw-contents)))
                     (and dropper #t)))
    (define/public (get-cell-op msg)
      (case msg
        [(cut:) cut:] [(copy:) copy:] [(paste:) paste:] [(clear:) clear:]
        [(show:) show:] [(print:) print:] [(eval:) eval:] [(rename:) rename:]))
    ;; events
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (send dc draw-bitmap bitmap 0 0)
        (when highlighted?
          (set-pen/brush dc HIGHLIGHT-PEN/BRUSH)
          (let ([w1 (round (/ HIGHLIGHT-WIDTH 2))]
                [w2 (- size HIGHLIGHT-WIDTH -1)])
            (send dc draw-rectangle w1 w1 w2 w2)))))
    (define right-menu-thread #f)
    (define dragging? #f)
    (define drag-to   #f)
    (define last-click-time #f)
    (define/override (on-event e)
      (when (and right-menu-thread (not (send e get-right-down)))
        (kill-thread right-menu-thread)
        (set! right-menu-thread #f))
      (case (send e get-event-type)
        [(enter)
         (set! current-cell this)
         (send this focus)
         (when (and draggable? (not (null-expr? expr))) (highlight!))]
        [(leave)
         (unless dragging? (set! current-cell #f) (unhighlight!))]
        [(left-down)
         (let ([d? (and last-click-time
                        (< (- (current-milliseconds) last-click-time)
                           DOUBLE-MILISECS))])
           (set! last-click-time (if d? #f (current-milliseconds)))
           (if d?
             (show: e)
             (begin (set! dragging? #t) (set! drag-to #f))))]
        [(left-up)
         (set! dragging? #f)
         (when drag-to
           (send drag-to set-contents! (get-contents))
           (when (and (not (send e get-shift-down))
                      (not (eq? drag-to main-cell))
                      (eq? 'copy (send drag-to get-dropper)))
             (clear: e)))]
        [(right-down)
         (if alt-func
           (set! right-menu-thread
                 (thread
                  (λ() (sleep 0.3)
                       (queue-callback
                        (λ() (popup-cell-menu this e
                                              (send e get-x) (send e get-y))))
                       (set! right-menu-thread #f))))
           (popup-cell-menu this e (send e get-x) (send e get-y)))]
        [(right-up)
         (when right-menu-thread
           (kill-thread right-menu-thread)
           (set! right-menu-thread #f))
         (when (and alt-func (not (null-expr? (send main-cell get-expr))))
           (alt-func this))]
        [(middle-down)
         (show: e)]
        [(motion)
         (when dragging?
           (let*-values ([(x y) (send this client->screen
                                      (send e get-x) (send e get-y))]
                         [(c) (find-cell x y)])
             (when (and c (let ([cdrop (send c get-dropper)])
                            (or (eq? c this) (not cdrop)
                                (and (not (eq? cdrop 'copy))
                                     (null-expr? (send main-cell get-expr))))))
               (set! c #f))
             (unless (eq? c drag-to)
               (when drag-to (send drag-to unhighlight!))
               (when c (send c highlight!))
               (set! drag-to c))))]))
    (define/override (on-char e)
      (let ([ch (send e get-key-code)])
        (when (eq? this current-cell)
          (cond [(memq ch '(escape f10))
                 (popup-cell-menu this e (send e get-x) (send e get-y))]
                [(eq? ch 'f1) (help)]
                [(assq ch cell-menu-items)
                 => (λ(mi) ((send this get-cell-op (3rd mi)) e))]
                [(assq ch global-menu-items)
                 => (λ(mi) ((3rd mi)))]))))
    ;; initialization
    (set! cells (cons this cells))
    (when (and (not name) (symbol? expr)) (set! name (symbol->string expr)))
    (super-instantiate [parent])
    (send* this (min-width size) (min-height size))
    (send dc set-text-mode 'solid)
    (send dc set-font CELL-FONT)
    (draw-contents)))

(define show-expr
  (let ([dlg (instantiate dialog% ["GCalc Expression" gcalc-frame]
                                  [style '(no-caption)])])
    (define bmp (instantiate bitmap% [SHOW-CELL-SIZE SHOW-CELL-SIZE]))
    (define dc  (instantiate bitmap-dc% [bmp]))
    (define cnv
      (instantiate
       (class canvas%
         (inherit get-dc)
         (define/override (on-event e)
           (when (send e button-down?) (send dlg show #f)))
         (define/override (on-char e)
           (unless (memq (send e get-key-code) '(release #\nul control shift))
             (send dlg show #f)))
         (define/override (on-paint)
           (send (get-dc) draw-bitmap bmp 0 0))
         (super-instantiate [dlg])
         (send* this (min-width SHOW-CELL-SIZE) (min-height SHOW-CELL-SIZE)))
       []))
    (define cdc (send cnv get-dc))
    (define last-expr #f)
    (λ(expr name)
      (send dc set-text-mode 'solid)
      (send dc set-font SHOW-FONT)
      (unless (eq? last-expr expr)
        (draw-expr dc expr name)
        (set! last-expr expr))
      (send cdc draw-bitmap bmp 0 0)
      (send dlg show #t))))

(define (print-expr expr name)
  (define dc (instantiate post-script-dc% []))
  (send dc start-doc "Printing...")
  (send dc start-page)
  (parameterize ([BG-PEN/BRUSH (list (instantiate pen%   ["BLACK" 1 'solid])
                                     (instantiate brush% ["WHITE" 'solid]))])
    (if (or NOBMP-PRINT (< (expr-size expr) 5000))
      (draw-expr dc expr name)
      (let* ([size  (let-values ([(sx sy) (send dc get-size)])
                      (inexact->exact (min sx sy)))]
             [bmp   (instantiate bitmap% [size size])]
             [bmpdc (instantiate bitmap-dc% [bmp])])
        (message-box "Printing" "The expression, is too complex, using bitmap."
                     gcalc-frame '(ok))
        (draw-expr bmpdc expr name)
        (send dc draw-bitmap bmp 0 0))))
  (send dc end-page)
  (send dc end-doc))

(define tiled-panel%
  (class vertical-panel%
    (init-field width size)
    (define current-pane #f)
    (define left 0)
    (define/public (add-cell name expr draggable? dropper . alt-func)
      (set! alt-func (and (not (null? alt-func)) (car alt-func)))
      (when (zero? left)
        (set! current-pane (instantiate horizontal-pane% [this]))
        (set! left width))
      (set! left (sub1 left))
      (instantiate cell%
        [name expr draggable? dropper alt-func size current-pane]))
    (super-instantiate [main-pane] [style '(border)])))

;; colors
(define colors-panel
  (instantiate tiled-panel% [COLOR-CELL-ROW COLOR-CELL-SIZE]))
(for ([c (in-list COLORS)])
  (send colors-panel add-cell #f (car c) #t #f
        (λ(this)
          (send main-cell set-expr!
                (make-abstraction (send this get-expr)
                                  (send main-cell get-expr))))))

;; operators
(define operator-panel
  (instantiate tiled-panel% [OPERS-CELL-ROW OPERS-CELL-SIZE]))
(define (make-dropper name maker op 1st?)
  (send operator-panel add-cell (string-append "  " name "  ") #f #f
        (λ(n/e)
          (when (and EVAL-NOW (eq? op 'apply)) (send main-cell eval-next-expr))
          (send main-cell set-expr!
                (maker (cdr n/e) (send main-cell get-expr))))
        (λ(this)
          (send main-cell set-expr!
                ((if 1st? expr-1st expr-2nd)
                 (split-expr (send main-cell get-expr) op))))))
(make-dropper "Back"         make-back-front   '/     #t)
(make-dropper "Top"          make-top-bottom   '-     #f)
(make-dropper "Apply (func)" make-application1 'apply #f)
(make-dropper "Left"         make-left-right   '\|    #f)
(define main-cell (send operator-panel add-cell "  Main  " #f #t 'copy))
(make-dropper "Right"        make-right-left   '\|    #t)
(make-dropper "Apply (arg)"  make-application2 'apply #t)
(make-dropper "Bottom"       make-bottom-top   '-     #t)
(make-dropper "Front"        make-front-back   '/     #f)
(send main-cell focus)

;; storage
(define store-panel
  (instantiate tiled-panel% [STORE-CELL-ROW STORE-CELL-SIZE]))
(define storage-cells
  (let loop ([n (* STORE-CELL-ROW STORE-CELL-COL)] [cells '()])
    (if (zero? n)
      (reverse cells)
      (loop (sub1 n)
            (cons (send store-panel add-cell #f #f #t 'copy) cells)))))
(define (get-storage-contents)
  (for/list ([c (in-list storage-cells)]) (send c get-contents)))
(define (set-storage-contents! names/exprs)
  (for ([c (in-list storage-cells)] [n/e (in-list names/exprs)])
    (send c set-contents! n/e)))

;; start the whole thing
(send gcalc-frame show #t)

))
