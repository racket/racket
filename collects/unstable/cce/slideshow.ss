#lang scheme

(require slideshow/base slideshow/pict
         scheme/splicing scheme/stxparam scheme/gui/base
         unstable/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter with-size current-font-size)
(define-syntax-rule (with-scale scale expr)
  (with-size (inexact->exact (ceiling (* scale (current-font-size)))) expr))
(define-syntax-rule (define-scale name scale)
  (define-syntax-rule (name expr) (with-scale scale expr)))
(define-scale big 3/2)
(define-scale small 2/3)

(define-with-parameter with-font current-main-font)
(define-syntax-rule (with-style style expr)
  (with-font (cons style (current-main-font)) expr))
(define-syntax-rule (define-style name style)
  (define-syntax-rule (name expr) (with-style style expr)))
(define-style bold 'bold)
(define-style italic 'italic)
(define-style subscript 'subscript)
(define-style superscript 'superscript)
(define-style caps 'caps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Picture Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fill pict w h)
  (cc-superimpose
   pict
   (blank (or w (pict-width pict))
          (or h (pict-height pict)))))

(define (color c p) (colorize p c))

(define color/c
  (or/c string? ;; might be faster
        ;;(and/c string? (lambda (s) (send the-color-database find-color s)))
        (is-a?/c color%)
        (list/c byte? byte? byte?)))

(define-syntax-rule (define-colors name ...)
  (begin (define (name pict) (color (symbol->string 'name) pict)) ...))

(define-colors
  red orange yellow green blue purple
  black brown gray white cyan magenta)

(define (light c) (scale-color 2 c))
(define (dark c) (scale-color 1/2 c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide / Paragraph Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter column current-para-width)

(define (columns . picts)
  (apply hc-append gap-size (map baseless picts)))

(define (column-size n [r (/ n)])
  (* r (- (current-para-width) (* (sub1 n) gap-size))))

(define-syntax-rule (two-columns a b)
  (columns (column (column-size 2) a)
           (column (column-size 2) b)))

(define (mini-slide . picts)
  (apply vc-append gap-size picts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define =!
  (case-lambda
    [(n) n]
    [(n . ns)
     (if (apply = n ns)
         n
         (error '=! "not all equal: ~a" (cons n ns)))]))

(define (elem->pict elem)
  (if (string? elem) (t elem) elem))

(define (tabular #:gap [gap gap-size]
                 #:vgap [vgap gap]
                 #:hgap [hgap gap]
                 #:align [align lbl-superimpose]
                 #:halign [halign align]
                 #:valign [valign align]
                 . cells)
  (let* ([rows (length cells)]
         [cols (apply =! (map length cells))]
         [picts (map elem->pict (append* cells))]
         [haligns (for/list ([i (in-range 0 cols)]) halign)]
         [valigns (for/list ([i (in-range 0 rows)]) valign)]
         [hseps (for/list ([i (in-range 1 cols)]) hgap)]
         [vseps (for/list ([i (in-range 1 rows)]) vgap)])
    (table cols picts haligns valigns hseps vseps)))

(define (matrixof c)
  (and/c (listof (listof c))
         (flat-named-contract "matrix"
           (match-lambda
             [(list) #t]
             [(list _) #t]
             [(list xs ...) (apply = (map length xs))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Space-smart picture selection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parameter pict-combine #'ltl-superimpose)

(define-syntax-rule (with-pict-combine combine body ...)
  (splicing-syntax-parameterize
   ([pict-combine #'combine])
   body ...))

(define-syntax (pict-if stx)
  (syntax-case stx ()
    [(_ #:combine combine test success failure)
     (syntax/loc stx
       (let* ([result test])
         (combine (show success result)
                  (hide failure result))))]
    [(_ test success failure)
     (quasisyntax/loc stx
       (pict-if #:combine #,(syntax-parameter-value #'pict-combine)
                test success failure))]))

(define-syntax (pict-cond stx)
  (syntax-case stx (else)
    [(_ #:combine combine [test expr] ... [else default])
     (with-syntax ([(pict ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([pict expr] ... [final default])
           (combine (cond [test pict] ... [else final])
                    (ghost pict) ... (ghost final)))))]
    [(_ #:combine combine [test pict] ...)
     (syntax/loc stx
       (pict-cond #:combine combine [test pict] ... [else (blank 0 0)]))]
    [(_ [test expr] ...)
     (quasisyntax/loc stx
       (pict-cond #:combine #,(syntax-parameter-value #'pict-combine)
                  [test expr] ...))]))

(define-syntax (pict-case stx)
  (syntax-case stx (else)
    [(_ test #:combine combine [literals expr] ... [else default])
     (with-syntax ([(pict ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([pict expr] ... [final default])
           (combine (case test [literals pict] ... [else final])
                    (ghost pict) ... (ghost final)))))]
    [(_ test #:combine combine [literals expr] ...)
     (syntax/loc stx
       (pict-case #:combine combine [literals expr] ... [else (blank 0 0)]))]
    [(_ test [literals expr] ...)
     (quasisyntax/loc stx
       (pict-case test #:combine #,(syntax-parameter-value #'pict-combine)
                  [literals expr] ...))]))

(define-syntax (pict-match stx)
  (syntax-case stx ()
    [(_ test #:combine combine [pattern expr] ...)
     (with-syntax ([(pict ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([pict expr] ...)
           (combine (match test [pattern pict] ... [_ (blank 0 0)])
                    (ghost pict) ...))))]
    [(_ test [pattern expr] ...)
     (quasisyntax/loc stx
       (pict-match test #:combine #,(syntax-parameter-value #'pict-combine)
                   [pattern expr] ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide Staging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (stage-keyword stx)
  (raise-syntax-error #f "not in the body of a staged slide" stx))

(define-syntax-parameter stage stage-keyword)
(define-syntax-parameter stage-name stage-keyword)

(define-syntax (staged stx)
  (syntax-case stx ()
    [(_ [name ...] body ...)
     (let* ([ids (syntax->list #'(name ...))])

       (for ([id (in-list ids)] #:when (not (identifier? id)))
         (raise-syntax-error #f "expected an identifier" stx id))

       (with-syntax ([(num ...)
                      (for/list ([i (in-naturals 1)] [id (in-list ids)])
                        (datum->syntax #'here i id))])

         (syntax/loc stx
           (let* ([name num] ...)
             (define (staged-computation number symbol)
               (syntax-parameterize
                   ([stage (make-rename-transformer #'number)]
                    [stage-name (make-rename-transformer #'symbol)])
                 (block body ...)))
             (begin (staged-computation name 'name) ...)))))]))

(define-syntax-rule (slide/stage [name ...] body ...)
  (staged [name ...] (slide body ...)))

(define-syntax-rule (before name) (< stage name))
(define-syntax-rule (before/at name) (<= stage name))
(define-syntax-rule (at/after name) (>= stage name))
(define-syntax-rule (after name) (> stage name))
(define-syntax-rule (before/after name) (not (= stage name)))
(define-syntax-rule (at name ...) (or (= stage name) ...))

(define (hide pict [hide? #t])
  (if hide? (ghost pict) pict))

(define (show pict [show? #t])
  (if show? pict (ghost pict)))

(define (shade pict [shade? #t] #:ratio [ratio 0.5])
  (if shade? (cellophane pict ratio) pict))

(define (strike pict [strike? #t])
  (if strike?
      (pin-over pict
                0
                (/ (pict-height pict) 2)
                (pip-line (pict-width pict) 0 0))
      pict))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide with-size with-scale big small
         with-font with-style bold italic subscript superscript caps)

(provide/contract
 [color/c flat-contract?]
 [red     (-> pict? pict?)]
 [orange  (-> pict? pict?)]
 [yellow  (-> pict? pict?)]
 [green   (-> pict? pict?)]
 [blue    (-> pict? pict?)]
 [purple  (-> pict? pict?)]
 [black   (-> pict? pict?)]
 [brown   (-> pict? pict?)]
 [gray    (-> pict? pict?)]
 [white   (-> pict? pict?)]
 [cyan    (-> pict? pict?)]
 [magenta (-> pict? pict?)]
 [light (-> color/c color/c)]
 [dark (-> color/c color/c)]
 [color (-> color/c pict? pict?)]
 [fill
  (-> pict?
      (or/c (real-in 0 +inf.0) #f)
      (or/c (real-in 0 +inf.0) #f)
      pict?)])

(provide column columns column-size two-columns mini-slide)

(provide/contract
 [tabular (->* []
               [#:gap natural-number/c
                #:hgap natural-number/c
                #:vgap natural-number/c
                #:align (->* [] [] #:rest (listof pict?) pict?)
                #:halign (->* [] [] #:rest (listof pict?) pict?)
                #:valign (->* [] [] #:rest (listof pict?) pict?)]
               #:rest (matrixof (or/c string? pict?))
               pict?)])

(provide/contract
 [hide (->* [pict?] [any/c] pict?)]
 [show (->* [pict?] [any/c] pict?)]
 [strike (->* [pict?] [any/c] pict?)]
 [shade (->* [pict?] [any/c #:ratio (real-in 0 1)] pict?)])
(provide staged slide/stage stage stage-name
         before at after before/at at/after except
         pict-if pict-cond pict-case pict-match
         pict-combine with-pict-combine)
