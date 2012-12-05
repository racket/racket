#lang racket/base
(require racket/class
         racket/pretty
         racket/gui/base
         racket/promise
         "pretty-helper.rkt"
         "interfaces.rkt")
(provide pretty-print-syntax)

;; FIXME: Need to disable printing of structs with custom-write property

;; pretty-print-syntax : syntax port partition number SuffixOption hasheq number bool
;;                    -> range%
(define (pretty-print-syntax stx port
                             primary-partition colors suffix-option styles columns abbrev?)
  (define range-builder (new range-builder%))
  (define-values (datum ht:flat=>stx ht:stx=>flat)
    (syntax->datum/tables stx primary-partition colors suffix-option abbrev?))
  (define identifier-list
    (filter identifier? (hash-map ht:stx=>flat (lambda (k v) k))))
  (define (flat=>stx obj)
    (hash-ref ht:flat=>stx obj #f))
  (define (stx=>flat stx)
    (hash-ref ht:stx=>flat stx))
  (define (current-position)
    (let-values ([(line column position) (port-next-location port)])
      (sub1 position)))
  (define (pp-pre-hook obj port)
    (when (flat=>stx obj)
      (send range-builder push! (current-position)))
    (send range-builder set-start obj (current-position)))
  (define (pp-post-hook obj port)
    (define stx (flat=>stx obj))
    (when stx
      (send range-builder pop! stx (current-position)))
    (let ([start (send range-builder get-start obj)]
          [end (current-position)])
      (when (and start stx)
        (send range-builder add-range stx (cons start end)))))

  (unless (syntax? stx)
    (raise-type-error 'pretty-print-syntax "syntax" stx))
  (parameterize 
   ([pretty-print-pre-print-hook pp-pre-hook]
    [pretty-print-post-print-hook pp-post-hook]
    [pretty-print-size-hook pp-size-hook]
    [pretty-print-print-hook pp-print-hook]
    [pretty-print-remap-stylable pp-remap-stylable]
    [pretty-print-abbreviate-read-macros abbrev?]
    [pretty-print-current-style-table (pp-better-style-table styles)]
    [pretty-print-columns columns])
   (pretty-print/defaults datum port)
   (new range%
        (range-builder range-builder)
        (identifier-list identifier-list))))

(define (pp-print-hook obj display-like? port)
  (cond [(syntax-dummy? obj)
         ((if display-like? display write) (syntax-dummy-val obj) port)]
        [(is-a? obj editor-snip%)
         (write-special obj port)]
        [else 
         (error 'pretty-print-hook "unexpected special value: ~e" obj)]))

(define (pp-size-hook obj display-like? port)
  (cond [(is-a? obj editor-snip%)
         (pretty-print-columns)]
        [(syntax-dummy? obj)
         (let ((ostring (open-output-string)))
           ((if display-like? display write) (syntax-dummy-val obj) ostring)
           (string-length (get-output-string ostring)))]
        [else #f]))

(define (pp-remap-stylable obj)
  (and (id-syntax-dummy? obj)
       (let ([remap (id-syntax-dummy-remap obj)])
         (and (not (memq remap special-expression-keywords))
              remap))))

(define (pp-better-style-table styles)
  (define style-list (for/list ([(k v) (in-hash styles)]) (cons k v)))
  (pretty-print-extend-style-table
   (basic-style-list)
   (map car style-list)
   (map cdr style-list)))

(define (basic-style-list)
  (pretty-print-extend-style-table
   (pretty-print-current-style-table)
   (map car basic-styles)
   (map cdr basic-styles)))
(define basic-styles
  '((define-values          . define)
    (define-syntaxes        . define-syntax)
    (define-for-syntax      . define)
    (define-values-for-syntax . define))
  #|
  ;; Messes up formatting too much :(
  (let* ([pref (pref:tabify)]
         [table (car pref)]
         [begin-rx (cadr pref)]
         [define-rx (caddr pref)]
         [lambda-rx (cadddr pref)])
    (let ([style-list (hash-table-map table cons)])
      (pretty-print-extend-style-table
       (basic-style-list)
       (map car style-list)
       (map cdr style-list))))
  |#)

(define-local-member-name range:get-ranges)

;; range-builder%
(define range-builder%
  (class object%
    (define starts (make-hasheq))
    (define ranges (make-hasheq))

    (define/public (set-start obj n)
      (hash-set! starts obj n))

    (define/public (get-start obj)
      (hash-ref starts obj #f))

    (define/public (add-range obj range)
      (hash-set! ranges obj (cons range (get-ranges obj))))

    (define (get-ranges obj)
      (hash-ref ranges obj null))

    (define/public (range:get-ranges) ranges)

    ;; ----

    (define/public (get-subs)
      working-subs)

    (define working-start #f)
    (define working-subs null)
    (define saved-starts null)
    (define saved-subss null)

    (define/public (push! start)
      (set! saved-starts (cons working-start saved-starts))
      (set! saved-subss (cons working-subs saved-subss))
      (set! working-start start)
      (set! working-subs null))

    (define/public (pop! stx end)
      (define latest (make-treerange stx working-start end (reverse working-subs)))
      (set! working-start (car saved-starts))
      (set! working-subs (car saved-subss))
      (set! saved-starts (cdr saved-starts))
      (set! saved-subss (cdr saved-subss))
      (set! working-subs (cons latest working-subs)))

    (super-new)))

;; range%
(define range%
  (class* object% (range<%>)
    (init range-builder)
    (init-field identifier-list)
    (super-new)

    (define ranges (hash-copy (send range-builder range:get-ranges)))
    (define subs (reverse (send range-builder get-subs)))

    (define/public (get-ranges obj)
      (hash-ref ranges obj null))

    (define/public (get-treeranges)
      subs)

    (define/public (all-ranges)
      (force sorted-ranges))

    (define/public (get-identifier-list)
      identifier-list)

    (define sorted-ranges
      (delay
        (sort 
         (apply append 
                (hash-map
                 ranges
                 (lambda (k vs)
                   (map (lambda (v) (make-range k (car v) (cdr v))) vs))))
         (lambda (x y)
           (>= (- (range-end x) (range-start x))
               (- (range-end y) (range-start y)))))))
    ))
