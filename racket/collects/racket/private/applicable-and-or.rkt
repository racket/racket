;; -----------------------------------------------------------------------------
;; applicable expansions for and, or

(module applicable-and-or '#%kernel
  (#%require (prefix s: "small-scheme.rkt")
             "define.rkt" "misc.rkt" "for.rkt"
             (for-syntax "stxcase-scheme.rkt"))
  
  (#%provide and or)
  
  (define-syntax-rule (define/renamed rename (name . args) body ...)
    (define name
      (procedure-rename
       (λ args body ...)
       'rename)))

  (define/renamed and (applicable-and . args)
    (for/fold ([v #t])
              ([arg (in-list args)])
      #:break (not v)
      (s:and v arg)))
  
  (define/renamed or (applicable-or . args)
    (for/fold ([v #f])
              ([arg (in-list args)])
      #:break v
      (s:or v arg)))

  (define-syntax and
    (syntax-id-rules ()
      [(_ . args) (s:and . args)]
      [_ applicable-and]))
  
  (define-syntax or
    (syntax-id-rules ()
      [(_ . args) (s:or . args)]
      [_ applicable-or])))
