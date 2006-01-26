(module environment mzscheme

  (require (lib "contract.ss")
           (prefix env: (planet "environment.ss" ("cobbe" "environment.plt" 2 1)))
           )

  (provide/contract
   [mapping? (any/c . -> . boolean?)]
   [empty (-> mapping?)]
   [extend (mapping? identifier? any/c . -> . mapping?)]
   [contains? (mapping? identifier? . -> . boolean?)]
   [lookup (([mapping
              (lambda (mapping)
                (and (mapping? mapping)
                     (contains? mapping id)))]
             [id identifier?])
            . ->r . any)]
   )

  ;; mapping? : Any -> Boolean
  ;; Reports whether a value is a mapping.
  (define (mapping? value)
    (env:env? value))

  ;; empty : -> [Mapping X]
  ;; Contructs an empty mapping.
  (define (empty)
    (env:make-empty-env bound-identifier=?))

  ;; extend : [Mapping X] Identifier X -> [Mapping X]
  ;; Adds or shadows an environment binding.
  (define (extend mapping id entry)
    (env:extend-env (list id) (list entry) mapping))

  ;; contains? : [Mapping X] Identifier -> Boolean
  ;; Reports whether the given key has an entry.
  (define (contains? mapping id)
    (env:bound? mapping id))

  ;; lookup : [Mapping X] Identifier -> X
  ;; Returns the entry for the given key.
  ;; Raises exn:fail:contract if no entry exists.
  (define (lookup mapping id)
    (env:lookup mapping id))

  )
