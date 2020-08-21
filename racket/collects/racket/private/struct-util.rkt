(module struct-util '#%kernel
  (#%require "define.rkt"
             "cond.rkt")

  (#%provide predicate->struct-name)

  ;; predicate->struct-name : any/c syntax? (or/c identifier? #f) -> string?
  ;; Infers struct name from a predicate identifier. This is used as a fallback
  ;; method to extract field names when struct-field-info is not available.
  (define (predicate->struct-name who orig-stx stx)
    (if stx
        (cond
          [(regexp-match #rx"^(.*)[?]$" (symbol->string (syntax-e stx))) => cadr]
          [else
           (raise-syntax-error
            who
            "unable to cope with a struct type whose predicate doesn't end with `?'"
            orig-stx)])
        (raise-syntax-error
         who
         "unable to cope with a struct whose predicate is unknown"
         orig-stx))))
