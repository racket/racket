(module runtime-path-table '#%kernel
  (#%declare #:unsafe-cross-phase-persistent)
  (#%provide table)
  (define-values (table) #f)
  ;; So table definition is not inlined across modules:
  (set! table #f))
