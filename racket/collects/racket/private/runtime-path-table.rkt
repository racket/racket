(module runtime-path-table '#%kernel
  (#%provide table)
  (define-values (table) #f)
  ;; So table definition is not inlined across modules:
  (set! table #f))
