(module runtime-path-table mzscheme
  (provide table)
  (define table #f)
  ;; So table definition is not inlined across modules:
  (set! table #f))
