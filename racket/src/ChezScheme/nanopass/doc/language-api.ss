(define-language Lannotated
  (entry Defn)
  (terminals
    (record-constructor-descriptor (rcd))
    (record-type-descriptor (rtd))
    (exact-integer (tag level tag-mask))
    (datum (handler record-name pred all-pred all-term-pred
             accessor maker))
    (box (b))
    (syntax (stx))
    (identifier (id))
    (dots (dots))
    (null (null)))
  (Defn (def)
    (define-language id ref (maybe id0) rtd rcd tag-mask
      (term* ...) nt* ...)) 
  (Terminal (term)
    (id (id* ...) b (maybe handler) pred))
  (Nonterminal (nt)
    (id (id* ...) b rtd rcd tag pred all-pred all-term-pred
        prod* ...))
  (Production (prod)
    (production pattern (maybe pretty-prod) rtd tag pred maker field* ...)
    (terminal ref (maybe pretty-prod))
    (nonterminal ref (maybe pretty-prod)))
  (Pattern (pattern)
    id
    ref
    null
    (maybe ref)
    (pattern dots)
    (pattern0 dots pattern1 ... . pattern2)
    (pattern0 . pattern1))
  (PrettyProduction (pretty-prod)
    (procedure handler)
    (pretty pattern))
  (Field (field)
    (ref level accessor)
    (optional ref level accessor))
  (Reference (ref)
    (reference id0 id1 b)))

(define-language Llanguage
  (entry Defn)
  (terminals
    (box (b))
    (syntax (stx))
    (identifier (id))
    (datum (handler))
    (dots (dots))
    (null (null)))
  (Defn (def)
    (define-language id cl* ...))
  (Clause (cl)
    (entry ref)
    (nongenerative-id id)
    (terminals term* ...)
    (id (id* ...) b prod* ...))
  (Terminal (term)
    simple-term
    (=> simple-term handler))
  (SimpleTerminal (simple-term)
    (id (id* ...) b))
  (Production (prod)
    pattern
    (=> pattern0 pattern1)
    (-> pattern handler))
  (Pattern (pattern)
    id
    ref
    null
    (maybe ref)
    (pattern dots)
    (pattern0 dots pattern1 ... . pattern2)
    (pattern0 . pattern1))
  (Reference (ref)
    (reference id0 id1 b)))
