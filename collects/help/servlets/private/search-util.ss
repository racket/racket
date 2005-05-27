(module search-util mzscheme

  (require (lib "string-constant.ss" "string-constants"))

  (provide 
    search-types 
    search-type-default
    match-types
    match-type-default
    kind-types)

  (define search-types
    `(("keyword" ,(string-constant plt:hd:search-for-keyword))
      ("keyword-index" ,(string-constant plt:hd:search-for-keyword-or-index))
      ("keyword-index-text" ,(string-constant plt:hd:search-for-keyword-or-index-or-text))))

  (define search-type-default "keyword-index")

  (define match-types
    `(("exact-match" ,(string-constant plt:hd:exact-match))
      ("containing-match" ,(string-constant plt:hd:containing-match))
      ("regexp-match" ,(string-constant plt:hd:regexp-match))))

  (define match-type-default "containing-match")

  (define kind-types
    `(("index entries" html)
      ("keyword entries" text)
      ("text" text))))


