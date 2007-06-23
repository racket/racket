#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "hash-tables"]{Hash Tables}

A @defterm{hash table} implements a maping from keys to values, where
both keys can values can be arbitrary Scheme values, and access and
update to the tabel are normally constant-time operations. Keys are
compared using @scheme[equal?] or @scheme[eq?], depending on whether
the hash table is created with the @scheme['equal] flag.

@examples[
(define ht (make-hash-table 'equal))
(hash-table-put! ht "apple" '(red round))
(hash-table-put! ht "banana" '(yellow long))
(hash-table-get ht "apple")
(hash-table-get ht "coconut")
(hash-table-get ht "coconut" "not there")
]

A literal hash table can be written as an expression by using
@litchar{#hash} (for an @scheme[equal?]-based table) or
@litchar{#hasheq} (for an @scheme[eq?]-based table). A parenthesized
sequence must immediately follow @litchar{#hash} or
@litchar{#hasheq}, where each element is a sequence is a dotted
key--value pair. Literal hash tables are immutable.

@examples[
(define ht #hash(("apple" . (red round))
                 ("banana" . (yellow long))))
(hash-table-get ht "apple")
]

@refdetails/gory["mz:parse-hashtable"]{the syntax of hash table literals}

A hash table can optionally retain its keys @defterm{weakly}, so each
mapping is retained only so long as the key is retained elsewhere.

@examples[
(define ht (make-hash-table 'weak))
(hash-table-put! ht (gensym) "can you see me?")
(collect-garbage)
(eval:alts (hash-table-count ht) 0)
]

Beware that even a weak hash table retains its values strongly, as
long as the corresponding key is accessible. This creates a catch-22
dependency when a value refers back to its key, so that the mapping is
retained permanently. To break the cycle, map the key to an
@seclink["guide:ephemerons"]{ephemeron} that pairs the value with its key (in
addition to the implicit pairing of the hash table).

@examples[
(define ht (make-hash-table 'weak))
(let ([g (gensym)])
  (hash-table-put! ht g (list g)))
(collect-garbage)
(eval:alts (hash-table-count ht) 1)
]

@interaction[
(define ht (make-hash-table 'weak))
(let ([g (gensym)])
  (hash-table-put! ht g (make-ephemeron g (list g))))
(collect-garbage)
(eval:alts (hash-table-count ht) 0)
]

@refdetails["mz:hashtables"]{hash tables and hash-table procedures}
