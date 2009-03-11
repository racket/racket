#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "hash-tables"]{Hash Tables}

A @deftech{hash table} implements a mapping from keys to values, where
both keys and values can be arbitrary Scheme values, and access and
update to the table are normally constant-time operations. Keys are
compared using @scheme[equal?] or @scheme[eq?], depending on whether
the hash table is created with @scheme[make-hash] or
@scheme[make-hasheq].

@examples[
(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(hash-ref ht "apple")
(hash-ref ht "coconut")
(hash-ref ht "coconut" "not there")
]

A literal hash table can be written as an expression by using
@litchar{#hash} (for an @scheme[equal?]-based table),
@litchar{#hasheq} (for an @scheme[eq?]-based table), or
@litchar{#hasheqv} (for an @scheme[eqv?]-based table). A parenthesized
sequence must immediately follow @litchar{#hash}, @litchar{#hasheq},
or @litchar{#hasheqv}, where each element is a sequence is a dotted
key--value pair. Literal hash tables are immutable, but they can be
extended functionally (producing a new hash table without changing the
old one) using @scheme[hash-set].

@examples[
(define ht #hash(("apple" . red)
                 ("banana" . yellow)))
(hash-ref ht "apple")
(define ht2 (hash-set ht "coconut" 'brown))
(hash-ref ht "coconut")
(hash-ref ht2 "coconut")
ht2
]

@refdetails/gory["parse-hashtable"]{the syntax of hash table literals}

A non-literal hash table can optionally retain its keys
@defterm{weakly}, so each mapping is retained only so long as the key
is retained elsewhere.

@examples[
(define ht (make-weak-hasheq))
(hash-set! ht (gensym) "can you see me?")
(collect-garbage)
(eval:alts (hash-count ht) 0)
]

Beware that even a weak hash table retains its values strongly, as
long as the corresponding key is accessible. This creates a catch-22
dependency when a value refers back to its key, so that the mapping is
retained permanently. To break the cycle, map the key to an ephemeron
that pairs the value with its key (in addition to the implicit pairing
of the hash table).

@examples[
(define ht (make-weak-hasheq))
(let ([g (gensym)])
  (hash-set! ht g (list g)))
(collect-garbage)
(eval:alts (hash-count ht) 1)
]

@interaction[
(define ht (make-weak-hasheq))
(let ([g (gensym)])
  (hash-set! ht g (make-ephemeron g (list g))))
(collect-garbage)
(eval:alts (hash-count ht) 0)
]

@refdetails["hashtables"]{hash tables and hash-table procedures}
