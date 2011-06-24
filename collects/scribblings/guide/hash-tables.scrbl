#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "hash-tables"]{Hash Tables}

A @deftech{hash table} implements a mapping from keys to values, where
both keys and values can be arbitrary Scheme values, and access and
update to the table are normally constant-time operations. Keys are
compared using @racket[equal?], @racket[eqv?], or @racket[eq?], depending on whether
the hash table is created with @racket[make-hash],
@racket[make-hasheqv], or @racket[make-hasheq].

@examples[
(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(hash-ref ht "apple")
(hash-ref ht "coconut")
(hash-ref ht "coconut" "not there")
]

The @racket[hash], @racket[hasheqv], and @racket[hasheq] functions
create immutable hash tables from an initial set of keys and values,
which each value is provided as an argument after its key. Immutable
hash tables can be extended with @racket[hash-set], which produces a
new immutable hash table in constant time.

@examples[
(define ht (hash "apple" 'red "banana" 'yellow))
(hash-ref ht "apple")
(define ht2 (hash-set ht "coconut" 'brown))
(hash-ref ht "coconut")
(hash-ref ht2 "coconut")
]

A literal immutable hash table can be written as an expression by using
@litchar{#hash} (for an @racket[equal?]-based table),
@litchar{#hasheqv} (for an @racket[eqv?]-based table), or
@litchar{#hasheq} (for an @racket[eq?]-based table). A parenthesized
sequence must immediately follow @litchar{#hash}, @litchar{#hasheq},
or @litchar{#hasheqv}, where each element is a dotted
key--value pair. The @litchar{#hash}, etc. forms implicitly
@racket[quote] their key and value sub-forms.

@examples[
(define ht #hash(("apple" . red)
                 ("banana" . yellow)))
(hash-ref ht "apple")
]

@refdetails/gory["parse-hashtable"]{the syntax of hash table literals}

Both mutable and immutable hash tables print like immutable hash
tables, using a quoted @litchar{#hash}, @litchar{#hasheqv}, or
@litchar{#hasheq} form if all keys and values can be expressed with
@racket[quote] or using @racketresult[hash], @racketresult[hasheq], or
@racketresult[hasheqv] otherwise:

@examples[
#hash(("apple" . red)
      ("banana" . yellow))
(hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
]

A mutable hash table can optionally retain its keys
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
