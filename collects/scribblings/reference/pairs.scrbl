#lang scribble/doc
@(require "mz.ss"
          scribble/scheme
          (for-syntax scheme/base))

@(define-syntax (defc_r stx)
   (syntax-case stx ()
     [(_ x ...)
      (let ([xs (map syntax-e (syntax->list #'(x ...)))])
        (let ([name (string->symbol
                     (string-append
                      "c"
                      (apply string-append (map symbol->string xs))
                      "r"))]
              [contract (let loop ([l (reverse xs)])
                          (cond
                            [(null? (cdr l)) 'pair?]
                            [(eq? (car l) 'a) `(cons/c ,(loop (cdr l)) any/c)]
                            [(eq? (car l) 'd) `(cons/c any/c ,(loop (cdr l)))]))]
              [equiv (let loop ([l xs])
                       (cond 
                         [(null? l) 'p]
                         [(eq? (car l) 'a) `(car ,(loop (cdr l)))]
                         [(eq? (car l) 'd) `(cdr ,(loop (cdr l)))]))])
          (with-syntax ([name name]
                        [contract (let loop ([c contract] [pos 0])
                                    (if (pair? c)
                                      (let* ([a (loop (car c) (add1 pos))]
                                             [b (loop (cdr c) (+ 1 pos (syntax-span a)))]
                                             [span (+ 1 (syntax-span a) (syntax-span b))])
                                        (datum->syntax #'here
                                                       (cons a b)
                                                       (list (syntax-source stx)
                                                             1
                                                             pos
                                                             (add1 pos)
                                                             span)))
                                      (datum->syntax #'here c 
                                                     (list (syntax-source stx) 1 pos (add1 pos) 1))))]
                        [equiv equiv])
            #'(defproc (name [v contract]) any/c
                "Returns " (to-element 'equiv)))))]))


@title[#:tag "pairs"]{Pairs and Lists}

@guideintro["pairs"]{pairs and lists}

A @deftech{pair} combines exactly two values. The first value is
accessed with the @scheme[car] procedure, and the second value is
accessed with the @scheme[cdr] procedure. Pairs are not mutable (but
see @secref["mpairs"]).

A @deftech{list} is recursively defined: it is either the constant
@scheme[null], or it is a pair whose second value is a list.

A list can be used as a single-valued sequence (see
@secref["sequences"]). The elements of the list serve as elements
of the sequence. See also @scheme[in-list].

Cyclic data structures can be created using only immutable pairs via
@scheme[read] or @scheme[make-reader-graph]. If starting with a pair
and using some number of @scheme[cdr]s returns to the starting pair,
then the pair is not a list.

@; ----------------------------------------
@section{Pair Constructors and Selectors}

@defproc[(pair? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
a pair, @scheme[#f] otherwise.}

@defproc[(null? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
the empty list, @scheme[#f] otherwise.}

@defproc[(cons [a any/c] [d any/c]) pair?]{Returns a pair whose first
element is @scheme[a] and second element is @scheme[d].}

@defproc[(car [p pair?]) any/c]{Returns the first element of the
pair @scheme[p].}

@defproc[(cdr [p pair?]) any/c]{Returns the second element of the
pair @scheme[p].}

@defthing[null null?]{The empty list.}


@defproc[(list? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v]
 is a list: either the empty list, or a pair whose second element is a
 list. This procedure takes amortized constant time.}

@defproc[(list [v any/c] ...) list?]{Returns a newly allocated list
containing the @scheme[v]s as its elements.}

@defproc[(list* [v any/c] ... [tail any/c]) any/c]{

Like @scheme[list], but the last argument is used as the tail of
the result, instead of the final element. The result is a list
only if the last argument is a list.}

@defproc[(build-list [n exact-nonnegative-integer?]
                     [proc (exact-nonnegative-integer? . -> . any)])
         list?]{

Creates a list of @scheme[n] elements by applying @scheme[proc] to the
integers from @scheme[0] to @scheme[(sub1 n)] in order. If
@scheme[_lst] is the resulting list, then @scheme[(list-ref _lst _i)]
is the value produced by @scheme[(proc _i)].

@mz-examples[
(build-list 10 values)
(build-list 5 (lambda (x) (* x x)))
]}

@; ----------------------------------------
@section{List Operations}

@defproc[(length [lst list?])
         exact-nonnegative-integer?]{

Returns the number of elements in @scheme[lst].}


@defproc[(list-ref [lst any/c] [pos exact-nonnegative-integer?])
         any/c]{

Returns the element of @scheme[lst] at position @scheme[pos], where
the list's first element is position @scheme[0]. If the list has
@scheme[pos] or fewer elements, then the
@exnraise[exn:fail:contract].

The @scheme[lst] argument need not actually be a list; @scheme[lst]
must merely start with a chain of at least @scheme[pos] pairs.}


@defproc[(list-tail [lst any/c] [pos exact-nonnegative-integer?])
         any/c]{

Returns the list after the first @scheme[pos] elements of
@scheme[lst]. If the list has fewer than @scheme[pos] elements, then
the @exnraise[exn:fail:contract].

The @scheme[lst] argument need not actually be a list; @scheme[lst]
must merely start with a chain of at least @scheme[pos] pairs.}


@defproc*[([(append [lst list?] ...) list?]
           [(append [lst list?] ... [v any/c]) any/c])]{

When given all list arguments, the result is a list that contains all
of the elements of the given lists in order. The last argument is used
directly in the tail of the result.

The last argument need not be a list, in which case the result is an
``improper list.''}

@defproc[(reverse [lst list?]) list?]{

Returns a list that has the same elements as @scheme[lst], but in
reverse order.}


@; ----------------------------------------
@section{List Iteration}

@defproc[(map [proc procedure?] [lst list?] ...+) 
         list?]{

Applies @scheme[proc] to the elements of the @scheme[lst]s from the
 first elements to the last. The @scheme[proc] argument must accept
 the same number of arguments as the number of supplied @scheme[lst]s,
 and all @scheme[lst]s must have the same number of elements.  The
 result is a list containing each result of @scheme[proc] in order.}

@defproc[(andmap [proc procedure?] [lst list?] ...+)
          any]{

Similar to @scheme[map], except that

@itemize[

 @item{the result is @scheme[#f] if any application of @scheme[proc] produces
       @scheme[#f], in which case @scheme[proc] is not applied to later
       elements of the @scheme[lst]s; or}

 @item{the result is that of @scheme[proc] applied to the last elements
       of the @scheme[lsts]s; more specifically, the application of
       @scheme[proc] to the last elements in the @scheme[lst]s is in tail
       position with respect to the @scheme[andmap] call.}

]

If the @scheme[lst]s are empty, then @scheme[#t] is returned.}

@mz-examples[
(andmap positive? '(1 2 3))
(andmap positive? '(1 2 a))
(andmap positive? '(1 -2 a))
(andmap + '(1 2 3) '(4 5 6))
]


@defproc[(ormap [proc procedure?] [lst list?] ...+)
         any]{

Similar to @scheme[map], except that

@itemize[

 @item{the result is @scheme[#f] if every application of @scheme[proc] produces
       @scheme[#f]; or}

 @item{the result of the first applciation of @scheme[proc] to produces a
       value other than @scheme[#f], in which case @scheme[proc] is not
       applied to later elements of the @scheme[lst]s;
       the application of @scheme[proc] to the last elements in the
       @scheme[lst]s is in tail position with respect to the
       @scheme[ormap] call.}

]

If the @scheme[lst]s are empty, then @scheme[#f] is returned.}

@mz-examples[
(ormap eq? '(a b c) '(a b c))
(ormap positive? '(1 2 a))
(ormap + '(1 2 3) '(4 5 6))
]


@defproc[(for-each [proc procedure?] [lst list?] ...+)
         void?]{

Similar to @scheme[map], but @scheme[proc] is called only for its
 effect, and its result (which can be any number of values) is
 ignored.}


@defproc[(foldl [proc procedure?] [init any/c] [lst list?] ...+)
         any/c]{

Like @scheme[map], @scheme[foldl] applies a procedure to the
 elements of one or more lists. Whereas @scheme[map] combines the return
 values into a list, @scheme[foldl] combines the return values in an
 arbitrary way that is determined by @scheme[proc].  

If @scheme[foldl] is called with @math{n} lists, then @scheme[proc]
 must take @math{n+1} arguments. The extra argument is the combined
 return values so far. The @scheme[proc] is initially invoked with the
 first item of each list, and the final argument is @scheme[init].  In
 subsequent invocations of @scheme[proc], the last argument is the
 return value from the previous invocation of @scheme[proc]. The input
 @scheme[lst]s are traversed from left to right, and the result of the
 whole @scheme[foldl] application is the result of the last
 application of @scheme[proc]. If the @scheme[lst]s are empty, the
 result is @scheme[init].

Unlike @scheme[foldr], @scheme[foldl] processes the @scheme[lst]s in
 constant space (plus the space for each call to @scheme[proc]).

@mz-examples[
(foldl cons '() '(1 2 3 4))
(foldl + 0 '(1 2 3 4))
]}

@defproc[(foldr [proc procedure?] [init any/c] [lst list?] ...+)
         any/c]{

Like @scheme[foldl], but the lists are traversed from right to left.
 Unlike @scheme[foldl], @scheme[foldr] processes the @scheme[lst]s in
 space proportional to the length of @scheme[lst]s (plus the space for
 each call to @scheme[proc]).

@mz-examples[
(foldr cons '() '(1 2 3 4))
(foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))
]}

@; ----------------------------------------
@section{List Filtering}

@defproc[(filter [pred procedure?] [lst list?])
         list?]{

Returns a list with the elements of @scheme[lst] for which
 @scheme[pred] produces a true value. The @scheme[pred] procedure is
 applied to each element from first to last.}


@defproc[(remove [v any/c] [lst list?] [proc procedure? equal?])
         list?]{

Returns a list that is like @scheme[lst], omitting the first element
 of @scheme[lst] that is equal to @scheme[v] using the comparison
 procedure @scheme[proc] (which must accept two arguments).}


@defproc[(remq [v any/c] [lst list?])
         list?]{

Returns @scheme[(remove v lst eq?)].}


@defproc[(remv [v any/c] [lst list?])
         list?]{

Returns @scheme[(remove v lst eqv?)].}


@defproc[(remove* [v-lst list?] [lst list?] [proc procedure? equal?])
         list?]{

Like @scheme[remove], but removes from @scheme[lst] every instance of
every element of @scheme[v-lst].}


@defproc[(remq* [v-lst list?] [lst list?])
         list?]{

Returns @scheme[(remove* v-lst lst eq?)].}


@defproc[(remv* [v-lst list?] [lst list?])
         list?]{

Returns @scheme[(remove* v-lst lst eqv?)].}


@defproc[(sort [lst list?] [less-than? (any/c any/c . -> . any/c)]
               [#:key extract-key (any/c . -> . any/c) (lambda (x) x)]
               [#:cache-keys? cache-keys? boolean? #f])
         list?]{

Returns a list sorted according to the @scheme[less-than?] procedure,
 which takes two elements of @scheme[lst] and returns a true value if
 the first is less than (i.e., should be sorted earlier) than the
 second.

The sort is stable; if two elements of @scheme[lst] are ``equal''
 (i.e., @scheme[proc] does not return a true value when given the pair
 in either order), then the elements preserve their relative order
 from @scheme[lst] in the output list.  To preserve this guarantee,
 use @scheme[sort] with a strict comparison functions (e.g.,
 @scheme[<] or @scheme[string<?]; not @scheme[<=] or
 @scheme[string<=?]).

The @scheme[#:key] argument @scheme[extract-key] is used to extract a
 key value for comparison from each list element.  That is, the full
 comparison procedure is essentially

@schemeblock[
  (lambda (x y)
    (less-than? (extract-key x) (extract-key y)))
]

By default, @scheme[extract-key] is applied to two list elements for
 every comparison, but if @scheme[cache-keys?] is true, then the
 @scheme[extract-key] function is used exactly once for each list
 item. Supply a true value for @scheme[cache-keys?] when
 @scheme[extract-key] is an expensive operation; for example, if
 @scheme[file-or-directory-modify-seconds] is used to extract a
 timestamp for every file in a list, then @scheme[cache-keys?] should
 be @scheme[#t] to minimize file-system calls, but if
 @scheme[extract-key] is @scheme[car], then @scheme[cache-keys?]
 should be @scheme[#f].  As another example, providing
 @scheme[extract-key] as @scheme[(lambda (x) (random))] and
 @scheme[#t] for @scheme[cache-keys?] effectively shuffles the list.}

@mz-examples[
(sort '(1 3 4 2) <)
(sort '("aardvark" "dingo" "cow" "bear") string<?)
(sort '(("aardvark") ("dingo") ("cow") ("bear"))
      #:key car string<?)
]

@; ----------------------------------------
@section{List Searching}

@defproc[(member [v any/c] [lst list?])
         (or/c list? #f)]{

Locates the first element of @scheme[lst] that is @scheme[equal?] to
 @scheme[v]. If such an element exists, the tail of @scheme[lst]
 starting with that element is returned. Otherwise, the result is
 @scheme[#f].}


@defproc[(memv [v any/c] [lst list?])
         (or/c list? #f)]{

Like @scheme[member], but finds an element using @scheme[eqv?].}


@defproc[(memq [v any/c] [lst list?])
         (or/c list? #f)]{

Like @scheme[member], but finds an element using @scheme[eq?].}


@defproc[(memf [proc procedure?] [lst list?])
         (or/c list? #f)]{

Like @scheme[member], but finds an element using the predicate
 @scheme[proc]; an element is found when @scheme[proc] applied to the
 element returns a true value.}


@defproc[(findf [proc procedure?] [lst list?]) any/c]{

Like @scheme[memf], but returns the element or @scheme[#f]
 instead of a tail of @scheme[lst] or @scheme[#f].}


@defproc[(assoc [v any/c] [lst (listof pair?)])
         (or/c pair? #f)]{

Locates the first element of @scheme[lst] whose @scheme[car] is
 @scheme[equal?] to @scheme[v]. If such an element exists, the pair
 (i.e., an element of @scheme[lst]) is returned. Otherwise, the result
 is @scheme[#f].}


@defproc[(assv [v any/c] [lst (listof pair?)])
         (or/c pair? #f)]{

Like @scheme[assoc], but finds an element using @scheme[eqv?].}


@defproc[(assq [v any/c] [lst (listof pair?)])
         (or/c pair? #f)]{

Like @scheme[assoc], but finds an element using @scheme[eq?].}


@defproc[(assf [proc procedure?] [lst list?])
         (or/c list? #f)]{

Like @scheme[assoc], but finds an element using the predicate
 @scheme[proc]; an element is found when @scheme[proc] applied to the
 @scheme[car] of an @scheme[lst] element returns a true value.}



@; ----------------------------------------
@section{Pair Accessor Shorthands}

@defc_r[a a]
@defc_r[a d]
@defc_r[d a]
@defc_r[d d]
@defc_r[a a a]
@defc_r[a a d]
@defc_r[a d a]
@defc_r[a d d]
@defc_r[d a a]
@defc_r[d a d]
@defc_r[d d a]
@defc_r[d d d]
@defc_r[a a a a]
@defc_r[a a a d]
@defc_r[a a d a]
@defc_r[a a d d]
@defc_r[a d a a]
@defc_r[a d a d]
@defc_r[a d d a]
@defc_r[a d d d]
@defc_r[d a a a]
@defc_r[d a a d]
@defc_r[d a d a]
@defc_r[d a d d]
@defc_r[d d a a]
@defc_r[d d a d]
@defc_r[d d d a]
@defc_r[d d d d]

@; ----------------------------------------
@section{Additional List Functions and Synonyms}

@note-lib[scheme/list]
@(define list-eval (make-base-eval))
@(interaction-eval #:eval list-eval
                   (require scheme/list (only-in scheme/function negate)))

@defthing[empty null?]{The empty list.}

@defproc[(cons? [v any/c]) boolean?]{The same as @scheme[(pair? v)].}

@defproc[(empty? [v any/c]) boolean?]{The same as @scheme[(null? v)].}

@defproc[(first [lst list?]) any/c]{The same as @scheme[(car lst)], but only for lists (that are not empty).}

@defproc[(rest [lst list?]) list?]{The same as @scheme[(cdr lst)], but only for lists (that are not empty).}

@defproc[(second [lst list?]) any]{Returns the second element of the list.}

@defproc[(third [lst list?]) any]{Returns the third element of the list.}

@defproc[(fourth [lst list?]) any]{Returns the fourth element of the list.}

@defproc[(fifth [lst list?]) any]{Returns the fifth element of the list.}

@defproc[(sixth [lst list?]) any]{Returns the sixth element of the list.}

@defproc[(seventh [lst list?]) any]{Returns the seventh element of the list.}

@defproc[(eighth [lst list?]) any]{Returns the eighth element of the list.}

@defproc[(ninth [lst list?]) any]{Returns the ninth element of the list.}

@defproc[(tenth [lst list?]) any]{Returns the tenth element of the list.}

@defproc[(last [lst list?]) any]{Returns the last element of the list.}

@defproc[(last-pair [p pair?]) pair?]{
Returns the last pair of a (possibly improper) list.}

@defproc[(make-list [k exact-nonnegative-integer?] [v any?]) list?]{
Returns a newly constructed list of length @scheme[k], holding
@scheme[v] in all positions.

@mz-examples[#:eval list-eval
  (make-list 7 'foo)]}

@defproc[(take [lst any/c] [pos exact-nonnegative-integer?]) list?]{
Returns a fresh list whose elements are the first @scheme[pos] elements of
@scheme[lst].  If @scheme[lst] has fewer than
@scheme[pos] elements, the @exnraise[exn:fail:contract].

The @scheme[lst] argument need not actually be a list; @scheme[lst]
must merely start with a chain of at least @scheme[pos] pairs.

@mz-examples[#:eval list-eval
 (take '(1 2 3 4) 2)
 (take 'non-list 0)
]}

@defproc[(drop [lst any/c] [pos exact-nonnegative-integer?]) any/c]{
Just like @scheme[list-tail].}

@defproc[(split-at [lst any/c] [pos exact-nonnegative-integer?])
         (values list? any/c)]{
Returns the same result as

@schemeblock[(values (take lst pos) (drop lst pos))]

except that it can be faster.}

@defproc[(take-right [lst any/c] [pos exact-nonnegative-integer?]) any/c]{
Returns the @scheme[list]'s @scheme[pos]-length tail. If @scheme[lst]
has fewer than @scheme[pos] elements, then the
@exnraise[exn:fail:contract].

The @scheme[lst] argument need not actually be a list; @scheme[lst]
must merely end with a chain of at least @scheme[pos] pairs.

@mz-examples[#:eval list-eval
 (take-right '(1 2 3 4) 2)
 (take-right 'non-list 0)
]}

@defproc[(drop-right [lst any/c] [pos exact-nonnegative-integer?]) list?]{
Returns a fresh list whose elements are the prefix of @scheme[lst],
dropping its @scheme[pos]-length tail. If @scheme[lst] has fewer than
@scheme[pos] elements, then the @exnraise[exn:fail:contract].

The @scheme[lst] argument need not actually be a list; @scheme[lst]
must merely end with a chain of at least @scheme[pos] pairs.

@mz-examples[#:eval list-eval
 (drop-right '(1 2 3 4) 2)
 (drop-right 'non-list 0)
]}

@defproc[(split-at-right [lst any/c] [pos exact-nonnegative-integer?])
         (values list? any/c)]{
Returns the same result as

@schemeblock[(values (drop-right lst pos) (take-right lst pos))]

except that it can be faster.}

@defproc[(add-between [lst list?] [v any/c]) list?]{

Returns a list with the same elements as @scheme[lst], but with
@scheme[v] between each pair of items in @scheme[lst].

@mz-examples[#:eval list-eval
  (add-between '(x y z) 'or)
  (add-between '(x) 'or)
]}

@defproc*[([(append* [lst list?] ... [lsts (listof list?)]) list?]
           [(append* [lst list?] ... [lsts list?]) any/c])]{
@; Note: this is exactly the same description as the one for string-append*

Like @scheme[append], but the last argument is used as a list of
arguments for @scheme[append], so @scheme[(append* lst ... lsts)] is the
same as @scheme[(apply append lst ... lsts)].  In other words, the
relationship between @scheme[append] and @scheme[append*] is similar
to the one between @scheme[list] and @scheme[list*].

@mz-examples[#:eval list-eval
  (append* '(a) '(b) '((c) (d)))
  (cdr (append* (map (lambda (x) (list ", " x))
                     '("Alpha" "Beta" "Gamma"))))
]}

@defproc[(flatten [v any/c])
         list?]{

Flattens an arbitrary S-expression structure of pairs into a
list. More precisely, @scheme[v] is treated as a binary tree where
pairs are interior nodes, and the resulting list contains all of the
non-@scheme[null] leaves of the tree in the same order as an inorder
traversal.

@mz-examples[#:eval list-eval
  (flatten '((a) b (c (d) . e) ()))
  (flatten 'a)
]}

@defproc[(remove-duplicates [lst list?]
                            [same? (any/c any/c . -> . any/c) equal?]
                            [#:key extract-key (any/c . -> . any/c)
                                   (lambda (x) x)])
         list?]{

Returns a list that has all items in @scheme[lst], but without
duplicate items, where @scheme[same?] determines whether two elements
of the list are equivalent.  The resulting list is in the same order
as @scheme[lst], and for any item that occurs multiple times, the
first one is kept.

The @scheme[#:key] argument @scheme[extract-key] is used to extract a
 key value from each list element, so two items are considered equal if
 @scheme[(same? (extract-key x) (extract-key y))] is true.

@mz-examples[#:eval list-eval
  (remove-duplicates '(a b b a))
  (remove-duplicates '(1 2 1.0 0))
  (remove-duplicates '(1 2 1.0 0) =)
]}

@defproc[(filter-map [proc procedure?] [lst list?] ...+)
         list?]{

Returns @scheme[(filter (lambda (x) x) (map proc lst ...))], but
without building the intermediate list.}


@defproc[(count [proc procedure?] [lst list?] ...+)
         list?]{

Returns @scheme[(length (filter proc lst ...))], but
without building the intermediate list.}


@defproc[(partition [pred procedure?] [lst list?])
         (values list? list?)]{

Similar to @scheme[filter], except that two values are returned: the
items for which @scheme[pred] returns a true value, and the items for
which @scheme[pred] returns @scheme[#f].

The result is the same as

@schemeblock[(values (filter pred lst) (filter (negate pred) lst))]

but @scheme[pred] is applied to each item in @scheme[lst] only once.

@mz-examples[#:eval list-eval
  (partition even? '(1 2 3 4 5 6))
]}


@defproc[(append-map [proc procedure?] [lst list?] ...+)
         list?]{

Returns @scheme[(append* (map proc lst ...))].

@mz-examples[#:eval list-eval
 (append-map vector->list '(#(1) #(2 3) #(4)))
]}


@defproc[(filter-not [pred (any/c . -> . any/c)] [lst list?])
         list?]{

Like @scheme[filter], but the meaning of the @scheme[pred] predicate
is reversed: the result is a list of all items for which @scheme[pred]
returns @scheme[#f].

@mz-examples[#:eval list-eval
  (filter-not even? '(1 2 3 4 5 6))
]}

@defproc[(argmin [proc (-> any/c real?)] [lst (and/c pair? list?)]) any/c]{

This returns the first element in the list @scheme[lst] that minimizes
the result of @scheme[proc]. 

@mz-examples[#:eval list-eval
(argmin car '((3 pears) (1 banana) (2 apples)))
(argmin car '((1 banana) (1 orange)))
]
}

@defproc[(argmax [proc (-> any/c real?)] [lst (and/c pair? list?)]) any/c]{

This returns the first element in the list @scheme[lst] that maximizes
the result of @scheme[proc]. 

@mz-examples[#:eval list-eval
(argmax car '((3 pears) (1 banana) (2 apples)))
(argmax car '((3 pears) (3 oranges)))
]
}

@close-eval[list-eval]

@; ----------------------------------------
@section{Immutable Cyclic Data}

@defproc[(make-reader-graph [v any/c]) any/c]{

Returns a value like @scheme[v], with placeholders created by
@scheme[make-placeholder] replaced with the values that they contain,
and with placeholders created by @scheme[make-hash-placeholder]
with an immutable hash table. No part of @scheme[v] is mutated;
instead, parts of @scheme[v] are copied as necessary to construct
the resulting graph, where at most one copy is created for any given
value.

Since the copied vales can be immutable, and since the copy is also
immutable, @scheme[make-reader-graph] can create cycles involving only
immutable pairs, vectors, boxes, and hash tables.

Only the following kinds of values are copied and traversed to detect
placeholders:

@itemize[

 @item{pairs}

 @item{vectors, both mutable and immutable}

 @item{boxes, both mutable and immutable}

 @item{hash tables, both mutable and immutable}

 @item{instances of a @techlink{prefab} structure type}

 @item{placeholders created by @scheme[make-placeholder] and
       @scheme[make-hash-placeholder]}

]

Due to these restrictions, @scheme[make-reader-graph] creates exactly
the same sort of cyclic values as @scheme[read].

@mz-examples[
(let* ([ph (make-placeholder #f)]
       [x (cons 1 ph)])
  (placeholder-set! ph x)
  (make-reader-graph x))
]}

@defproc[(placeholder? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a placeholder created by
@scheme[make-placeholder], @scheme[#f] otherwise.}


@defproc[(make-placeholder [v any/c]) placeholder?]{

Returns a placeholder for use with @scheme[placeholder-set!]  and
@scheme[make-reader-graph]. The @scheme[v] argument supplies the
initial value for the placeholder.}

@defproc[(placeholder-set! [ph placeholder?] [datum any/c]) void?]{

Changes the value of @scheme[ph] to @scheme[v].}

@defproc[(placeholder-get [ph placeholder?]) any/c]{

Returns the value of @scheme[ph].}


@defproc[(hash-placeholder? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a placeholder created by
@scheme[make-hash-placeholder], @scheme[#f] otherwise.}


@defproc[(make-hash-placeholder [assocs (listof pair?)])
         hash-placeholder?]{

Like @scheme[make-immutable-hash], but produces a table placeholder
for use with @scheme[make-reader-graph].}

@defproc[(make-hasheq-placeholder [assocs (listof pair?)])
         hash-placeholder?]{

Like @scheme[make-immutable-hasheq], but produces a table placeholder
for use with @scheme[make-reader-graph].}
