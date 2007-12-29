#lang scribble/doc
@require["mz.ss"]
@require[scribble/scheme]
@require[(for-syntax scheme/base)]

@define-syntax[(defc_r stx)
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
                       [contract (let loop ([c contract][pos 0])
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
               "Returns " (to-element 'equiv)))))])]

@title[#:tag "pairs"]{Pairs and Lists}

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
the result, insteda of the final element. The result is a list
only if the last argument is a list.}

@defproc[(build-list [n exact-nonnegative-integer?]
                     [proc (exact-nonnegative-integer? . -> . any)])
         list?]{

Creates a list of @scheme[n] elements by applying @scheme[proc] to the
integers from @scheme[0] to @scheme[(sub1 n)] in order. If
@scheme[_lst] is the resulting list, then @scheme[(list-ref _lst _i)]
is the value produced by @scheme[(proc _i)].

@examples[
(build-list 10 values)
(build-list 5 (lambda (x) (* x x)))
]}

@; ----------------------------------------
@section{List Operations}

@defproc[(length [lst list?])
         nonnegative-exact-integer?]{

Returns the number of elements in @scheme[lst].}


@defproc[(list-ref [lst list?][pos nonnegative-exact-integer?])
         any/c]{

Returns the element of @scheme[lst] at position @scheme[pos], where
the list's first element is position @scheme[0]. If the list has
@scheme[pos] or fewer elements, then the
@exnraise[exn:fail:contract].}


@defproc[(list-tail [lst list?][pos nonnegative-exact-integer?])
         any/c]{

Returns the list after the first @scheme[pos] elements of
@scheme[lst]. If the list has @scheme[pos] or fewer elements, then the
@exnraise[exn:fail:contract].}


@defproc*[([(append [lst list?] ...) list?]
           [(append [lst list?] ... [v any/c]) any/c])]{

When given all list arguments, the result is a lists that contains all
of the elements of the given lists in order. The last argument is used
directly in the tail of the result.

The last argument need not be a list, in which case the result is an
``improper list'' ...}

@defproc[(reverse [lst list?]) list?]{

Returns a list that has the same elements as @scheme[lst], but in
reverse order.}


@; ----------------------------------------
@section{List Iteration}

@defproc[(map [proc procedure?] [lst list?] ...+) 
         list?]{

Applies @scheme[proc] to the elements of the @scheme[lst]s from the
 first elements to the last, returning @scheme[#f] as soon as any
 application returns @scheme[#f]. The @scheme[proc] argument must
 accept the same number of arguments as the number of supplied
 @scheme[lst]s, and all @scheme[lst]s must have the same number of
 elements.  The result is a list containing each result of
 @scheme[proc].}


@defproc[(andmap [proc procedure?] [lst list?] ...+)
          any]{

Similar to @scheme[map], except that

@itemize{

 @item{the result is @scheme[#f] if any application of @scheme[proc] produces
       @scheme[#f], in which case @scheme[proc] is not applied to later
       elements of the @scheme[lst]s; or}

 @item{the result is that of @scheme[proc] applied to the last elements
       of the @scheme[lsts]s; more specifically, the application of
       @scheme[proc] to the last elements in the @scheme[lst]s is in tail
       position with respect to the @scheme[andmap] call.}

}

If the @scheme[lst]s are empty, then @scheme[#t] is returned.}

@examples[
(andmap positive? '(1 2 3))
(andmap positive? '(1 2 a))
(andmap positive? '(1 -2 a))
(andmap + '(1 2 3) '(4 5 6))
]


@defproc[(ormap [proc procedure?] [lst list?] ...+)
         any]{

Similar to @scheme[map], except that

@itemize{

 @item{the result is @scheme[#f] if every application of @scheme[proc] produces
       @scheme[#f]; or}

 @item{the result of the first applciation of @scheme[proc] to produces a
       value other than @scheme[#f], in which case @scheme[proc] is not
       applied to later elements of the @scheme[lst]s; more specifically,
       the application of @scheme[proc] to the last elements in the
       @scheme[lst]s is in tail position with respect to the
       @scheme[andmap] call.}

}

If the @scheme[lst]s are empty, then @scheme[#f] is returned.}

@examples[
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
         list?]{

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

@examples[
(foldl cons '() '(1 2 3 4))
(foldl + 0 '(1 2 3 4))
]}

@defproc[(foldr [proc procedure?] [init any/c] [lst list?] ...+)
         list?]{

Like @scheme[foldl], but the lists are traversed from right to left.
 Unlike @scheme[foldl], @scheme[foldr] processes the @scheme[lst]s in
 space proportional to the length of @scheme[lst]s (plus the space for
 each call to @scheme[proc]).

@examples[
(foldr cons '() '(1 2 3 4))
(foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))
]}

@; ----------------------------------------
@section{List Filtering}

@defproc[(filter [proc procedure?] [lst list?])
         list?]{

Returns a list with the elements of @scheme[lst] for which
 @scheme[proc] produces a true value. The predicate @scheme[proc]
 is applied to each element from first to last.}


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


@defproc[(remq* [v any/c] [lst list?])
         list?]{

Returns @scheme[(remove* v lst eq?)].}


@defproc[(remv* [v any/c] [lst list?])
         list?]{

Returns @scheme[(remove* v lst eqv?)].}


@defproc[(sort [lst list?] [less-than procedure?])
         list?]{

Returns a list sorted according to the @scheme[less-than] procedure,
 which takes two elements of @scheme[lst] and returns a true value if
 the first is less than (i.e., should be sorted earlier) than the
 second.

The sort is stable: if two elements of @scheme[lst] are ``equal''
 (i.e., @scheme[proc] does not return a true value when given the pair
 in either order), then the elements preserve their relative order
 from @scheme[lst] in the output list.}

@; ----------------------------------------
@section{List Searching}

@defproc[(member [v any/c] [lst list?])
         (or/c list? false/c)]{

Locates the first element of @scheme[lst] that is @scheme[equal?] to
 @scheme[v]. If such an element exists, the tail of @scheme[lst]
 starting with that element is returned. Otherwise, the result is
 @scheme[#f].}


@defproc[(memv [v any/c] [lst list?])
         (or/c list? false/c)]{

Like @scheme[member], but finds an element using @scheme[eqv?].}


@defproc[(memq [v any/c] [lst list?])
         (or/c list? false/c)]{

Like @scheme[member], but finds an element using @scheme[eq?].}


@defproc[(memf [proc procedure?] [lst list?])
         (or/c list? false/c)]{

Like @scheme[member], but finds an element using the predicate
 @scheme[proc]; an element is found when @scheme[proc] applied to the
 element returns a true value.}


@defproc[(findf [proc procedure?] [lst list?]) any/c]{

Like @scheme[memf], but returns the element or @scheme[#f]
 instead of a tail of @scheme[lst] or @scheme[#f].}


@defproc[(assoc [v any/c] [lst (listof pair?)])
         (or/c pair? false/c)]{

Locates the first element of @scheme[lst] whose @scheme[car] is
 @scheme[equal?] to @scheme[v]. If such an element exists, the pair
 (i.e., an element of @scheme[lst]) is returned. Otherwise, the result
 is @scheme[#f].}


@defproc[(assv [v any/c] [lst (listof pair?)])
         (or/c pair? false/c)]{

Like @scheme[assoc], but finds an element using @scheme[eqv?].}


@defproc[(assq [v any/c] [lst (listof pair?)])
         (or/c pair? false/c)]{

Like @scheme[assoc], but finds an element using @scheme[eq?].}


@defproc[(assf [proc procedure?] [lst list?])
         (or/c list? false/c)]{

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
@section{List Synonyms}

@note-lib[scheme/list]

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

@defproc[(last [lst list?]) any]{Returns the last element of the list.}

@; ----------------------------------------
@section{Immutable Cyclic Data}

@defproc[(make-reader-graph [v any/c]) any/c]{

Returns a value like @scheme[v], with placeholders created by
@scheme[make-placeholder] replaced with the values that they contain,
and with placeholders created by @scheme[make-hash-table-placeholder]
with an immutable hash table. No part of @scheme[v] is mutated;
instead, parts of @scheme[v] are copied as necessary to construct
the resulting graph, where at most one copy is created for any given
value.

Since the copied vales can be immutable, and since the copy is also
immutable, @scheme[make-reader-graph] can cycles involving only
immutable pairs, vectors, boxes, and hash tables. 

Only the following kinds of values are copied and traversed to detect
placeholders:

@itemize{

 @item{pairs}

 @item{immutable pairs (as created by @scheme[mcons])}

 @item{vectors, both mutable and immutable}

 @item{boxes, both mutable and immutable}

 @item{hash tables, both mutable and immutable}

 @item{placeholders created by @scheme[make-placeholder] and
       @scheme[make-hash-table-placeholder]}

}

Due to these restrictions, @scheme[make-reader-graph] creates exactly
the same sort of cyclic values as @scheme[read].

@examples[
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


@defproc[(hash-table-placeholder? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a placeholder created by
@scheme[make-hash-table-placeholder], @scheme[#f] otherwise.}


@defproc[(make-hash-table-placeholder [assocs (listof pair?)]
                                      [flag (one-of/c 'equal)]
                                      ...)
         hash-table-placeholder?]{

Like @scheme[make-immutable-hash-table], but produces a table
placeholder for use with @scheme[make-reader-graph].}
