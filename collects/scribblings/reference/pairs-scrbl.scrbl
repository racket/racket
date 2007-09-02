#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]
@require[(lib "scheme.ss" "scribble")]
@require-for-syntax[mzscheme]

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
                                         (datum->syntax-object #'here
                                                               (cons a b)
                                                               (list (syntax-source stx)
                                                                     1
                                                                     pos
                                                                     (add1 pos)
                                                                     span)))
                                       (datum->syntax-object #'here c 
                                                             (list (syntax-source stx) 1 pos (add1 pos) 1))))]
                       [equiv equiv])
           #'(defproc (name [v contract]) any/c
               "Returns " (to-element 'equiv)))))])]

@title[#:tag "pairs"]{Pairs and Lists}

A list can be used as a single-valued sequence (see
@secref["sequences"]). The elements of the list serve as elements
of the sequence. See also @scheme[in-list].

@; ----------------------------------------
@section{Pair Constructors, Selectors, and Mutators}

@defproc[(pair? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
a pair, @scheme[#f] otherwise.}

@defproc[(cons? [v any/c]) boolean?]{The same as @scheme[(pair? v)].}

@defproc[(null? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
the empty list, @scheme[#f] otherwise.}

@defproc[(empty? [v any/c]) boolean?]{The same as @scheme[(null? v)].}

@defproc[(cons [a any/c] [d any/c]) pair?]{Returns a pair whose first
element is @scheme[a] and second element is @scheme[d].}

@defproc[(car [p pair?]) any/c]{Returns the first element of the
pair @scheme[p].}

@defproc[(cdr [p pair?]) any/c]{Returns the second element of the
pair @scheme[p].}

@defproc[(first [p pair?]) any/c]{The same as @scheme[(car p)].}

@defproc[(rest [p pair?]) any/c]{The same as @scheme[(cdr p)].}

@defthing[null null?]{The empty list.}

@defthing[empty null?]{The empty list.}


@defproc[(list? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v]
 is a list: either the empty list, or a pair whose second element is a
 list.}

@defproc[(list [v any/c] ...) list?]{Returns a newly allocated list
containing the @scheme[v]s as its elements.}

@defproc[(list* [v any/c] ... [tail any/c]) any/c]{

Like @scheme[list], but the last argument is used as the tail of
the result, insteda of the final element. The result is a list
only if the last argument is a list.}


@defproc[(set-car! [p mutable-pair?] [v any/v]) 
         void?]{

Changes the mutable pair @scheme[p] so that its first element is
@scheme[v].}


@defproc[(set-cdr! [p mutable-pair?] [v any/v]) 
         void?]{

Changes the mutable pair @scheme[p] so that its second element is
@scheme[v].}


@defproc[(immutable? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a mutable pair, string, byte
string, vector, or box, @scheme[#f] otherwise.}

@; ----------------------------------------
@section{List Operations}

@defproc[(length [lst list?])
         nonnegative-exact-integer?]{

Returns the number of elements in @scheme[lst].}


@defproc[(list-ref [lst list?][pos nonnegative-exact-integer?])
         any/c]{

Returns the element of @scheme[vec] at position @scheme[pos], where
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

