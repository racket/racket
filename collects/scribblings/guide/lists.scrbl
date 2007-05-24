#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require[(lib "bnf.ss" "scribble")]
@require["guide-utils.ss"]

@interaction-eval[(require (lib "list.ss"))]
@interaction-eval[(require (lib "for.ss"))]
@define[step @elem{=}]

@title{Lists, Iteration, and Recursion}

Scheme is a dialect of the language Lisp, whose name originally stood
for ``LISt Processor.'' The built-in list datatype remains a prominent
feature of the language.

The @scheme[list] procedure takes any number of values and returns
a list containing the values:

@interaction[(list "red" "green" "blue")
             (list 1 2 3 4 5)]

As you can see, a list result prints in the REPL as a pair of
parentheses wrapped around the printed form of the list
elements. There's an opportunity for confusion here, because
parentheses are used for both expressions, such as @scheme[(list "red"
"green" "blue")], and printed results, such as @schemeresult[("red"
"green" "blue")]. Rememeber that, in the documentation and in
DrScheme, parentheses for results are printed in blue, whereas
parentheses for expressions are brown.

Many predefined procedures operate on lists. Here are a few examples:

@interaction[
(code:line (length (list "a" "b" "c"))        (code:comment #, @t{count the elements}))
(code:line (list-ref (list "a" "b" "c") 0)    (code:comment #, @t{extract an element by position}))
(list-ref (list "a" "b" "c") 1)
(code:line (append (list "a" "b") (list "c")) (code:comment #, @t{combine lists}))
(code:line (reverse (list "a" "b" "c"))       (code:comment #, @t{reverse order}))
(code:line (member "d" (list "a" "b" "c"))    (code:comment #, @t{check for an element}))
]

@;------------------------------------------------------------------------
@section{Predefined List Loops}

In addition to simple operations like @scheme[append], Scheme includes
procedures that iterate over the elements of a list. These iteration
procedures play much the same role as @tt{for} in Java and other
languages. The body of a Scheme iteration is packaged into a procedure
to be applied to each element, so the @scheme[lambda] form becomes
particularly handy in combination with iteration procedures.

The @scheme[for-each] procedure acts the most like a @tt{for} loop:

@interaction[
(for-each (lambda (elem) 
            (printf "I have ~a\n" elem))
          (list "pie" "stew" "carrots and pizza, and pineapple, too"))
]

The @scheme[for-each] procedure completely ignores the per-element
result of the iteration body, so it is used with loop bodies that have
a side-effect (such as printing output). Keeping in mind that Scheme
programmers avoid side-effects, they also avoid @scheme[for-each].

Other list-iteration procedures use the per-element results, but in
different ways. The @scheme[map] procedure uses the per-element
results to create a new list:

@interaction[
(map sqrt (list 1 4 9 16))
(map (lambda (i)
       (string-append i "!"))
     (list "peanuts" "popcorn" "crackerjack"))
]

The @scheme[andmap] and @scheme[ormap] procedures combine the results
by @scheme[and]ing or @scheme[or]ing:

@interaction[
(andmap string? (list "a" "b" "c"))
(andmap string? (list "a" "b" 6))
(ormap number? (list "a" "b" 6))
]

The @scheme[filter] procedure keeps elements for which the body result
is true, and discards elements for which it is @scheme[#f]:

@interaction[
(filter string? (list "a" "b" 6))
(filter positive? (list 1 -2 6 7 0))
]

The @scheme[for-each], @scheme[map], @scheme[andmap], @scheme[ormap],
and @scheme[filter] procedures can all handle multiple lists, instead
of just a single list. The lists must all have the same length, and
the given procedure must accept one argument for each list:

@interaction[
(map (lambda (s n) (substring s 0 n))
     (list "peanuts" "popcorn" "crackerjack")
     (list 6 3 7))
]

The @scheme[foldl] procedure generalizes some iteration procedures. It
uses the per-element procedure to both process an element and combine
it with the ``current'' value, so the per-element procedure takes an
extra first argument. Also, a starting ``current'' value must be
provided before the lists:

@interaction[
(foldl (lambda (v elem)
         (+ v (* elem elem)))
       0
       '(1 2 3))
]

Despite its generality, @scheme[foldl] is not as popular as the other
procedures. One reason is that @scheme[map], @scheme[ormap],
@scheme[andmap], and @scheme[filter] cover the most common kinds of
list loops.

@;------------------------------------------------------------------------
@section{Iterative Folds and Comprehensions: @scheme[fold-for] and @scheme[list-for]}

Besides iteration procedures like @scheme[foldl], Scheme provides a
syntactic form for iteration that more closely resembles the syntax of
other languages. The @scheme[foldl] example above can be written with
the @scheme[fold-for] syntax as follows:

@interaction[
(fold-for ([sum 0])
          ([elem (list 1 2 3)])
  (+ sum elem))
]

Compare to analogous Java code, where @scheme[(list 1 2 3)] is
replaced by a collection @scheme[lst]:

@verbatim[
#<<EOS
  int sum = 0;
  for (Object elem : lst) {
    sum = sum + elem;
  }
  return sum;
EOS
]

The only significant difference is that the updating of @scheme[sum]
and the return of @scheme[sum]'s value are implicit. Those implicit
actions are why the form is called @scheme[fold-for] instead of just
@scheme[for].

Along similar lines, the @scheme[list-for] form iterates through a list
and implicitly accumulates each result into a list:

@interaction[
(list-for ([i (list "peanuts" "popcorn" "crackerjack")])
  (string-append i "!"))
]

The @scheme[list-for] form is a @defterm{list compherension} form, as
in Haskell, Ruby, Python, and other languages. One advantage over
@scheme[map] is that it can iterate over more things than just lists.
For example, @scheme[list-for] can iterate over a range of numbers:

@interaction[
(list-for ([i (range 0 10)])
  i)
]

The @scheme[list-for] form can even iterate over a list and a range of
numbers in parallel:

@interaction[
(list-for ([s (list "a" "b" "c")]
           [n (range 0 3)])
  (if (= n 2)
      "oops!"
       s))
]

Note that the binding syntax of @scheme[fold-for] and
@scheme[list-for] is similar to that of @scheme[let] (as introduced in
@secref["local-binding-intro"]). In the same way that @scheme[let*]
supports nested bindings, @scheme[list-for*] supports nested
iterations:

@interaction[
(list-for* ([s (list "a" "b" "c")]
            [n (list "x" "y" "z")])
  (string-append s n))
]

Unlike the @scheme[list-for], the nested iteration of
@scheme[list-for*] covers patterns with lists not as easily expressed
with @scheme[map]. When procedures like @scheme[map] suffice, however,
Scheme programmers tend to use them, partly because the syntax is
simpler (just a procedure call).

We have ignored several other variants of the interation
form---including plain @scheme[for], which is use when the iteration
body is to be run only for its effect. For more complete information,
see @secref["iterations+comprehensions"].

@;------------------------------------------------------------------------
@section{List Iteration from Scratch}

Although @scheme[map] and @scheme[list-for] are predefined, they are
not primitive in any interesting sense. You can write equivalent
iterations using a handful of list primitives.

Since a Scheme list is a linked list, the two core operations on a
non-empty list are

@itemize{

 @item{@scheme[first]: get the first thing in the list; and}

 @item{@scheme[rest]: get the rest of the list.}

}

@examples[
(first (list 1 2 3))
(rest (list 1 2 3))
]

To create a new node for a linked list---that is, to add to the front
of the list---use the @scheme[cons] procedure, which is short for
``construct.'' To get an empty list to start with, use the
@scheme[empty] constant:

@interaction[
empty
(cons "head" empty)
(cons "dead" (cons "head" empty))
]

To process a list, you need to be able to distinguish empty lists from
non-empty lists, because @scheme[first] and @scheme[rest] work only on
non-empty lists. The @scheme[empty?] procedure detects empty lists,
and @scheme[cons?] detects non-empty lists:

@interaction[
(empty? empty)
(empty? (cons "head" empty))
(cons? empty)
(cons? (cons "head" empty))
]

With these pieces, you can write your own versions of the
@scheme[length] procedure, @scheme[map] procedure, and more.

@defexamples[
(define (my-length lst)
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))
(my-length empty)
(my-length (list "a" "b" "c"))
]
@def+int[
(define (my-map f lst)
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
               (my-map f (rest lst)))]))
(my-map string-upcase (list "ready" "set" "go"))
]

If the derivation of the above definitions is mysterious to you,
consider reading @|HtDP|. But if you are merely suspicious of the use
of recursive calls instead of a looping construct, then read on.

Both the @scheme[my-length] and @scheme[my-map] procedures run in
@math{O(n)} time for a list of length @math{n}. This is easy to see by
imagining how @scheme[(my-length (list "a" "b" "c"))] must evaluate:

@schemeblock[
(my-length (list "a" "b" "c"))
#,step (+ 1 (my-length (list "b" "c")))
#,step (+ 1 (+ 1 (my-length (list "c"))))
#,step (+ 1 (+ 1 (+ 1 (my-length (list)))))
#,step (+ 1 (+ 1 (+ 1 0)))
#,step (+ 1 (+ 1 1))
#,step (+ 1 2)
#,step 3
]

For a list with @math{n} elements, evalution will stack up @math{n}
@scheme[(+ 1 ...)] additions, and then finally add them up when the
list is exhausted.

You can avoid piling up additions by adding along the way. To
accumulate a length this way, we need a procedure that takes both a
list and the length of the list seem so far; the code below uses a
local procedure @scheme[iter] that accumulates the length in an
argument @scheme[len]:

@schemeblock[
(define (my-length lst)
  (code:comment #, @elem{local procedure @scheme[iter]:})
  (define (iter lst len)
    (cond
     [(empty? lst) len]
     [else (iter (rest lst) (+ len 1))]))
  (code:comment #, @elem{body of @scheme[my-length] calls @scheme[iter]:})
  (iter lst 0))
]

Now evaluation looks like this:

@schemeblock[
(my-length (list "a" "b" "c"))
#,step (iter (list "a" "b" "c") 0)
#,step (iter (list "b" "c") 1)
#,step (iter (list "c") 2)
#,step (iter (list) 3)
3
]

The revised @scheme[my-length] runs in constant space, just as the
evaluation steps above suggest. That is, when the result of a
procedure call, like @scheme[(iter (list "b" "c") 1)], is exactly the
result of some other procedure call, like @scheme[(iter (list "c")
2)], then the first one doesn't have to wait around for the second
one, because that takes up space for no good reason.

This evaluation behavior is sometimes called @idefterm{tail-call
optimization}, but it's not merely an ``optimization'' in Scheme; it's
a guarantee about the way the code will run.

In the case of @scheme[my-map], @math{O(n)} space compelxity is
reasonable, since it has to generate a result of size
@math{O(n)}. Nevertheless, you can reduce the constant factor by
accumulating the result list. The only catch is that the accumulated
list will be backwards, so you'll have to reverse it at the very end:

@schemeblock[
(define (my-map f lst)
  (define (iter lst backward-result)
    (cond
     [(empty? lst) (reverse backward-result)]
     [else (iter (rest lst)
                 (cons (f (first lst))
                          backward-result))]))
  (iter lst empty))
]

It turns out that if you write

@schemeblock[
(define (my-map f lst)
  (list-for ([i lst])
    (f i)))
]

then the @scheme[list-for] form in the procedure both is expanded to
essentially the same code as the @scheme[iter] local definition and
use. The difference is merely syntactic convenience.

@;------------------------------------------------------------------------
@section{Recursion versus Iteration}

The @scheme[my-length] and @scheme[my-map] examples demonstrate that
iteration is just a special case of recursion. In many languages, it's
important to try to fit as many computations as possible into
iteration form. Otherwise, performance will be bad, and moderately
large inputs can lead to stack overflow.  Similarly, in Scheme, it is
often important to make sure that tail recursion is used to avoid
@math{O(n)} space consumption when the computation is easily performed
in constant space.

At the same time, recursion does not lead to particularly bad
performance in Scheme, and there is no such thing as stack overflow;
you can run out of memory if a computation involves too much context,
but exhausting memory typically requires orders of magnitude deeper
recursion than would trigger a stack overflow in other
languages. These considerations, combined with the fact that
tail-recursive programs automatically run the same as a loop, lead
Scheme programmers to embrace recursive forms rather than avoid them.

Suppose, for example, that you want to remove consecutive duplicates
from a list. While that procedure can be written as a loop that
remembers the previous element for each iteration, a Scheme programmer
would more likely just write the following:

@def+int[
(define (remove-dups l)
  (cond
   [(empty? l)                           empty]
   [(empty? (rest l))                    l]
   [(equal? (first l) (first (rest l)))  (remove-dups (rest l))]
   [else                                 (cons (first l) 
                                               (remove-dups (rest l)))]))
(remove-dups (list "a" "b" "b" "b" "c" "c"))
]

In general, this procedure consumes @math{O(n)} space for an input
list of length @math{n}, but that's fine, since it produces an
@math{O(n)} result. If the input list happens to be mostly consecutive
duplicates, then the resulting list can be much smaller than
@math{O(n)}---and @scheme[remove-dups] will also use much less than
@math{O(n)} space! The reason is that the third case in the
@scheme[cond], which discards duplicates, returns the result of a
@scheme[remove-dups] call directly, so the tail-call ``optimization''
kicks in:

@schemeblock[
(remove-dups (list "a" "b" "b" "b" "b" "b"))
#,step (cons "a" (remove-dups (list "b" "b" "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b")))
#,step (cons "a" (remove-dups (list "b")))
#,step (cons "a" (list "b"))
#,step (list "a" "b")
]

Tail-call behavior becomes even more important when dealing with
non-list data or when using an object-oriented style. In the latter
case, an object must sometimes dispatch to another object; if the
other object's result is the complete answer, there's no reason for
the first object to wait around. We defer futher discussion of this
point until @secref["datatypes"], after which we'll have more forms of
data to consider.

@;------------------------------------------------------------------------
@section{Named @scheme[let]}

As you start reading Scheme code, you'll discover one more form that
is commonly used to implement iterations and recursive functions:
@idefterm{named @scheme[let]}.  A named @scheme[let] uses the same
syntactic keyword as a simple sequence of local bindings, but an
@nonterm{identifier} after the @scheme[let] (instead of an immediate
open parenthesis) triggers a different parsing. In general,

@schemeblock[
#, @BNF-seq[@litchar{(} @litchar{let} @nonterm{proc-identifier} @litchar{(}
                        @kleenestar{@BNF-group[@litchar{[} @nonterm{arg-identifier} @nonterm{init-expression} @litchar{]}]}
                        @litchar{)}
                    @kleeneplus{@nonterm{body-expression}} @litchar{)}]
]

is equivalent to the sequence

@schemeblock[
#, @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{proc-identifier} @kleenestar{@nonterm{arg-identifier}} @litchar{)}
                  @kleeneplus{@nonterm{body-expression}} @litchar{)}]
#, @BNF-seq[@litchar{(}@nonterm{proc-identifier} @kleenestar{@nonterm{init-expression}}@litchar{)}]
]

except that the @scheme[let] form works in any expression
context.

That is, a named @scheme[let] binds a procedure identifier that is
visible only in the procedure's body, and it implicitly calls the
procedure with the values of some initial expressions.  A named
@scheme[let] looks similar to the start of @scheme[fold-for], but the
recursive calls in the body are explicit, and they are not constrained
to tail position.

As an example, here is @scheme[my-map] once again, using a named let
to bind the local @scheme[iter] procedure:

@schemeblock[
(define (my-map f lst)
  (let iter ([lst lst]
             [backward-result empty])
   (cond
    [(empty? lst) (reverse backward-result)]
    [else (iter (rest lst)
                (cons (f (first lst))
                      backward-result))])))
]

Here's another example, where the local @scheme[dup] procedure is used
recursively and not merely iteratively, and where the traversal of a
list stops part-way:

@def+int[
(define (duplicate pos lst)
  (let dup ([i 0]
            [lst lst])
   (cond
    [(= i pos) (cons (first lst) lst)]
    [else (cons (first lst) (dup (+ i 1) (rest lst)))])))
(duplicate 1 (list "apple" "cheese burger!" "banana"))
]
