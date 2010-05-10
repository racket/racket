#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "pairs"]{Pairs and Lists}

A @deftech{pair} joins two arbitrary values. The @scheme[cons]
procedure constructs pairs, and the @scheme[car] and @scheme[cdr]
procedures extract the first and second elements of the pair,
respectively. The @scheme[pair?] predicate recognizes pairs.

Some pairs print by wrapping parentheses around the printed forms of
the two pair elements, putting a @litchar{'} at the beginning and a
@litchar{.} between the elements.

@examples[
(cons 1 2)
(cons (cons 1 2) 3)
(car (cons 1 2))
(cdr (cons 1 2))
(pair? (cons 1 2))
]

A @deftech{list} is a combination of pairs that creates a linked
list. More precisely, a list is either the empty list @scheme[null],
or it is a pair whose first element is a list element and whose second
element is a list. The @scheme[list?] predicate recognizes lists. The
@scheme[null?]  predicate recognizes the empty list.

A list normally prints as a @litchar{'} followed by a pair of parentheses
wrapped around the list elements.

@examples[
null
(cons 0 (cons 1 (cons 2 null)))
(list? null)
(list? (cons 1 (cons 2 null)))
(list? (cons 1 2))
]

A list or pair prints using @schemeresult[list] or @schemeresult[cons]
when one of its elements cannot be written as a @scheme[quote]d
value. For example, a value constructed with @racket[srcloc] cannot be
written using @scheme[quote], and it prints using @racketresult[srcloc]:

@interaction[
(srcloc "file.rkt" 1 0 1 (+ 4 4))
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(cons 1 (srcloc "file.rkt" 1 0 1 8))
(cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))
]

@margin-note{See also @racket[list*].}

As shown in the last example, @schemeresult[list*] is used to
abbreviate a series of @schemeresult[cons]es that cannot be
abbreviated using @racketresult[list].

The @scheme[write] and @scheme[display] functions print a pair or list
without a leading @litchar{'}, @schemeresult[cons],
@schemeresult[list], or @schemeresult[list*]. There is no difference
between @scheme[write] and @racket[display] for a pair or list, except
as they apply to elements of the list:

@examples[
(write (cons 1 2))
(display (cons 1 2))
(write null)
(display null)
(write (list 1 2 "3"))
(display (list 1 2 "3"))
]

Among the most important predefined procedures on lists are those that
iterate through the list's elements:

@interaction[
(map (lambda (i) (/ 1 i))
     '(1 2 3))
(andmap (lambda (i) (i . < . 3))
       '(1 2 3))
(ormap (lambda (i) (i . < . 3))
       '(1 2 3))
(filter (lambda (i) (i . < . 3))
        '(1 2 3))
(foldl (lambda (v i) (+ v i))
       10
       '(1 2 3))
(for-each (lambda (i) (display i))
          '(1 2 3))
(member "Keys"
        '("Florida" "Keys" "U.S.A."))
(assoc 'where
       '((when "3:30") (where "Florida") (who "Mickey")))
]

@refdetails["pairs"]{pairs and lists}

Pairs are immutable (contrary to Lisp tradition), and @scheme[pair?]
and @scheme[list?] recognize immutable pairs and lists, only. The
@scheme[mcons] procedure creates a @deftech{mutable pair}, which works
with @scheme[set-mcar!] and @scheme[set-mcdr!], as well as
@scheme[mcar] and @scheme[mcdr]. A mutable pair prints using
@schemeresult[mcons], while @scheme[write] and @scheme[display] print
mutable pairs with @litchar["{"] and @litchar["}"]:

@examples[
(define p (mcons 1 2))
p
(pair? p)
(mpair? p)
(set-mcar! p 0)
p
(write p)
]

@refdetails["mpairs"]{mutable pairs}
