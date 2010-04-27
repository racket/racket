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
the two pair elements, putting a @litchar{`} at the beginning and a
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

A list prints as a @litchar{`} followed by a pair of parentheses
wrapped around the list elements.

@examples[
null
(cons 0 (cons 1 (cons 2 null)))
(list? null)
(list? (cons 1 (cons 2 null)))
(list? (cons 1 2))
]

The @scheme[display] function prints a pair or list without a leading
@litchar{`}:

@examples[
(display (cons 1 2))
(display null)
(display (list 1 2 3))
]

Pairs are immutable (contrary to Lisp tradition), and @scheme[pair?]
and @scheme[list?] recognize immutable pairs and lists, only. The
@scheme[mcons] procedure creates a mutable pair, which works with
@scheme[set-mcar!] and @scheme[set-mcdr!], as well as @scheme[mcar]
and @scheme[mcdr].

@examples[
(define p (mcons 1 2))
p
(pair? p)
(mpair? p)
(set-mcar! p 0)
p
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
