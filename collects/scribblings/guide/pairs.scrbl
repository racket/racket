#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]
@require[(lib "list.ss")]
@define[mutable-cons cons]

@title{Pairs and Lists}

A @defterm{pair} joins two arbitrary values. The @scheme[cons]
procedure constructs pairs, and the @scheme[car] and @scheme[cdr]
procedures extract the first and second elements of the pair,
respectively. The @scheme[pair?] predicate recognizes pairs.

Some pairs print by wrapping parentheses around the printed forms of
the two pair elements, putting a @litchar{.} between them.

@examples[
(cons 1 2)
(cons (cons 1 2) 3)
(car (cons 1 2))
(cdr (cons 1 2))
(pair? (cons 1 2))
]

A @defterm{list} is a combination of pairs that creates a linked
list. More precisely, a list is either the empty list @scheme[null],
or it is a pair whose first element is a list element and whose second
element is a list. The @scheme[list?] predicate recognizes lists. The
@scheme[null?]  predicate recognizes the empty list.

A list prints as a pair of parentheses wrapped around the list
elements.

@examples[
null
(cons 0 (cons 1 (cons 2 null)))
(list? null)
(list? (cons 1 (cons 2 null)))
(list? (cons 1 2))
]

An expression with @litchar{'} followed by the printed form of a pair
or list produces a pair or list constant.

@examples[
'()
'(1 . 2)
'(1 2 3)
]

A pair can be mutable or immutable. Most pairs are immutable (contrary
to Lisp tradition), and @scheme[pair?] and @scheme[list?] recognize
immutable pairs and lists, only. The @scheme[mutable-cons] procedure
creates a mutable pair, which works with @scheme[set-car!] and
@scheme[set-cdr!], as well as @scheme[car] and @scheme[cdr].

@examples[
(define p (mutable-cons 1 2))
p
(eval:alts (pair? p) #f)
(set-car! p 0)
p
]

Among the most important predefined proecdures on lists are those that
iterate through the lists elements:

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

@refdetails["mz:pairs"]{pairs and lists}
