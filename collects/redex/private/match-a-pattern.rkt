#lang racket/base
(require racket/match 
         (for-syntax racket/match
                     racket/base))
(provide match-a-pattern)

#|

The grammar for the internal patterns is the 
contents of the should-be-pats list, where each
'pat' that appears behind an unquote there is
a self-reference in the grammar. 

  lpat ::= pat 
         | `(repeat ,pat ,(or/c symbol? #f) ,(or/c symbol? #f))
         ;; repeat indicates a repetition (ellipsis in the
         ;; surface language), where the pattern inside is
         ;; what's repeated, the second position is a name
         ;; if the ellipsis is named normally and the final
         ;; position is a name if the ellipsis has a mismatch
         ;; name (more below).
  var ::= symbol?     
  condition ::= (-> bindings? any) ;; any is treated like a boolean

Also, the `(cross ,nt) pattern alwyas has hypenated non-terminals, ie
(cross e) in the source turns into (cross e-e) after translation (which
means that the other cross non-terminals, e.g. (cross e-v), are not
directly available as redex patterns, but can only be used via the
non-terminals that Redex creates for the cross languages.

Internal patterns also come with the invariant that there are no
redundant or non-local ellipses names. That is, consider this pattern:

  (any_1 ..._1 any_1 ..._2)

It might seem like it would turn into something like this:

  (list (repeat (name any_1 any) ..._1 #f)
        (repeat (name any_1 any) ..._2 #f))

but the _1 and _2 are actually not as specific as they could be, 
since the any_1 name will force the two ellipses lengths to be
the same. So, this must turn into this pattern:

  (list (repeat (name any_1 any) ..._1 #f)
        (repeat (name any_1 any) ..._1 #f))

Similarly, if there are superflous names, they are deleted. For
example, this source pattern:

  (any_1 ..._1)

turns into this:

  (list (repeat (name any_1 any) #f #f))

Also, although there cannot be any patterns at the source level
that have both kinds of names, there can be once the ellipses
have been resolved. For example, this:

  (any_1 ..._1
   any_1 ..._!_2
   any_1 ..._1
   any_1 ..._!_2)

turns into this:

  (list (repeat (name any_1 any) ..._1 #f)
        (repeat (name any_1 any) ..._1 ..._!_2)
        (repeat (name any_1 any) ..._1 #f)
        (repeat (name any_1 any) ..._1 ..._!_2))

|#

(define-syntax (match-a-pattern stx)
  (define (check-pats pats allow-else?)
    (let ()
      (define should-be-pats
        (append '(`any
                  `number
                  `string
                  `natural
                  `integer
                  `real
                  `boolean
                  `variable
                  `(variable-except ,var ...)
                  `(variable-prefix ,var)
                  `variable-not-otherwise-mentioned
                  `hole
                  `(nt ,var)
                  `(name ,var ,pat)
                  `(mismatch-name ,var ,pat)
                  `(in-hole ,pat ,pat) ;; context, then contractum
                  `(hide-hole ,pat)
                  `(side-condition ,pat ,condition ,srcloc-expr)
                  `(cross ,var)
                  `(list ,lpat ...)
                  (? (compose not pair?))) ;; pattern for literals (numbers, strings, prefabs, etc etc etc)
                (if allow-else?
                    (list 'else)
                    (list))))
      (for ([pat (in-list pats)])
        (when (null? should-be-pats)
          (raise-syntax-error 'match-a-pattern "too many patterns" stx pat))
        (define should-be (car should-be-pats))
        (set! should-be-pats (cdr should-be-pats))
        (define pats-match?
          (let loop ([pat (syntax->datum pat)]
                     [should-be should-be])
            (cond
              [(and (null? pat) (null? should-be)) #t]
              [(and (pair? pat) (pair? should-be))
               (cond
                 [(eq? (car should-be) 'unquote)
                  (eq? (car pat) 'unquote)]
                 [else
                  (and (loop (car pat) (car should-be))
                       (loop (cdr pat) (cdr should-be)))])]
              [else (equal? pat should-be)])))
        (unless pats-match?
          (raise-syntax-error 'match-a-pattern
                              (format "expected pattern ~s" 
                                      should-be)
                              stx
                              pat)))
      (unless (null? should-be-pats)
        (raise-syntax-error 'match-a-pattern 
                            (format "did not find pattern ~s"
                                    (car should-be-pats))
                            stx))))
  (syntax-case stx ()
    [(_ #:allow-else to-match [pats rhs ...] ...)
     (let ()
       (check-pats (syntax->list #'(pats ...)) #t)
       #'(match to-match [pats rhs ...] ...))]
    [(_ to-match [pats rhs ...] ...)
     (let ()
       (check-pats (syntax->list #'(pats ...)) #f)
       #'(match to-match [pats rhs ...] ...))]))
