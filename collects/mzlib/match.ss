;; (documentation (name match))
;; <pre>Pattern Matching Syntactic Extensions for Scheme
;;
;; Special thanks go out to:
;; Robert Bruce Findler for support and bug detection.
;; Doug Orleans for pointing out that pairs should be reused while
;; matching lists.
;;
;; Originally written by Andrew K. Wright, 1993 (wright@research.nj.nec.com)
;; which in turn was adapted from code written by Bruce F. Duba, 1991.
;;
;; This macro package extends Scheme with several new expression forms.
;; Following is a brief summary of the new forms.  See the associated
;; LaTeX documentation for a full description of their functionality.
;;
;;
;;         match expressions:
;;
;; exp ::= ...
;;       | (match exp clause ...)
;;       | (match-lambda clause ...) 
;;       | (match-lambda* clause ...)
;;       | (match-let ((pat exp) ...) body ...)
;;       | (match-let var ((pat exp) ...) body ...)
;;       | (match-let* ((pat exp) ...) body ...)
;;       | (match-letrec ((pat exp) ...) body ...)
;;       | (match-define pat exp)
;;
;; clause ::= (pat body) | (pat (=> identifier) exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= 
;;         identifier                      this binds an identifier if it 
;;                                         doesn't conflict with
;;                                         ..k, var, $, =, and, 
;;                                         or, not, ?, set!, or get!
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (lvp_1 ... lvp_n)               list of n elements
;;       | (pat ... pat_n . pat_{n+1})           list of n or more
;;       | #(lvp_1 ... lvp_n)              vector of n elements
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (= field pat)                   a field of a structure (field is 
;;                                         an accessor)
;;                                         Actually field can be any function 
;;                                         which can be
;;                                         applied to the data being matched.
;;                                         Ex: (match 5 ((= add1 b) b)) => 6
;;
;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
;;                                           pat_1 thru pat_n match
;;       | (set! identifier)               anything, and binds setter
;;       | (get! identifier)               anything, and binds getter
;;       | `qp                             a quasi-pattern
;;
;; lvp ::= pat ooo                         greedily matches n or more of pat, 
;;                                         each element must match pat
;;       | pat                             matches pat
;;
;; ooo ::= ...                             zero or more
;;       | ___                             zero or more
;;       | ..k                             k or more
;;       | __k                             k or more
;;
;;         quasi-patterns:                 matches:
;;
;; qp  ::= ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | identifier                      a symbol
;;       | (qp_1 ... qp_n)                 list of n elements
;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #(qp_1 ... qp_n)                vector of n elements
;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #&qp                            box
;;       | ,pat                            a pattern
;;       | ,@(lvp . . . lvp-n)
;;       | ,@(pat . . . pat_n . pat_{n+1})
;;       | ,@`qp                           qp must evaluate to a list as 
;;                                         so that this rule resembles the 
;;                                         above two rules
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, list-no-order, hash-table, ..., ___) 
;; cannot be used as pattern variables.</pre>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module match mzscheme
  (provide
   match
   match-lambda
   match-lambda*
   match-let
   match-let*
   match-letrec
   match-define   
   match-equality-test
   exn:misc:match?
   exn:misc:match-value
   define-match-expander)
  
  ;; FIXME: match-helper and match-error should each be split
  ;; into a compile-time part and a run-time part.

  (require-for-syntax "private/match/convert-pat.ss"
                      "private/match/match-helper.ss")
  
  (require-for-template mzscheme)
  
  (require  (prefix plt: "private/match/match-internal-func.ss")
	    "private/match/match-expander.ss"
	    "private/match/match-helper.ss"
	    "private/match/match-error.ss"
	    "private/match/test-no-order.ss")
  
  
  (define-syntax match-definer
    (syntax-rules ()
      [(match-definer name clauses ...)
       (define-syntax (name stx)
         (md-help syntax stx
                  (syntax-case stx ()
                    clauses ...)))]))
  
  (match-definer match-lambda
    [(k clause ...)
     (with-syntax ([(new-clauses ...) (handle-clauses #'(clause ...))]) 
       #'(plt:match-lambda new-clauses ...))])
  
  (match-definer match-lambda*
    [(k clause ...)
     (with-syntax ([(new-clauses ...) (handle-clauses #'(clause ...))])
       #'(plt:match-lambda* new-clauses ...))])
  
  (match-definer match-let
    [(k name (clauses ...) body ...)
     (identifier? (syntax name))
     (with-syntax ([(new-clauses ...) (handle-clauses #'(clauses ...))])
       #'(plt:match-let name (new-clauses ...) body ...))]          
    [(k (clauses ...) body ...)
     (with-syntax ([(new-clauses ...) (handle-clauses #'(clauses ...))])
       #'(plt:match-let (new-clauses ...) body ...))])
  
  (match-definer match-let*
    [(k (clauses ...) body ...)
     (with-syntax
         ([(new-clauses ...) (handle-clauses #'(clauses ...))])
       #'(plt:match-let* (new-clauses ...) body ...))])

  (match-definer match
    [(_ exp clause ...)
     (with-syntax 
         ([(new-clauses ...) (handle-clauses #'(clause ...))])
       #'(plt:match exp new-clauses ...))])
  
  
  (match-definer match-letrec
    [(k (clauses ...) body ...)
     (with-syntax 
         ([(new-clauses ...) (handle-clauses #'(clauses ...))])
       #'(plt:match-letrec (new-clauses ...) body ...))])

  
  (match-definer match-define
    [(k pat exp)
     (with-syntax ([new-pat (convert-pat #'pat)])
       #'(plt:match-define new-pat exp))])
  
  
  
  )


