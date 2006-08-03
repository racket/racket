;; (documentation (name plt-match))
;; <pre>Pattern Matching Syntactic Extensions for Scheme
;;
;; All bugs or questions concerning this software should be directed to
;; Bruce Hauman <bhauman@cs.wcu.edu>.  The latest version of this software
;; can be obtained from http://sol.cs.wcu.edu/~bhauman/scheme/pattern.php.
;;
;; Special thanks go out to:
;; Robert Bruce Findler for support and bug detection.
;; Doug Orleans for pointing out that pairs should be reused while
;; matching lists.
;;
;;
;; Originally written by Andrew K. Wright, 1993 (wright@research.nj.nec.com)
;; which in turn was adapted from code written by Bruce F. Duba, 1991.
;;
;; This software is in the public domain.  Feel free to copy,
;; distribute, and modify this software as desired.  No warranties
;; nor guarantees of any kind apply.  Please return any improvements
;; or bug fixes to bhauman@cs.wcu.edu so that they may be included
;; in future releases.
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
;;                                         doesn't conflict with ..k, __k or _
;;       | _                               anything
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (var id)                        allows one to use ..k or _ as 
;;                                         identifiers
;;       | (list lvp_1 ... lvp_n)               list of n elements
;;       | (list-rest lvp_1 ... lvp_n pat) an improper list of n elements
;;                                         plus a last element which represents
;;                                         the last cdr of the list
;;       | (vector lvp_1 ... lvp_n)        vector of n elements
;;       | (box pat)                       box
;;       | (struct struct-name (pat_1 ... pat_n)) a structure
;;       | (regexp exp)                     if regular expression exp matches
;;       | (regexp exp pat)                 if result of regexp-match matches pat
;;       | (pregexp exp)                    if pregexp.ss regular expression exp matches
;;       | (pregexp exp pat)                if result of pregexp-match matches pat
;;       | (list-no-order pat ...)         matches a list with no regard for 
;;                                         the order of the
;;                                         items in the list
;;       | (list-no-order pat ... pat_n ooo) pat_n matches the remaining 
;;                                           unmatched items
;;       | (hash-table (pat_k pat_v) ...)  matches the elements of a hash table
;;       | (hash-table (pat_k pat_v) ... (pat_kn pat_vn) ooo)  
;;                                         pat_kn must match the remaining 
;;                                         unmatched key elements
;;                                         pat_vn must match the remaining 
;;                                         unmatched value elements
;;       | (app field pat)                 a field of a structure (field is 
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
;;       | ,@(list lvp . . . lvp-n)
;;       | ,@(list-rest lvp-1 . . . lvp-n pat)
;;       | ,@`qp                           qp must evaluate to a list as 
;;                                         so that this rule resembles the 
;;                                         above two rules
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(module plt-match mzscheme
  (provide 
   match
   match-lambda
   match-lambda*
   match-let
   match-let*
   match-letrec
   match-define
   pregexp-match-with-error
   exn:misc:match?
   exn:misc:match-value
   match-equality-test
   define-match-expander) 
  
  (require "private/match/match-internal-func.ss"
           "private/match/match-expander.ss"
           "private/match/match-helper.ss"
           "private/match/match-error.ss"
	   "private/match/test-no-order.ss")
  
  )




