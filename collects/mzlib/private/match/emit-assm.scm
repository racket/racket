;; This library is used by match.ss

(module emit-assm mzscheme
  (provide emit assm)
  
  (require "match-helper.ss"
	   "coupling-and-binding.scm")
  
  (require-for-template mzscheme)
  
  ;;!(function emit
  ;;          (form (emit act-test-func ae let-bound sf bv kf ks)
  ;;                ->
  ;;                syntax)
  ;;          (contract ((syntax -> syntax)
  ;;                     syntax
  ;;                     list
  ;;                     list
  ;;                     list
  ;;                     (list list -> syntax)
  ;;                     (list list -> syntax))
  ;;                    ->
  ;;                    syntax))
  ;; emit's true function is to manage the tests-seen-so-far lists
  ;; it decides whether a new test needs to be added to the list
  ;; or whether this condition has already been tested for and if
  ;; it is true emit calls the success function. If it has been
  ;; determined to be a false property emit calls the fail function.
  ;; emit adds implied truths to the test seen so far list so that
  ;; these truths can be checked against later.
  (define (emit act-test-func ae let-bound sf bv kf ks)
    (let ([test (syntax-object->datum (act-test-func ae))])
      (cond
        [(in test sf) (ks sf bv)]
        [(in `(not ,test) sf) (kf sf bv)]
        [else
         (let* ([pred (car test)]
                [exp (cadr test)]
                [implied (implied test)]
                [not-imp
                 (if (equal? pred 'list?)
                     (list `(not (null? ,exp)))
                     '())]
                [s (ks (cons test (append implied sf)) bv)]
                [k (kf (cons `(not ,test) (append not-imp sf)) bv)]
                [the-test (act-test-func (subst-bindings ae let-bound))])
           (assm (syntax-case the-test (struct-pred)
                   [(struct-pred pred parent-list exp) #'(pred exp)]
                   [reg #'reg])
                 k s))])))
  
  ;;!(function assm
  ;;          (form (assm tst main-fail main-succ) -> syntax)
  ;;          (contract (syntax syntax syntax) -> syntax))
  ;; assm - this function is responsible for constructing the actual
  ;; if statements.  It performs minor expansion optimizations.
  (define (assm tst main-fail main-succ)
    (node-count (add1 (node-count)))
    (cond 
      [(stx-equal? main-succ main-fail) 
       (begin 
         (when (stx-equal? main-succ #'(match-failure))
           (node-count (sub1 (node-count))))                 
         main-succ)]
      [(and (eq? (syntax-e main-succ) #t) (eq? (syntax-e main-fail) #f)) tst]
      [else
       (syntax-case main-succ (if
                               and
                               let/ec
                               lambda
                               let) ;free-identifier=?  ;stx-equal?
         [(if (and tsts ...) true-act fail-act)
          (stx-equal? main-fail #'fail-act)
          (quasisyntax/loc
              tst
            (if (and #,tst tsts ...) true-act fail-act))]
         [(if tst-prev true-act fail-act)
          (stx-equal? main-fail #'fail-act)
          (quasisyntax/loc
              tst
            (if (and #,tst tst-prev) true-act fail-act))]
         [(let/ec k (let ((fail (lambda () (_ f2)))) s2))
          (stx-equal? main-fail #'f2)
          (begin 
            (error "never happens")
            (printf "got here!~n")
            (quasisyntax/loc
                tst
              (let/ec k
                (let ((fail (lambda () (k #,main-fail))))
                  #,(assm tst (syntax/loc tst (fail)) (syntax s2))))))]
         ;; leaving out pattern that is never used in original
         [_ (quasisyntax/loc
                tst
              (if #,tst #,main-succ #,main-fail))])]))
  )