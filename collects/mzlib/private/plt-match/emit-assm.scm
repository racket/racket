;; This library is used by match.ss

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
(define emit
  (lambda (act-test-func ae let-bound sf bv kf ks)
    (let ((test (syntax-object->datum (act-test-func ae))))
      (cond
       ((in test sf) (ks sf bv))
       ((in `(not ,test) sf) (kf sf bv))
       (else
        (let* ((pred (car test))
               (exp (cadr test))
               (implied (implied test))
               (not-imp
                (if (equal? pred 'list?)
                    (list `(not (null? ,exp)))
                    '()))
               (s (ks (cons test (append implied sf)) bv))
               (k (kf (cons `(not ,test) (append not-imp sf)) bv))
               (the-test (act-test-func (subst-bindings ae let-bound))))
          (assm (syntax-case the-test (struct-pred)
                  ((struct-pred pred parent-list exp) (syntax (pred exp)))
                  (reg (syntax reg)))
                k s))))))) 

;;!(function assm
;;          (form (assm tst main-fail main-succ) -> syntax)
;;          (contract (syntax syntax syntax) -> syntax))
;; assm - this function is responsible for constructing the actual
;; if statements.  It performs minor expansion optimizations.
(define assm
  (lambda (tst main-fail main-succ)
    (let ((s (syntax-object->datum main-succ))
          (f (syntax-object->datum main-fail)))
      ;; this is for match-count
      ;;(write (syntax-object->datum tst))(newline)
      (set! node-count (add1 node-count))
      (cond ((equal? s f) 
              (begin 
                (when (equal? s '(match-failure))
                   (set! node-count (sub1 node-count))
                  ;(write 'here)(newline)
                  '()
                  )
                main-succ))
            ((and (eq? s #t) (eq? f #f)) tst)
            (else
             (syntax-case main-succ (if
                                     and
                                     call/ec
                                     lambda
                                     let) ;free-identifier=?  ;stx-equal?
               ((if (and tsts ...) true-act fail-act)
                (equal? f (syntax-object->datum (syntax fail-act)))
                (quasisyntax/loc
                 tst
                 (if (and #,tst tsts ...) true-act fail-act)))
               ((if tst-prev true-act fail-act)
                (equal? f (syntax-object->datum (syntax fail-act)))
                (quasisyntax/loc
                 tst
                 (if (and #,tst tst-prev) true-act fail-act)))
               ((call/ec
                 (lambda (k) (let ((fail (lambda () (_ f2)))) s2)))
                (equal? f (syntax-object->datum (syntax f2)))
                (quasisyntax/loc
                 tst
                 (call/ec
                  (lambda (k)
                    (let ((fail (lambda () (k #,main-fail))))
                      #,(assm tst (syntax/loc tst (fail)) (syntax s2)))))))
               ;; leaving out pattern that is never used in original
               (_ (quasisyntax/loc
                   tst
                   (if #,tst #,main-succ #,main-fail)))))))))
