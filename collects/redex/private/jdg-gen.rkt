#lang racket/base

(require
 (only-in "rg.rkt"
          [compile rg:compile])
 (only-in "reduction-semantics.rkt"
          do-test-match)
 "pat-unify.rkt"
 (for-syntax racket/base)
 racket/match)

(provide pat->term
         check-dq)

;; term generation

;; pat->term pat* env -> term
(define (pat->term lang pat full-env [term-e (make-hash)])
  ;(printf "\np->t: ~s\n\n ~s\n" pat full-env)
  (define nt-matchers (make-hash))
  (define eqs (env-eqs full-env))
  (define (get-matcher nt)
    (hash-ref nt-matchers nt
              (λ () (let ([mtchr (do-test-match lang `(nt ,nt) '() 'pat->term #t)])
                      (hash-set! nt-matchers nt mtchr)
                      mtchr))))
  (define res-term
    (let recur ([p pat])
      (match p
        [`(name ,var ,(bound))
         (define-values (rep-lvar pat) (lookup var eqs))
         (call/ec (λ (fail)
                    (hash-ref term-e rep-lvar
                              (λ () (let ([t (recur pat)])
                                      (unless t (fail #f))
                                      (hash-set! term-e rep-lvar t)
                                      t)))))]
        [`(cstr (,nts ...) ,pat)
         (match pat
           [`(nt ,p-nt)
            (define all-nts (cons p-nt nts))
            (for/or ([nt-pat all-nts])
              (define term (recur `(nt ,nt-pat)))
              (and (for/and ([nt (remove nt-pat all-nts)])
                     ((get-matcher nt) term))
                   term))]
           [`any
            (for/or ([nt-pat nts])
              (define term (recur `(nt ,nt-pat)))
              (and (for/and ([nt (remove nt-pat nts)])
                     ((get-matcher nt) term))
                   term))]
           [else
            (define term (recur pat))
            (and (for/and ([nt nts])
                   ((get-matcher nt) term))
                 term)])]
        [`(name ,var ,pat)
         (error 'make-term "can't instantiate a term with an unbound variable: ~s" p)]
        [`(list ,ps ...)
         (call/ec (λ (fail)
                    (for/list ([p ps])
                      (let ([res (recur p)])
                        (unless res (fail #f))
                        res))))]
        [else
         (let-values ([(p bs) (gen-term p lang 2)])
           p)])))
  (and (check-dqs (remove-empty-dqs (env-dqs full-env)) term-e lang eqs)
       res-term))

(define (check-dqs dqs term-e lang eqs)
  (for/and ([dq dqs])
    (define te (hash-copy term-e))
    (define rhs (list-ref dq 0))
    (define lhs (list-ref dq 1))
    (check-dq rhs lhs te lang eqs)))

(define sym-index 0)

(struct not-ground ())

(define (check-dq rhs lhs term-e lang eqs)
  (set! sym-index 0)
  (define rhs-term (pat->term/term-e rhs term-e eqs lang))
  (define lhs-term (pat->term/term-e lhs term-e eqs lang))
  (not (equal? rhs-term lhs-term)))
  
(define (pat->term/term-e t term-e actual-e lang)
  (call/ec
   (λ (fail)
     (let recur ([p t])
       (match p
         [`(name ,var ,(bound))
          (if (hash-has-key? term-e (lvar var))
              (recur (hash-ref term-e (lvar var)))
              (let ([new-val (recur (hash-ref actual-e (lvar var)))])
                (hash-set! term-e (lvar var) new-val)
                new-val))]
         [`(cstr  (,nts ...) ,pat)
          (recur pat)]
         [`(list ,ps ...)
          (for/list ([p ps]) (recur p))]
         [`(nt ,_)
          (fail (not-ground))]
         [`(,stuff ...) ;; here it's a fully instanatiated list
          `(,@stuff)]
         [else
          (let-values ([(p bs) (gen-term p lang 2)])
           p)])))))
                   

(define (gen-term pat lang size [num-atts 1] [retries 100])
  (((rg:compile lang 'what) pat) size num-atts retries))

(define (lookup-pat id env)
  (define-values (_ pat) (lookup id env))
  pat)

(define (lookup-rep id env)
  (define-values (rep _) (lookup id env))
  rep)

(define (lookup id env)
  (define res (hash-ref env (lvar id)))
  (match res
    [(lvar new-id)
     (lookup new-id env)]
    [else
     (values (lvar id) res)]))
