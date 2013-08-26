#lang racket/base

(require racket/match
         racket/set
         (only-in "rg.rkt"
                  [compile rg:compile])
         (only-in "reduction-semantics.rkt"
                  do-test-match)
         "pat-unify.rkt"
         (for-syntax racket/base))

(provide pat->term
         check-dq
         dq)

(struct ok ())


;; term generation

;; pat->term lang pat* env env -> term
(define (pat->term lang pat full-env [term-e (make-hash)])
  (define nt-matchers (make-hash))
  (define eqs (env-eqs full-env))
  (define (get-matcher nt)
    (hash-ref nt-matchers nt
              (λ () (let ([mtchr (do-test-match lang `(nt ,nt) '() 'pat->term #t)])
                      (hash-set! nt-matchers nt mtchr)
                      mtchr))))
  (define (ground-or-ok p)
    (let/ec okk
      (let recur ([p p])
        (match p
          [(lvar id)
           ;; careful! term-e has terms, eqs has pats!
           (hash-ref term-e p
              (λ () (recur (hash-ref eqs p))))]
          [`(name ,id ,(bound))
           (hash-ref term-e (lvar id)
              (λ () (recur (hash-ref eqs (lvar id)))))]
          [`(list ,ps ...)
           `(,@(for/list ([p ps]) (recur p)))]
          [`(cstr (,nts ...) ,p)
           (recur p)]
          [`(nt ,_)
           (okk (ok))]
          [(? predef-pat? _)
           (okk (ok))]
          [_ p]))))
  ;; do this first since the term environment (term-e) is needed for the dqs
  (define res-term
    (let recur ([p pat])
      (match p
        [`(name ,var ,(bound))
         (define-values (rep-lvar pat) (lookup var eqs))
         (call/ec (λ (fail)
                    (hash-ref term-e rep-lvar
                              (λ () (let ([t (recur pat)])
                                      (unless (not-failed? t) (fail (unif-fail)))
                                      (hash-set! term-e rep-lvar t)
                                      t)))))]
        [`(cstr (,nts ...) ,pat)
         (match pat
           [`(nt ,p-nt)
            (define all-nts (cons p-nt nts))
            (for/not-failed ([nt-pat all-nts])
                            (define term (recur `(nt ,nt-pat)))
                            (and/fail (for/and ([nt (remove nt-pat all-nts)])
                                        ((get-matcher nt) term))
                                      term))]
           [`any
            (for/not-failed ([nt-pat nts])
                            (define term (recur `(nt ,nt-pat)))
                            (and/fail (for/and ([nt (remove nt-pat nts)])
                                        ((get-matcher nt) term))
                                      term))]
           [_
            (define term (recur pat))
            (and/fail (for/and ([nt nts])
                        ((get-matcher nt) term))
                      term)])]
        [`(name ,var ,pat)
         (error 'make-term "can't instantiate a term with an unbound variable: ~s" p)]
        [`(list ,ps ...)
         (call/ec (λ (fail)
                    (for/list ([p ps])
                      (let ([res (recur p)])
                        (unless (not-failed? res) (fail (unif-fail)))
                        res))))]
        [_
         (make-term p lang)])))
  (and/fail 
   (not-failed? res-term)
   (for/and ([(k v) (in-hash eqs)])
     (match v
       [`(cstr (,nts ...) ,p)
        (define grook (ground-or-ok p))
        (or (ok? grook)
            (for/and ([nt nts])
              ((get-matcher nt) grook)))]
       [_ #t]))
   (check-dqs (remove-empty-dqs (env-dqs full-env)) term-e lang eqs)
   res-term))

(define-syntax-rule (for/not-failed ((x xs)) b ...)
  (for/fold ([res (unif-fail)])
    ([x xs])
    #:break (not-failed? res)
    b ...))

(define (make-term p lang)
  (let-values ([(p bs) (gen-term p lang 2)])
    p))   

(define (check-dqs dqs term-e lang eqs)
  (for/and ([dq dqs])
    (define te (hash-copy term-e))
    (check-dq dq te lang eqs)))

(struct not-ground ())

(define (check-dq the-dq term-e lang eqs)
  (match-define (dq ps `(,lhs ,rhs)) the-dq)
  (define rhs-term (pat->term/term-e ps rhs term-e eqs lang))
  (define lhs-term (pat->term/term-e ps lhs term-e eqs lang))
  (not (compare-partial-terms rhs-term lhs-term)))
  
(define (pat->term/term-e ps t term-e actual-e lang)
  (call/ec
   (λ (fail)
     (let recur ([p t])
       (match p
         [`(name ,var ,(bound))
          (cond
            [(member var ps)
             `(name ,var ,(bound))]
            [(hash-has-key? term-e (lvar var))
             (recur (hash-ref term-e (lvar var)))]
            [else
             (let ([new-val (recur (hash-ref actual-e (lvar var)))])
               (hash-set! term-e (lvar var) new-val)
               new-val)])]
         [`(cstr  (,nts ...) ,pat)
          (recur pat)]
         [`(list ,ps ...)
          (for/list ([p ps]) (recur p))]
         [`(nt ,_)
          (fail (not-ground))]
         [`(,stuff ...) ;; here it's a fully instanatiated list
          `(,@stuff)]
         [_
          (let-values ([(p bs) (gen-term p lang 2)])
           p)])))))

(define (compare-partial-terms l r)
  (define param-vals (hash))
  (define (update-param-vals p v)
    (set! param-vals
          (hash-set param-vals p
              (set-add (hash-ref param-vals p (λ () (set))) v))))
  (and
   (let recur ([l l]
               [r r])
     (match* (l r)
       [(`(name ,lv ,(bound)) `(name ,rv ,(bound)))
        (update-param-vals lv r)
        (update-param-vals rv l)
        #t]
       [(`(name ,lv ,(bound)) _)
        (update-param-vals lv r)
        #t]
       [(_ `(name ,rv ,(bound)))
        (update-param-vals rv l)
        #t]
       [(`(,l-ts ...) `(,r-ts ...))
        (and (= (length l-ts) (length r-ts))
             (for/and ([lt l-ts]
                       [rt r-ts])
               (recur lt rt)))]
       [(_ _)
        (equal? l r)]))
   ;; TODO: handle case where param appears twice against different stuff
   (for/and ([vs (in-list (hash-values param-vals))])
     ((set-count vs) . < . 2))))
                   

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
    [_
     (values (lvar id) res)]))
