#lang racket/base
(require syntax/parse/private/minimatch
         racket/private/stx) ;; syntax/stx
(provide translate
         error/not-stx)

#|
;; Doesn't seem to make much difference.
(require (rename-in racket/unsafe/ops
                    [unsafe-vector-ref vector-ref]
                    [unsafe-vector-set! vector-set!]
                    [unsafe-car car]
                    [unsafe-cdr cdr]))
|#

;; ============================================================

#|
A Guide (G) is one of:
  - '_
  - positive-exact-integer  ;; represents depth=0 pvar ref or metafun ref
  - negative-exact-integer  ;; represents depth>0 pvar ref (within ellipsis)
  - (cons G G)
  - (vector 'vector G)
  - (vector 'struct G)
  - (vector 'box G)
  - (vector 'dots HG (listof (vector-of integer)) nat (listof nat) G)
  - (vector 'app HG G)
  - (vector 'escaped G)
  - (vector 'orelse G (vector-of integer) G)
  - (vector 'metafun integer G)

A HeadGuide (HG) is one of:
  - G
  - (vector 'app-opt H (vector-of integer))
  - (vector 'orelse-h H (vector-of integer) H)
  - (vector 'splice G)
|#

(define (head-guide? x)
  (match x
    [(vector 'app-opt g vars) #t]
    [(vector 'splice g) #t]
    [(vector 'orelse-h g1 vars g2) #t]
    [_ #f]))

;; ============================================================

;; A translated-template is (vector loop-env -> syntax)
;; A loop-env is either a vector of values or a single value,
;; depending on lenv-mode of enclosing ellipsis ('dots) form.

(define (translate stx g env-length)
  (let ([f (translate-g stx stx g env-length 0)])
    (lambda (env lenv)
      (unless (>= (vector-length env) env-length)
        (error 'template "internal error: environment too short"))
      (f env lenv))))

;; lenv-mode is one of
;;  - 'one ;; lenv is single value; address as -1
;;  - nat  ;; lenv is vector; address as (- -1 index); 0 means no loop env

(define (translate-g stx0 stx g env-length lenv-mode)
  (define (loop stx g) (translate-g stx0 stx g env-length lenv-mode))
  (define (loop-h stx hg) (translate-hg stx0 stx hg env-length lenv-mode))
  (define (get index env lenv) (get-var index env lenv lenv-mode))
  (match g
    ['_ (lambda (env lenv) stx)]
    [(? exact-integer? index)
     (check-var index env-length lenv-mode)
     (lambda (env lenv) (check-stx stx (get index env lenv)))]
    [(cons g1 g2)
     (let ([f1 (loop (stx-car stx) g1)]
           [f2 (loop (stx-cdr stx) g2)])
       (cond [(syntax? stx)
              (lambda (env lenv)
                (restx stx (cons (f1 env lenv) (f2 env lenv))))]
             [(eq? g1 '_)
              (let ([c1 (stx-car stx)])
                (lambda (env lenv)
                  (cons c1 (f2 env lenv))))]
             [(eq? g2 '_)
              (let ([c2 (stx-cdr stx)])
                (lambda (env lenv)
                  (cons (f1 env lenv) c2)))]
             [else
              (lambda (env lenv)
                (cons (f1 env lenv) (f2 env lenv)))]))]
    [(vector 'dots ghead henv nesting uptos gtail)
     ;; At each nesting depth, indexes [0,upto) of lenv* vary; the rest are fixed.
     ;; An alternative would be to have a list of henvs, but that would inhibit
     ;; the nice simple vector reuse via vector-car/cdr!.
     (let* ([lenv*-len (vector-length henv)]
            [ghead-is-hg? (head-guide? ghead)]
            [ftail (loop (stx-drop (add1 nesting) stx) gtail)])
       (for ([var (in-vector henv)])
         (check-var var env-length lenv-mode))
       (unless (= nesting (length uptos))
         (error 'template "internal error: wrong number of uptos"))
       (let ([last-upto
              (for/fold ([last 1]) ([upto (in-list uptos)])
                (unless (<= upto lenv*-len)
                  (error 'template "internal error: upto is to big"))
                (unless (>= upto last)
                  (error 'template "internal error: uptos decreased: ~e" uptos))
                upto)])
         (unless (= lenv*-len last-upto)
           (error 'template "internal error: last upto was not full env")))
       (cond [(and (= lenv*-len 1) (= nesting 1) (not ghead-is-hg?) (equal? ghead '-1))
              ;; template was just (pvar ... . T)
              (let ([fhead (translate-g stx0 (stx-car stx) ghead env-length 'one)])
                (lambda (env lenv)
                  (let ([lenv* (get (vector-ref henv 0) env lenv)])
                    (restx stx (append lenv* (ftail env lenv))))))]
             [(and (= lenv*-len 1) (= nesting 1) (not ghead-is-hg?))
              (let ([fhead (translate-g stx0 (stx-car stx) ghead env-length 'one)])
                (lambda (env lenv)
                  (restx stx
                         (let dotsloop ([lenv* (get (vector-ref henv 0) env lenv)])
                           (if (null? lenv*)
                               (ftail env lenv)
                               (cons (fhead env (car lenv*))
                                     (dotsloop (cdr lenv*))))))))]
             [else
              (let ([fhead (if ghead-is-hg?
                               (translate-hg stx0 (stx-car stx) ghead env-length lenv*-len)
                               (translate-g stx0 (stx-car stx) ghead env-length lenv*-len))])
                (lambda (env lenv)
                  (define (nestloop lenv* nesting uptos)
                    (cond [(zero? nesting)
                           (fhead env lenv*)]
                          [else
                           (check-lenv stx lenv*)
                           (let ([iters (length (vector-ref lenv* 0))])
                             (let ([lenv** (make-vector lenv*-len)]
                                   [upto** (car uptos)]
                                   [uptos** (cdr uptos)])
                               (let dotsloop ([iters iters])
                                 (if (zero? iters)
                                     null
                                     (begin (vector-car/cdr! lenv** lenv* upto**)
                                            (cons (nestloop lenv** (sub1 nesting) uptos**)
                                                  (dotsloop (sub1 iters))))))))]))
                  (let ([head-results
                         ;; if ghead-is-hg?, is (listof^(nesting+1) stx) -- extra listof for loop-h
                         ;; otherwise, is (listof^nesting stx)
                         (nestloop (vector-map (lambda (index) (get index env lenv)) henv)
                                   nesting uptos)]
                        [tail-result (ftail env lenv)])
                    (restx stx
                           (nested-append head-results
                                          (if ghead-is-hg? nesting (sub1 nesting))
                                          tail-result)))))]))]
    [(vector 'app ghead gtail)
     (let ([fhead (loop-h (stx-car stx) ghead)]
           [ftail (loop (stx-cdr stx) gtail)])
       (lambda (env lenv)
         (restx stx (append (fhead env lenv) (ftail env lenv)))))]
    [(vector 'escaped g1)
     (loop (stx-cadr stx) g1)]
    [(vector 'orelse g1 drivers1 g2)
     (let ([f1 (loop (stx-cadr stx) g1)]
           [f2 (loop (stx-caddr stx) g2)])
       (for ([var (in-vector drivers1)])
         (check-var var env-length lenv-mode))
       (lambda (env lenv)
         (if (for/and ([index (in-vector drivers1)]) (get index env lenv))
             (f1 env lenv)
             (f2 env lenv))))]
    [(vector 'metafun index g1)
     (let ([f1 (loop (stx-cdr stx) g1)])
       (check-var index env-length lenv-mode)
       (lambda (env lenv)
         (let ([v (restx stx (cons (stx-car stx) (f1 env lenv)))]
               [mark (make-syntax-introducer)]
               [old-mark (current-template-metafunction-introducer)]
               [mf (get index env lenv)])
           (parameterize ((current-template-metafunction-introducer mark))
             (let ([r (mf (mark (old-mark v)))])
               (unless (syntax? r)
                 (raise-syntax-error 'template "result of metafunction was not syntax" stx))
               (restx stx (old-mark (mark r))))))))]
    [(vector 'vector g1)
     (let ([f1 (loop (vector->list (syntax-e stx)) g1)])
       (lambda (env lenv)
         (restx stx (list->vector (f1 env lenv)))))]
    [(vector 'struct g1)
     (let ([f1 (loop (cdr (vector->list (struct->vector (syntax-e stx)))) g1)]
           [key (prefab-struct-key (syntax-e stx))])
       (lambda (env lenv)
         (restx stx (apply make-prefab-struct key (f1 env lenv)))))]
    [(vector 'box g1)
     (let ([f1 (loop (unbox (syntax-e stx)) g1)])
       (lambda (env lenv)
         (restx stx (box (f1 env lenv)))))]))

(define (translate-hg stx0 stx hg env-length lenv-mode)
  (define (loop stx g) (translate-g stx0 stx g env-length lenv-mode))
  (define (loop-h stx hg) (translate-hg stx0 stx hg env-length lenv-mode))
  (define (get index env lenv) (get-var index env lenv lenv-mode))
  (match hg
    [(vector 'app-opt hg1 drivers1)
     (let ([f1 (loop-h (stx-cadr stx) hg1)])
       (for ([var (in-vector drivers1)])
         (check-var var env-length lenv-mode))
       (lambda (env lenv)
         (if (for/and ([index (in-vector drivers1)]) (get index env lenv))
             (f1 env lenv)
             null)))]
    [(vector 'orelse-h hg1 drivers1 hg2)
     (let ([f1 (loop-h (stx-cadr stx) hg1)]
           [f2 (loop-h (stx-caddr stx) hg2)])
       (for ([var (in-vector drivers1)])
         (check-var var env-length lenv-mode))
       (lambda (env lenv)
         (if (for/and ([index (in-vector drivers1)]) (get index env lenv))
             (f1 env lenv)
             (f2 env lenv))))]
    [(vector 'splice g1)
     (let ([f1 (loop (stx-cdr stx) g1)])
       (lambda (env lenv)
         (let* ([v (f1 env lenv)]
                [v* (stx->list v)])
           (unless v*
             (raise-syntax-error 'template
                                 "splicing template did not produce a syntax list"
                                 stx))
           v*)))]
    [else
     (let ([f (loop stx hg)])
       (lambda (env lenv)
         (list (f env lenv))))]))

(define (get-var index env lenv lenv-mode)
  (cond [(positive? index)
         (vector-ref env (sub1 index))]
        [(negative? index)
         (case lenv-mode
           ((one) lenv)
           (else (vector-ref lenv (- -1 index))))]))

(define (check-var index env-length lenv-mode)
  (cond [(positive? index)
         (unless (< (sub1 index) env-length)
           (error/bad-index index))]
        [(negative? index)
         (unless (< (- -1 index)
                    (case lenv-mode
                      ((one) 1)
                      (else lenv-mode)))
           (error/bad-index))]))

(define (check-lenv stx lenv)
  (for ([v (in-vector lenv)])
    (unless v
      (error 'template "pattern variable used in ellipsis pattern is not defined")))
  (let ([len0 (length (vector-ref lenv 0))])
    (for ([v (in-vector lenv)])
      (unless (= len0 (length v))
        (raise-syntax-error 'template
                            "incomplatible ellipsis match counts for template"
                            stx)))))

;; ----

(define current-template-metafunction-introducer
  (make-parameter
   (lambda (stx)
     (if (syntax-transforming?)
         (syntax-local-introduce stx)
         stx))))

;; ----

(define (stx-cadr x) (stx-car (stx-cdr x)))
(define (stx-cddr x) (stx-cdr (stx-cdr x)))
(define (stx-caddr x) (stx-car (stx-cdr (stx-cdr x))))

(define (stx-drop n x)
  (cond [(zero? n) x]
        [else (stx-drop (sub1 n) (stx-cdr x))]))

(define (restx basis val)
  (if (syntax? basis)
      (let ([stx (datum->syntax basis val basis)]
            [paren-shape (syntax-property basis 'paren-shape)])
        (if paren-shape
            (syntax-property stx 'paren-shape paren-shape)
            stx))
      val))

;; nested-append : (listof^(nesting+1) A) nat (listof A) -> (listof A)
;; (Actually, in practice onto is stx, so this is an improper append.)
(define (nested-append lst nesting onto)
  (cond [(zero? nesting) (append lst onto)]
        [(null? lst) onto]
        [else (nested-append (car lst) (sub1 nesting)
                             (nested-append (cdr lst) nesting onto))]))

(define (check-stx ctx v)
  (if (syntax? v)
      v
      (error/not-stx ctx v)))

(define (error/not-stx ctx v)
  (raise-syntax-error 'template "pattern variable is not syntax-valued" ctx))

(define (error/bad-index index)
  (error 'template "internal error: bad index: ~e" index))

(define (vector-car/cdr! dest-v src-v upto)
  (let ([len (vector-length dest-v)])
    (let loop ([i 0])
      (when (< i upto)
        (let ([p (vector-ref src-v i)])
          (vector-set! dest-v i (car p))
          (vector-set! src-v i (cdr p)))
        (loop (add1 i))))
    (let loop ([j upto])
      (when (< j len)
        (vector-set! dest-v j (vector-ref src-v j))
        (loop (add1 j))))))

(define (vector-map f src-v)
  (let* ([len (vector-length src-v)]
         [dest-v (make-vector len)])
    (let loop ([i 0])
      (when (< i len)
        (vector-set! dest-v i (f (vector-ref src-v i)))
        (loop (add1 i))))
    dest-v))
