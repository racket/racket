#lang racket

(require compiler/zo-parse
         compiler/zo-marshal
         "impl-eval.rkt")

(provide (all-defined-out)
         (all-from-out "impl-eval.rkt"))

(define (compile-bytecode expr)
  (let ([tmp (make-temporary-file)])
    (call-with-output-file tmp #:exists 'truncate
      (λ (p) 
        (parameterize ([current-namespace (make-base-namespace)])
          (write (compile expr) p))))
    (begin0
      (call-with-input-file tmp zo-parse)
      (delete-file tmp))))

(define (impl->model expr)
  (define cycled (cycle-points expr))
  (define text-addr (make-hasheq))
  (define next-loc
    (let ([suffix 0])
      (λ ()
        (set! suffix (add1 suffix))
        (string->symbol (format "x~s" suffix)))))
  (define text-seg '())
  (cons 
   (let recur ([e expr])
     (match e
       [(compilation-top _ _ e)
        (recur e)]
       [(localref #f n #f #t #f)
        `(loc ,n)]
       [(localref #f n #t _ #f)
        `(loc-clr ,n)]
       [(localref #f n #f #f #f)
        `(loc-noclr ,n)]
       [(localref #t n #f #t #f)
        `(loc-box ,n)]
       [(localref #t n #t _ #f)
        `(loc-box-clr ,n)]
       [(localref #t n #f #f #f)
        `(loc-box-noclr ,n)]
       [(let-one r b #f #f)
        `(let-one ,(recur r) ,(recur b))]
       [(let-void n #f b)
        `(let-void ,n ,(recur b))]
       [(let-void n #t b)
        `(let-void-box ,n ,(recur b))]
       [(install-value 1 n #f r b)
        `(install-value ,n ,(recur r) ,(recur b))]
       [(install-value 1 n #t r b)
        `(install-value-box ,n ,(recur r) ,(recur b))]
       [(boxenv n b)
        `(boxenv ,n ,(recur b))]
       [(application f as)
        `(application ,(recur f) ,@(map recur as))]
       [(seq es)
        `(seq ,@(map recur es))]
       [(branch c t e)
        `(branch ,(recur c) ,(recur t) ,(recur e))]
       [(let-rec rs b)
        `(let-rec ,(map recur rs) ,(recur b))]
       [(lam _ _ _ τs #f ns `(val/ref ...) _ _ b)
        `(lam ,τs ,(vector->list ns) ,(recur b))]
       [(closure l _)
        (define (model-rep)
          (match-let ([`(lam ,τs () ,b) (recur l)])
            `(proc-const ,τs ,b)))
        (if (hash-ref cycled e #f)
            `(indirect ,(let ([x (hash-ref text-addr e #f)])
                          (or x
                              (let ([x (next-loc)])
                                (hash-set! text-addr e x)
                                (set! text-seg (cons (list x (model-rep)) text-seg))
                                x))))
            (model-rep))]
       [(case-lam _ ls)
        `(case-lam ,@(map recur ls))]
       [(? void?) 'void]
       [(? number?) e]
       [(? boolean?) e]
       [(? symbol?) `',e]
       [_ (error 'impl->model "unrecognized form ~s" e)]))
   text-seg))

(define (cycle-points expr)
  (define seen (make-hasheq))
  (let recur ([e expr])
    (when (zo? e) ; i.e., not a literal
      (if (hash-ref seen e #f)
          (unless (closure? e)
            (error 'cycle-refs "illegal cycle through ~s" e))
          (begin
            (hash-set! seen e #t)
            (match e
              [(compilation-top _ _ e)
               (recur e)]
              [(localref _ _ _ _ _)
               (void)]
              [(let-one r b _ _)
               (recur r)
               (recur b)]
              [(let-void _ _ b)
               (recur b)]
              [(install-value _ _ _ r b)
               (recur r)
               (recur b)]
              [(boxenv _ b)
               (recur b)]
              [(application f as)
               (recur f) 
               (for-each recur as)]
              [(seq es)
               (for-each recur es)]
              [(branch c t e)
               (recur c)
               (recur t)
               (recur e)]
              [(let-rec rs b)
               (for-each recur rs) 
               (recur b)]
              [(lam _ _ _ _ _ _ _ _ _ b)
               (recur b)]
              [(closure l _)
               (recur l)]
              [(case-lam _ ls)
               (for-each recur ls)]
              [_ (void)])))))
  seen)

(define (model->impl expr [cycles '()])
  (let ([cycle-places (make-immutable-hash
                       (map (match-lambda
                              [(cons x _) 
                               (cons x (make-placeholder #f))])
                            cycles))])
    (letrec ([recur (λ (e)
                      (match e
                        [`(loc ,n) (cons (localref #f n #f #t #f) 0)]
                        [`(loc-clr ,n) (cons (localref #f n #t #t #f) 0)]
                        [`(loc-noclr ,n) (cons (localref #f n #f #f #f) 0)]
                        [`(loc-box ,n) (cons (localref #t n #f #t #f) 0)]
                        [`(loc-box-clr ,n) (cons (localref #t n #t #t #f) 0)]
                        [`(loc-box-noclr ,n) (cons (localref #t n #f #f #f) 0)]
                        [`(let-one ,r ,b)
                         (match-let ([(cons rr dr) (recur r)]
                                     [(cons rb db) (recur b)])
                           (cons (let-one rr rb #f #f)
                                 (add1 (max dr db))))]
                        [`(,(and (or 'let-void 'let-void-box) i) ,n ,b)
                         (match-let ([(cons rb db) (recur b)])
                           (let ([b? (match i ['let-void #f] ['let-void-box #t])])
                             (cons (let-void n b? rb) 
                                   (+ n db))))]
                        [`(,(and (or 'install-value 'install-value-box) i) ,n ,r ,b)
                         (match-let ([(cons rr dr) (recur r)]
                                     [(cons rb db) (recur b)])
                           (let ([b? (match i ['install-value #f] ['install-value-box #t])])
                             (cons (install-value 1 n b? rr rb)
                                   (max dr db))))]
                        [`(boxenv ,n ,e)
                         (match-let ([(cons re de) (recur e)])
                           (cons (boxenv n re) de))]
                        [`(application ,f . ,as)
                         (match-let ([`((,rf . ,df) (,ras . ,das) ...)
                                      (map recur (cons f as))])
                           (cons (application rf ras)
                                 (+ (length as) (apply max df das))))]
                        [`(seq ,e ,es ...)
                         (match-let ([`((,re . ,de) (,res . ,des) ...)
                                      (map recur (cons e es))])
                           (cons (seq (cons re res))
                                 (apply max de des)))]
                        [`(branch ,c ,t ,f)
                         (match-let ([(cons rc dc) (recur c)]
                                     [(cons rt dt) (recur t)]
                                     [(cons rf df) (recur f)])
                           (cons (branch rc rt rf)
                                 (max dc dt df)))]
                        [`(let-rec (,rs ...) ,b)
                         (match-let ([`((,rrs . 0) ...) (map recur rs)]
                                     [(cons rb db) (recur b)])
                           (cons (let-rec rrs rb) db))]
                        [`(lam (,τs ...) (,ns ...) ,b)
                         (match-let ([(cons rb db) (recur b)])
                           (cons (lam (gensym) '()
                                      (length τs) τs #f
                                      (list->vector ns) (for/list ([_ ns]) 'val/ref)
                                      #f (+ (length τs) (length ns) db) rb)
                                 0))]
                        [`(proc-const (,τs ...) ,b)
                         (match-let ([(cons re 0) (recur `(lam ,τs () ,b))])
                           (cons (closure re (gensym)) 0))]
                        [`(case-lam ,cs ...)
                         (match-let ([`((,rcs . 0) ...) (map recur cs)])
                           (cons (case-lam (gensym) rcs) 0))]
                        [`(indirect ,x)
                         (cons (hash-ref cycle-places x) 0)]
                        ['void (cons (void) 0)]
                        [else (cons e 0)]))])
      (match-let ([(cons rep-expr dep-expr) (recur expr)])
        (for-each
         (match-lambda
           [(list x e)
            (placeholder-set! (hash-ref cycle-places x) (car (recur e)))])
         cycles)
        (make-reader-graph
         (compilation-top dep-expr (prefix 0 '() '()) rep-expr))))))
