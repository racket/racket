#lang scheme/base
(require scheme/dict scheme/match scheme/function "define.ss"
         (for-syntax scheme/base scheme/list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HIGHER ORDER TOOLS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Automatic case-lambda repetition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (split-syntax-at orig stx id)
  (let loop ([found #f]
             [seen null]
             [stx stx])
    (syntax-case stx []
      [(head . tail)
       (and (identifier? #'head)
            (free-identifier=? #'head id))
       (if found
           (raise-syntax-error
            #f
            (format "duplicate occurrence of ~a" (syntax-e id))
            orig
            #'head)
           (loop (list (reverse seen) #'head #'tail)
                 (cons #'head seen)
                 #'tail))]
      [(head . tail) (loop found (cons #'head seen) #'tail)]
      [_ found])))

(define-for-syntax (expand-ellipsis-clause stx pattern expr)
  (cond
   [(split-syntax-at stx pattern #'(... ...))
    =>
    (lambda (found)
      (syntax-case found [...]
        [([pre ... repeat] (... ...) [count post ... . tail])
         (and (identifier? #'repeat)
              (exact-nonnegative-integer? (syntax-e #'count)))
         (build-list
          (add1 (syntax-e #'count))
          (lambda (i)
            (with-syntax ([(var ...)
                           (generate-temporaries
                            (build-list i (lambda (j) #'repeat)))]
                          [body expr])
              (list
               (syntax/loc pattern (pre ... var ... post ... . tail))
               (syntax/loc expr
                 (let-syntax ([the-body
                               (lambda _
                                 (with-syntax ([(repeat (... ...)) #'(var ...)])
                                   #'body))])
                   the-body))))))]
        [(pre mid post)
         (raise-syntax-error
          #f
          "expected ellipsis between identifier and natural number literal"
          stx
          #'mid)]))]
   [else (list (list pattern expr))]))

(define-syntax (case-lambda* stx)
  (syntax-case stx []
    [(_ [pattern body] ...)
     (with-syntax ([([pattern body] ...)
                    (append-map
                     (lambda (p e) (expand-ellipsis-clause stx p e))
                     (syntax->list #'(pattern ...))
                     (syntax->list #'(body ...)))])
       (syntax/loc stx
         (case-lambda [pattern body] ...)))]))

(define-syntax (make-intermediate-procedure stx)
  (syntax-case stx [quote]
    [(_ (quote name) positional-clause ... #:keyword keyword-clause)
     (syntax/loc stx
       (make-keyword-procedure
        (let* ([name (case-lambda keyword-clause)]) name)
        (let* ([name (case-lambda* positional-clause ...)]) name)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Degenerate Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identity x) x)

(define-if-unbound (const v)
  (make-intermediate-procedure
   'constant-function
   [(x ... 8) v]
   [xs v]
   #:keyword
   [(ks vs . xs) v]))

(define-syntax (thunk stx)
  (syntax-case stx ()
    [(thunk body ...)
     (syntax/loc stx
       (make-keyword-thunk (lambda () body ...)))]))

(define (make-keyword-thunk f)
  (make-intermediate-procedure
   'thunk-function
    [(x ... 8) (f)]
    [xs (f)]
    #:keyword
    [(ks vs . xs) (f)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Higher-Order Boolean Operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conjoin
  (case-lambda*
   [(f ... 8)
    (make-intermediate-procedure
     'conjoined
      [(x (... ...) 8) (and (f x (... ...)) ...)]
      [xs (and (apply f xs) ...)]
      #:keyword
      [(keys vals . args)
       (and (keyword-apply f keys vals args) ...)])]
   [fs
    (make-intermediate-procedure
     'conjoined
     [(x ... 8) (andmap (lambda (f) (f x ...)) fs)]
     [xs (andmap (lambda (f) (apply f xs)) fs)]
     #:keyword
     [(keys vals . args)
      (andmap (lambda (f) (keyword-apply f keys vals args)) fs)])]))

(define disjoin
  (case-lambda*
   [(f ... 8)
    (make-intermediate-procedure
     'disjoined
      [(x (... ...) 8) (or (f x (... ...)) ...)]
      [xs (or (apply f xs) ...)]
      #:keyword
      [(keys vals . args)
       (or (keyword-apply f keys vals args) ...)])]
   [fs
    (make-intermediate-procedure
     'disjoined
     [(x ... 8) (ormap (lambda (f) (f x ...)) fs)]
     [xs (ormap (lambda (f) (apply f xs)) fs)]
     #:keyword
     [(keys vals . args)
      (ormap (lambda (f) (keyword-apply f keys vals args)) fs)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Function Invocation (partial or indirect)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (cons2 one two rest)
  (let*-values ([(ones twos) rest])
    (values (cons one ones) (cons two twos))))

(define merge-keywords
  (match-lambda*
    [(or (list _ '() '() keys vals)
         (list _ keys vals '() '()))
     (values keys vals)]
    [(list name
           (and keys1* (cons key1 keys1)) (and vals1* (cons val1 vals1))
           (and keys2* (cons key2 keys2)) (and vals2* (cons val2 vals2)))
     (cond
      [(keyword<? key1 key2)
       (cons2 key1 val1 (merge-keywords name keys1 vals1 keys2* vals2*))]
      [(keyword<? key2 key1)
       (cons2 key2 val2 (merge-keywords name keys1* vals1* keys2 vals2))]
      [else
       (error name
              "duplicate values for ~s: ~s and ~s"
              key1 val1 val2)])]))

(define curryn
  (make-intermediate-procedure
   'curryn
   [(n f x ... 8)
    (if (<= n 0)
        (f x ...)
        (make-intermediate-procedure
         'curried
         [(y (... ...) 8) (curryn (sub1 n) f x ... y (... ...))]
         [ys (curryn (sub1 n) f x ... ys)]
         #:keyword
         [(ks vs . ys)
          (keyword-apply curryn ks vs (sub1 n) f x ... ys)]))]
   [(n f . xs)
    (if (<= n 0)
        (apply f xs)
        (make-intermediate-procedure
         'curried
         [ys (apply curryn (sub1 n) f (append xs ys))]
         #:keyword
         [(ks vs . ys)
          (keyword-apply curryn ks vs (sub1 n) f (append xs ys))]))]
   #:keyword
   [(ks vs n f . xs)
    (if (<= n 0)
        (keyword-apply f ks vs xs)
        (make-intermediate-procedure
         'curried
         [ys (keyword-apply curryn ks vs (sub1 n) f (append xs ys))]
         #:keyword
         [(ks* vs* . ys)
          (let*-values ([(keys vals) (merge-keywords 'curryn ks vs ks* vs*)])
            (keyword-apply curryn keys vals (sub1 n) f (append xs ys)))]))]))

(define currynr
  (make-intermediate-procedure
   'currynr
   [(n f x ... 8)
    (if (<= n 0)
        (f x ...)
        (make-intermediate-procedure
         'curried
         [(y (... ...) 8) (currynr (sub1 n) f y (... ...) x ...)]
         [ys (currynr (sub1 n) f (append ys (list x ...)))]
         #:keyword
         [(ks vs . ys)
          (keyword-apply currynr ks vs (sub1 n) f (append ys (list x ...)))]))]
   [(n f . xs)
    (if (<= n 0)
        (apply f xs)
        (make-intermediate-procedure
         'curried
         [ys (apply currynr (sub1 n) f (append ys xs))]
         #:keyword
         [(ks vs . ys)
          (keyword-apply currynr ks vs (sub1 n) f (append ys xs))]))]
   #:keyword
   [(ks vs n f . xs)
    (if (<= n 0)
        (keyword-apply f ks vs xs)
        (make-intermediate-procedure
         'curried
         [ys (keyword-apply currynr ks vs (sub1 n) f (append ys xs))]
         #:keyword
         [(ks* vs* . ys)
          (let*-values ([(keys vals) (merge-keywords 'currynr ks vs ks* vs*)])
            (keyword-apply currynr keys vals (sub1 n) f (append ys xs)))]))]))

(define papply
  (make-intermediate-procedure
   'papply
   [(f x ... 8)
    (make-intermediate-procedure
     'partially-applied
     [(y (... ...) 8) (f x ... y (... ...))]
     [ys (apply f x ... ys)]
     #:keyword
     [(ks vs . ys) (keyword-apply f ks vs x ... ys)])]
   [(f . xs)
    (make-intermediate-procedure
     'partially-applied
     [ys (apply f (append xs ys))]
     #:keyword
     [(ks vs . ys) (keyword-apply f ks vs (append xs ys))])]
   #:keyword
   [(ks vs f . xs)
    (make-intermediate-procedure
     'partially-applied
     [ys (keyword-apply f ks vs (append xs ys))]
     #:keyword
     [(ks* vs* . ys)
      (let*-values ([(keys vals) (merge-keywords 'papply ks vs ks* vs*)])
        (keyword-apply f keys vals (append xs ys)))])]))

(define papplyr
  (make-intermediate-procedure
   'papplyr
   [(f x ... 8)
    (make-intermediate-procedure
     'partially-applied
     [(y (... ...) 8) (f y (... ...) x ...)]
     [ys (apply f (append ys (list x ...)))]
     #:keyword
     [(ks vs . ys) (keyword-apply f ks vs (append ys (list x ...)))])]
   [(f . xs)
    (make-intermediate-procedure
     'partially-applied
     [ys (apply f (append ys xs))]
     #:keyword
     [(ks vs . ys) (keyword-apply f ks vs (append ys xs))])]
   #:keyword
   [(ks vs f . xs)
    (make-intermediate-procedure
     'partially-applied
     [ys (keyword-apply f ks vs (append ys xs))]
     #:keyword
     [(ks* vs* . ys)
      (let*-values ([(keys vals) (merge-keywords 'papplyr ks vs ks* vs*)])
        (keyword-apply f keys vals (append ys xs)))])]))

(define call
  (make-intermediate-procedure
   'call
   [(f x ... 8) (f x ...)]
   [(f . xs) (apply f xs)]
   #:keyword
   [(ks vs f . xs) (keyword-apply f ks vs xs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eta expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax eta*
  (syntax-rules ()
    [(_ f arg ...) (lambda (arg ...) (f arg ...))]
    [(_ f arg ... . rest) (lambda (arg ... . rest) (apply f arg ... rest))]))

(define-syntax-rule (eta f) (make-eta-expansion (lambda () f)))

(define (make-eta-expansion f*)
  (make-intermediate-procedure
   'eta
   [(x ... 8) ((f*) x ...)]
   [xs (apply (f*) xs)]
   #:keyword
   [(ks vs . xs) (keyword-apply (f*) ks vs xs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameter arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (strip-param orig p-arg)
  (syntax-case p-arg ()
    [(id #:param param)
     (values (syntax/loc p-arg (id (param)))
             (syntax/loc p-arg [param id]))]
    [_ (values p-arg #f)]))

(define-for-syntax (strip-params orig p-args)
  (syntax-case p-args ()
    [(key p-arg . rest)
     (keyword? #'key)
     (let*-values ([(arg param) (strip-param orig #'p-arg)]
                   [(args params) (strip-params orig #'rest)])
       (values (cons #'key (cons arg args))
               (if param (cons param params) params)))]
    [(p-arg . rest)
     (let*-values ([(arg param) (strip-param orig #'p-arg)]
                   [(args params) (strip-params orig #'rest)])
       (values (cons arg args)
               (if param (cons param params) params)))]
    [_ (values p-args null)]))

(define-syntax (lambda/parameter stx)
  (syntax-case stx ()
    [(_ p-args . body)
     (let*-values ([(args params) (strip-params stx #'p-args)])
       (quasisyntax/loc stx
         (lambda #,args (parameterize #,params . body))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ;; functions
 identity
 thunk const
 negate conjoin disjoin
 curryn currynr papply papplyr call
 ;; macros
 eta eta*
 lambda/parameter)
