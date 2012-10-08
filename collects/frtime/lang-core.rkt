(module lang-core racket
  (require (for-syntax (only-in frtime/struct build-struct-names build-struct-generation build-struct-expand-info)
                      (only-in racket/base foldl)
                      (only-in racket/list empty)))
  (require (only-in racket/list cons? first second rest empty empty?)
           (only-in racket/base vector-ref build-vector build-list)
           (only-in racket/function identity)
           (only-in frtime/core/frp super-lift undefined undefined? behavior? do-in-manager-after do-in-manager proc->signal set-signal-thunk! register unregister iq-enqueue value-now/no-copy
                 signal? signal-depth signal:switching? signal-value value-now signal:compound? signal:compound-content signal:switching-current signal:switching-trigger set-cell!)
           (only-in frtime/lang-ext lift new-cell switch ==> changes deep-value-now))
  
  #| (VECTOR-ANY <pred?> <vector>) -> value
  ;;;   Apply PRED? to each element in VECTOR ...; if PRED?
  ;;;   should ever return a true value, immediately stop and return that
  ;;;   value; otherwise, when the vector runs out, return #f.
  ;;;   The iteration and order of application of PRED? across elements
  ;;;   is of the vectors is strictly left-to-right. Definition of this function taken from srfi/43/vector-lib. |#
  (define vector-any
    (letrec ((loop1 (lambda (pred? vec i len)
                      (and (not (= i len))
                           (or (pred? (vector-ref vec i))
                               (loop1 pred? vec (add1 i) len))))))
      (lambda (pred? vec)
        (loop1 pred? vec 0 (vector-length vec)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Fundamental Macros ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define-syntax frp:letrec
    (syntax-rules ()
      [(_ ([id val] ...) expr ...)
       (let ([id (new-cell)] ...)
         (let ([tmp val])
           (when (or (signal? tmp) (any-nested-reactivity? tmp))
             (set-cell! id tmp))
           (set! id tmp))
         ...
         expr ...)]))
  
  (define (->boolean x)
    (if x #t #f))
  
  (define (frp:->boolean x)
    (lift #t ->boolean x))
  
  (define-syntax frp:if
    (syntax-rules ()
      [(_ test-exp then-exp)
       (frp:if test-exp then-exp (void))]
      [(_ test-exp then-exp else-exp)
       (frp:if test-exp then-exp else-exp undefined)]
      [(_ test-exp then-exp else-exp undef-exp)
       (super-lift
        (lambda (b)
          (cond
            [(undefined? b) undef-exp]
            [b then-exp]
            [else else-exp]))
        (frp:->boolean test-exp))]))
  
  (define (frp:copy-list lst)
    (frp:if (null? lst)
            '()
            (frp:cons (frp:car lst) (frp:copy-list (frp:cdr lst)))))
  
  (define-syntax frp:let-values
    (syntax-rules ()
      [(_ ([vars expr] ...) body0 body1 ...)
       (let-values ([vars (split-multiple expr)] ...)
         body0 body1 ...)]))
  
  (define-for-syntax (get-rest-arg arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       arglist-stx]
      [(var ...)
       #f]
      [(var . others)
       (get-rest-arg #'others)]))
  
  (define-for-syntax (translate-clause stx)
    (syntax-case stx ()
      [(bindings body0 body1 ...)
       (let ([the-rest-arg (get-rest-arg #'bindings)])
         (if the-rest-arg
             #`(bindings
                (let ([#,the-rest-arg (frp:copy-list #,the-rest-arg)])
                  body0 body1 ...))
             #'(bindings body0 body1 ...)))]))
  
  (define-syntax (frp:lambda stx)
    (syntax-case stx ()
      [(_ bindings body0 body1 ...)
       (with-syntax ([new-clause (translate-clause #'(bindings body0 body1 ...))])
         #'(lambda . new-clause))]))
  
  (define-syntax (frp:case-lambda stx)
    (syntax-case stx ()
      [(_ clause ...)
       (with-syntax ([(new-clause ...)
                      (map translate-clause (syntax->list #'(clause ...)))])
         #'(case-lambda
             new-clause ...))]))
  
  (define any-nested-reactivity?
    (lambda (obj [mem empty])
      (with-handlers ((exn:fail?
                       (lambda (e)
                         (eprintf
                          "you've encountered a bug in frtime.  please send a report to the Racket mailing list.\nexn: ~a\n"
                          e)
                         #f)))
        (cond
          [(memq obj mem) #f]
          [(behavior? obj) #t]
          [(cons? obj)
           (let ([mem (cons obj mem)])
             (or (any-nested-reactivity? (car obj) mem)
                 (any-nested-reactivity? (cdr obj) mem)))]
          [(struct? obj)
           (let*-values ([(info skipped) (struct-info obj)]
                         [(name init-k auto-k acc mut immut sup skipped?) (struct-type-info info)]
                         [(ctor) (struct-type-make-constructor info)])
             (ormap (lambda (i) (any-nested-reactivity? (acc obj i) (cons obj mem)))
                    (build-list init-k (lambda (x) x))))]
          [(vector? obj) (vector-any (lambda (o) (any-nested-reactivity? o (cons obj mem))) obj)]
          [else #f]))))
  
  (define (deep-value-now/update-deps obj deps table)
    (cond
      [(assq obj table) => second]
      [(behavior? obj)
       (case (hash-ref deps obj 'absent)
         [(absent) (hash-set! deps obj 'new)]
         [(old)    (hash-set! deps obj 'alive)]
         [(new)    (void)])
       (deep-value-now/update-deps (signal-value obj) deps
                                   (cons (list obj (signal-value obj)) table))]
      [(cons? obj)
       (let* ([result (cons #f #f)]
              [new-table (cons (list obj result) table)]
              [car-val (deep-value-now/update-deps (car obj) deps new-table)]
              [cdr-val (deep-value-now/update-deps (cdr obj) deps new-table)])
         (if (and (eq? car-val (car obj))
                  (eq? cdr-val (cdr obj)))
             obj
             (cons car-val cdr-val)))]
      ; won't work in the presence of super structs or immutable fields
      [(struct? obj)
       (let*-values ([(info skipped) (struct-info obj)]
                     [(name init-k auto-k acc mut! immut sup skipped?) (struct-type-info info)]
                     [(ctor) (struct-type-make-constructor info)]
                     [(indices) (build-list init-k identity)]
                     [(result) (apply ctor (build-list init-k (lambda (i) #f)))]
                     [(new-table) (cons (list obj result) table)]
                     [(elts) (build-list init-k (lambda (i) (deep-value-now/update-deps (acc obj i) deps new-table)))])
         (if (andmap (lambda (i e) (eq? (acc obj i) e)) indices elts)
             obj
             (begin
               (for-each (lambda (i e) (mut! result i e)) indices elts)
               result)))]
      [(vector? obj)
       (let* ([len (vector-length obj)]
              [indices (build-list len identity)]
              [result (build-vector len (lambda (_) #f))]
              [new-table (cons (list obj result) table)]
              [elts (build-list len (lambda (i) (deep-value-now/update-deps (vector-ref obj i) deps new-table)))])
         (if (andmap (lambda (i e) (eq? (vector-ref obj i) e)) indices elts)
             obj
             (begin
               (for-each (lambda (i e) (vector-set! result i e)) indices elts)
               result)))]
      [else obj]))
  
  (define (public-dvn obj)
    (do-in-manager-after
     (deep-value-now obj empty)))
  
  (define any-spinal-reactivity?
    (lambda (lst [mem empty])
      (cond
        [(memq lst mem) #f]
        [(behavior? lst) #t]
        [(cons? lst) (any-spinal-reactivity? (cdr lst) (cons lst mem))]
        [else #f])))
  
  (define (deep-cdr-value-now/update-deps obj deps table)
    (cond
      [(behavior? obj)
       (case (hash-ref deps obj 'absent)
         [(absent) (hash-set! deps obj 'new)]
         [(old)    (hash-set! deps obj 'alive)]
         [(new)    (void)])
       (deep-cdr-value-now/update-deps (signal-value obj) deps table)]
      [(cons? obj)
       (let* ([cdr-val (deep-cdr-value-now/update-deps (cdr obj) deps table)])
         (cons (car obj) cdr-val))]
      [else obj]))
  
  (define (raise-list-for-apply obj)
    (if (any-spinal-reactivity? obj)
        (let ([rtn (proc->signal void)])
          (set-signal-thunk!
           rtn
           (let ([deps (make-hasheq)])
             (lambda ()
               (begin0
                 (deep-cdr-value-now/update-deps obj deps empty)
                 (hash-for-each
                  deps
                  (lambda (k v)
                    (case v
                      [(new)   (hash-set! deps k 'old)
                               (register rtn k)
                               (do-in-manager
                                (iq-enqueue rtn))]
                      [(alive) (hash-set! deps k 'old)]
                      [(old)   (hash-remove! deps k)
                               (unregister rtn k)])))))))
          (do-in-manager
           (iq-enqueue rtn))
          rtn)
        obj))
  
  (define (raise-reactivity obj)
    (let ([rtn (proc->signal void)])
      (set-signal-thunk!
       rtn
       (let ([deps (make-hasheq)])
         (lambda ()
           (begin0
             (deep-value-now/update-deps obj deps empty)
             (hash-for-each
              deps
              (lambda (k v)
                (case v
                  [(new)   (hash-set! deps k 'old)
                           (register rtn k)]
                  [(alive) (hash-set! deps k 'old)]
                  [(old)   (hash-remove! deps k)
                           (unregister rtn k)])))))))
      (do-in-manager
       (iq-enqueue rtn))
      rtn))
  
  (define (compound-lift proc)
    (let ([rtn (proc->signal void)])
      (set-signal-thunk!
       rtn
       (let ([deps (make-hasheq)])
         (lambda ()
           (begin0
             (let/ec esc
               (begin0
                 (proc (lambda (obj)
                         (if (behavior? obj)
                             (begin
                               (case (hash-ref deps obj 'absent)
                                 [(absent) (hash-set! deps obj 'new)
                                           (let ([o-depth (signal-depth rtn)])
                                             (register rtn obj)
                                             (when (> (signal-depth rtn) o-depth)
                                               (iq-enqueue rtn)
                                               (esc #f)))]
                                 [(old)    (hash-set! deps obj 'alive)]
                                 [(new)    (void)])
                               (value-now obj))
                             obj)));)
                 (hash-for-each
                  deps
                  (lambda (k v)
                    (case v
                      [(new alive) (hash-set! deps k 'old)]
                      [(old)   (hash-remove! deps k)
                               (unregister rtn k)])))))))))
      (iq-enqueue rtn)
      rtn))
  
  ;;;;;;;;;;;;;;;;
  ;; Structures ;;
  ;;;;;;;;;;;;;;;;
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONS
  
  
  (define frp:cons cons)
  
  (define (make-accessor acc)
    (lambda (v)
      (let loop ([v v])
        (cond
          [(signal:compound? v) (acc (signal:compound-content v))]
          [(signal? v) (super-lift acc v)]
          [(signal:switching? v) (super-lift
                                  (lambda (_)
                                    (loop (unbox (signal:switching-current v))))
                                  (signal:switching-trigger v))]
          [(undefined? v) undefined]
          [else (acc v)]))))
  
  (define frp:car
    (make-accessor car))
  
  (define frp:cdr
    (make-accessor cdr))
  
  (define frp:pair? (lambda (arg) (if (signal:compound? arg)
                                      (pair? (signal:compound-content arg))
                                      (lift #t pair? arg))))
  
  (define (frp:null? arg)
    (if (signal:compound? arg)
        #f
        (lift #t null? arg)))
  
  (define frp:empty? frp:null?)
  
  (define (list-match lst cf ef)
    (super-lift
     (lambda (lst)
       (cond
         [(undefined? lst) undefined]
         [(pair? lst) (cf (first lst) (rest lst))]
         [(empty? lst) (ef)]
         [else (error "list-match: expected a list, got ~a" lst)]))
     lst))
  
  (define frp:append
    (case-lambda
      [() '()]
      [(lst) lst]
      [(lst1 lst2 . lsts)
       (list-match lst1
                   (lambda (f r) (cons f (apply frp:append r lst2 lsts)))
                   (lambda () (apply frp:append lst2 lsts)))]))
  
  (define frp:list list)
  
  (define frp:list*
    (lambda elts
      (frp:if (frp:empty? elts)
              '()
              (frp:if (frp:empty? (frp:cdr elts))
                      (frp:car elts)
                      (frp:cons (frp:car elts)
                                (apply frp:list* (frp:cdr elts)))))))
  
  (define (frp:list? itm)
    (if (signal:compound? itm)
        (let ([ctnt (signal:compound-content itm)])
          (if (cons? ctnt)
              (frp:list? (cdr ctnt))
              #f))
        (if (signal? itm)
            (frp:if (lift #t cons? itm)
                    (frp:list? (frp:cdr itm))
                    (frp:null? itm))
            (or (null? itm)
                (and (cons? itm) (frp:list? (cdr itm)))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vector
  
  
  (define frp:vector vector)
  
  (define (frp:vector-ref v i)
    (cond
      [(behavior? v) (super-lift (lambda (v) (frp:vector-ref v i)) v)]
      [else (lift #t vector-ref v i)]))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make-struct-type + define-struct Macros
  
  
  (define (frp:make-struct-type name-symbol super-struct-type init-field-k auto-field-k . args)
    (let-values ([(desc ctor pred acc mut)
                  (apply make-struct-type name-symbol super-struct-type init-field-k auto-field-k
                         args)])
      (values
       desc
       ctor
       (lambda (v) (if (signal:compound? v)
                       (pred (value-now/no-copy v))
                       (lift #t pred v)))
       acc
       mut)))
  
  (define (frp:make-struct-field-accessor acc i sym)
    (make-accessor (make-struct-field-accessor acc i sym)))
  
  ; FORBIDS MUTATION
  (define (frp:make-struct-field-mutator acc i sym)
    (lambda (s _)
      (error "MUTATION NOT ALLOWED IN FrTime STRUCTURES")))
  
  (define-syntax (frp:define-struct stx)
    (syntax-case stx ()
      [(_ (s t) (field ...) insp)
       #;(define-struct (s t) (field ...) (make-inspector insp))
       (let ([field-names (syntax->list #'(field ...))]
             [super-for-gen (if (syntax-e #'t)
                                (string->symbol
                                 (format "struct:~a" (syntax-e #'t)))
                                #f)]
             [super-for-exp (if (syntax-e #'t)
                                #'t
                                #t)])
         #`(begin
             (define-values #,(build-struct-names #'s field-names #f #f stx)
               (parameterize ([current-inspector (make-inspector insp)])
                 #,(build-struct-generation #'s field-names #f #f super-for-gen)))
             (define-syntax s
               #,(build-struct-expand-info #'s field-names #f #f super-for-exp
                                           empty empty))))]
      [(_ (s t) (field ...))
       #'(frp:define-struct (s t) (field ...) (current-inspector))]
      [(_ s (field ...) insp)
       #'(frp:define-struct (s #f) (field ...) insp)]
      [(_ s (field ...))
       #'(frp:define-struct (s #f) (field ...) (current-inspector))]))
  
  (define (find pred lst)
    (cond
      [(empty? lst) #f]
      [(pred (first lst)) (first lst)]
      [else (find pred (rest lst))]))
  
  
  (define (ensure-no-signal-args val name)
    (when (procedure? val)
        (lambda args
          (cond
            [(find signal? args)
             =>
             (lambda (v)
               (raise-type-error name "non-signal"
                                 (format "#<signal: ~a>" (signal-value v))))]
            [else (apply val args)]))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Provide & Require ;;
  ;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define-syntax (frp:provide stx)
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted:nonstrict)
               [(lifted . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax stx (syntax->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift #t fun-name args))
                     ...
                     (provide (rename-out [tmp-name fun-name]) ...))))]
               [(lifted:nonstrict . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax stx (syntax->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift #f fun-name args))
                     ...
                     (provide (rename-out [tmp-name fun-name]) ...))))]
               [provide-spec
                (syntax (begin clause ... (provide provide-spec)))])]))
        (syntax (begin))
        (syntax->list (syntax clauses)))]))
  
  (define-syntax (frp:require stx)
    (define (generate-temporaries/loc st ids)
      (map (lambda (id)
             (datum->syntax stx (syntax->datum id)))
           (generate-temporaries ids)))
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted:nonstrict as-is:unchecked as-is)
               [(lifted:nonstrict module . ids)
                (with-syntax ([(fun-name ...) #'ids]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename-in module [fun-name tmp-name]) ...)
                      (define (fun-name . args)
                        (apply lift #f tmp-name args))
                      ...))]
               [(lifted module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename-in module [fun-name tmp-name]) ...)
                      (define (fun-name . args)
                        (apply lift #t tmp-name args))
                      ...))]
               [(as-is:unchecked module id ...)
                (syntax (begin clause ... (require (rename-in module [id id]) ...)))]
               [(as-is module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename-in module [fun-name tmp-name]) ...)
                      (define fun-name (ensure-no-signal-args tmp-name 'fun-name))
                      ...))]
               [require-spec
                #'(begin clause ... (require require-spec))])]))
        #'(begin)
        (syntax->list #'clauses))]))
  
  
  
  
  (provide module
           #%app
           #%top
           #%datum
           #%plain-module-begin
           #%module-begin
           #%top-interaction
           only-in
           except-in
           rename-in
           all-from-out
           all-defined-out
           except-out
           
           raise-reactivity
           raise-list-for-apply 
           any-nested-reactivity?
           compound-lift
           list-match
           frp:copy-list
           frp:->boolean
           
           (rename-out [public-dvn deep-value-now])
           (rename-out [frp:if if])
           (rename-out [frp:lambda lambda])
           (rename-out [frp:case-lambda case-lambda])
           (rename-out [frp:letrec letrec])
           (rename-out [frp:cons cons])
           (rename-out [frp:car car])
           (rename-out [frp:cdr cdr])
           (rename-out [frp:list list])
           (rename-out [frp:list? list?])
           (rename-out [frp:list* list*])
           (rename-out [frp:null? null?])
           (rename-out [frp:pair? pair?])
           (rename-out [frp:append append])
           (rename-out [frp:vector vector])
           (rename-out [frp:vector-ref vector-ref])
           (rename-out [frp:make-struct-type make-struct-type])
           (rename-out [frp:make-struct-field-accessor make-struct-field-accessor])
           (rename-out [frp:make-struct-field-mutator make-struct-field-mutator])
           (rename-out [frp:define-struct define-struct])
           (rename-out [frp:provide provide])
           (rename-out [frp:require require])))
