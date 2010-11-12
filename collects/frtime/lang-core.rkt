(module lang-core mzscheme
  (require-for-syntax frtime/struct mzlib/list)
  (require mzlib/list
           frtime/core/frp
           (only srfi/43/vector-lib vector-any)
           (only frtime/lang-ext lift new-cell switch ==> changes deep-value-now)
           (only mzlib/etc build-vector rec build-list opt-lambda identity))
  
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
            ()
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
    (opt-lambda (obj [mem empty])
      (with-handlers ((exn:fail?
                       (lambda (e)
                         (fprintf
                          (current-error-port)
                          "you've encountered a bug in frtime.  please send a report to the Racket mailing list.\nexn: ~a\n"
                          e) #f)))
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
       (case (hash-table-get deps obj 'absent)
         [(absent) (hash-table-put! deps obj 'new)]
         [(old)    (hash-table-put! deps obj 'alive)]
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
    (opt-lambda (lst [mem empty])
      (cond
        [(memq lst mem) #f]
        [(behavior? lst) #t]
        [(cons? lst) (any-spinal-reactivity? (cdr lst) (cons lst mem))]
        [else #f])))
  
  (define (deep-cdr-value-now/update-deps obj deps table)
    (cond
      [(behavior? obj)
       (case (hash-table-get deps obj 'absent)
         [(absent) (hash-table-put! deps obj 'new)]
         [(old)    (hash-table-put! deps obj 'alive)]
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
           (let ([deps (make-hash-table)])
             (lambda ()
               (begin0
                 (deep-cdr-value-now/update-deps obj deps empty)
                 (hash-table-for-each
                  deps
                  (lambda (k v)
                    (case v
                      [(new)   (hash-table-put! deps k 'old)
                               (register rtn k)
                               (do-in-manager
                                (iq-enqueue rtn))]
                      [(alive) (hash-table-put! deps k 'old)]
                      [(old)   (hash-table-remove! deps k)
                               (unregister rtn k)])))))))
          (do-in-manager
           (iq-enqueue rtn))
          rtn)
        obj))
  
  (define (raise-reactivity obj)
    (let ([rtn (proc->signal void)])
      (set-signal-thunk!
       rtn
       (let ([deps (make-hash-table)])
         (lambda ()
           (begin0
             (deep-value-now/update-deps obj deps empty)
             (hash-table-for-each
              deps
              (lambda (k v)
                (case v
                  [(new)   (hash-table-put! deps k 'old)
                           (register rtn k)]
                  [(alive) (hash-table-put! deps k 'old)]
                  [(old)   (hash-table-remove! deps k)
                           (unregister rtn k)])))))))
      (do-in-manager
       (iq-enqueue rtn))
      rtn))
  
  (define (compound-lift proc)
    (let ([rtn (proc->signal void)])
      (set-signal-thunk!
       rtn
       (let ([deps (make-hash-table)])
         (lambda ()
           (begin0
             (let/ec esc
               (begin0
                 (proc (lambda (obj)
                         (if (behavior? obj)
                             (begin
                               (case (hash-table-get deps obj 'absent)
                                 [(absent) (hash-table-put! deps obj 'new)
                                           (let ([o-depth (signal-depth rtn)])
                                             (register rtn obj)
                                             (when (> (signal-depth rtn) o-depth)
                                               (iq-enqueue rtn)
                                               (esc #f)))]
                                 [(old)    (hash-table-put! deps obj 'alive)]
                                 [(new)    (void)])
                               (value-now obj))
                             obj)));)
                 (hash-table-for-each
                  deps
                  (lambda (k v)
                    (case v
                      [(new alive) (hash-table-put! deps k 'old)]
                      [(old)   (hash-table-remove! deps k)
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
      [() ()]
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
    (if (procedure? val)
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
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift #t fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [(lifted:nonstrict . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                       (apply lift #f fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [provide-spec
                (syntax (begin clause ... (provide provide-spec)))])]))
        (syntax (begin))
        (syntax->list (syntax clauses)))]))
  
  (define-syntax (frp:require stx)
    (define (generate-temporaries/loc st ids)
      (map (lambda (id)
             (datum->syntax-object stx (syntax-object->datum id)))
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
                      (require (rename module tmp-name fun-name) ...)
                      (define (fun-name . args)
                        (apply lift #f tmp-name args))
                      ...))]
               [(lifted module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename module tmp-name fun-name) ...)
                      (define (fun-name . args)
                        (apply lift #t tmp-name args))
                      ...))]
               [(as-is:unchecked module id ...)
                (syntax (begin clause ... (require (rename module id id) ...)))]
               [(as-is module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...) (generate-temporaries/loc stx #'ids)])
                  #'(begin
                      clause ...
                      (require (rename module tmp-name fun-name) ...)
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
           raise-reactivity
           raise-list-for-apply
           (rename public-dvn deep-value-now)
           any-nested-reactivity?
           compound-lift
           list-match
           (rename frp:if if)
           (rename frp:lambda lambda)
           (rename frp:case-lambda case-lambda)
           (rename frp:letrec letrec)
           (rename frp:cons cons)
           (rename frp:car car)
           (rename frp:cdr cdr)
           (rename frp:list list)
           (rename frp:list? list?)
           (rename frp:list* list*)
           (rename frp:null? null?)
           (rename frp:pair? pair?)
           (rename frp:append append)
           (rename frp:vector vector)
           (rename frp:vector-ref vector-ref)
           (rename frp:make-struct-type make-struct-type)
           (rename frp:make-struct-field-accessor make-struct-field-accessor)
           (rename frp:make-struct-field-mutator make-struct-field-mutator)
           (rename frp:define-struct define-struct)
           (rename frp:provide provide)
           (rename frp:require require)
           frp:copy-list
           frp:->boolean))
