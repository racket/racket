(module mzscheme-core mzscheme
  ;(require (all-except mzscheme provide module if require letrec null?)
           ;(lib "list.ss"))
  (require-for-syntax (lib "struct.ss" "frtime") (lib "list.ss"))
  (require (lib "list.ss")
           (lib "frp-core.ss" "frtime")
           (rename (lib "lang-ext.ss" "frtime") lift lift)
           (rename (lib "lang-ext.ss" "frtime") new-cell new-cell))


  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Fundamental Macros ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define-syntax frp:letrec
    (syntax-rules ()
      [(_ ([id val] ...) expr ...)
       (let ([id (new-cell)] ...)
         (let ([tmp val])
           (if (signal? tmp)
               (set-cell! id tmp)
               (set! id tmp)))
         ...
         expr ...)]))
  
  ;(define-syntax frp:match
  ;  (syntax-rules ()
  ;    [(_ expr clause ...) (lift #t (match-lambda clause ...) expr)]))
  
  (define (->boolean x)
    (if x #t #f))
  
  (define-syntax frp:if
    (syntax-rules ()
      [(_ test-exp then-exp)
       (frp:if test-exp then-exp (void))]
      [(_ test-exp then-exp else-exp)
       (frp:if test-exp then-exp else-exp undefined)]
      [(_ test-exp then-exp else-exp undef-exp)
       (super-lift
        (lambda (b)
          ;(printf "~n\t******\tIF CONDITION IS ~a~n" b)
          (cond
            [(undefined? b) undef-exp]
            [b then-exp]
            [else else-exp]))
        (lift #t ->boolean test-exp))]))
  
  (define (copy-list lst)
    (frp:if (null? lst)
            ()
            (frp:cons (frp:car lst) (copy-list (frp:cdr lst)))))
  
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
                 (let ([#,the-rest-arg (copy-list #,the-rest-arg)])
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
  #|
  (define (split-list acc lst)
    (if (null? (cdr lst))
        (values acc lst)
        (split-list (append acc (list (car lst))) (cdr lst))))
  
  (define (frp:apply fn . args)
    (let-values ([(first-args rest-args) (split-list () args)])
      (if (behavior? rest-args)
          (super-lift
           (lambda (rest-args)
             (apply apply fn (append first-args rest-args)))
           args)
          (apply apply fn (append first-args rest-args)))))
  |#
  
  
  ;;;;;;;;;;;;;;;;
  ;; Structures ;;
  ;;;;;;;;;;;;;;;;
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONS
  
  
  (define (frp:cons f r)
    (if (or (behavior? f) (behavior? r))
        (procs->signal:compound
         cons
         (lambda (p i)
           (if (zero? i)
               (lambda (v) (set-car! p v))
               (lambda (v) (set-cdr! p v))))
         f r)
        (cons f r)))
  
  (define (make-accessor acc)
    (lambda (v)
      (let loop ([v v])
        (cond
          [(signal:compound? v) (acc (signal:compound-content v))]
          [(signal:switching? v) (super-lift
                                  (lambda (_)
                                    (loop (unbox (signal:switching-current v))))
                                  (signal:switching-trigger v))]
          [(signal? v) #;(printf "access to ~a in ~a~n" acc
                                 (value-now/no-copy v))
                       (lift #t acc v)]
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
  
  (define frp:append
    (case-lambda
      [() ()]
      [(lst) lst]
      [(lst1 lst2 . lsts)
       (frp:if (frp:empty? lst1)
               (apply frp:append lst2 lsts)
               (frp:cons (frp:car lst1)
                         (apply frp:append (frp:cdr lst1) lst2 lsts)))]))
  
  (define frp:list
    (lambda elts
     (frp:if (frp:empty? elts)
              '()
              (frp:cons (frp:car elts)
                        (apply frp:list (frp:cdr elts))))))
  
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
          ;        (let ([ctnt (value-now itm)])
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
  
  
  (define (frp:vector . args)
    (if (ormap behavior? args)
        (apply procs->signal:compound
               vector
               (lambda (vec idx)
                 (lambda (x)
                   (vector-set! vec idx x)))
               args)        
        (apply vector args)))
  
  (define (frp:vector-ref v i)
    (cond
      [(signal:compound? v) (vector-ref (signal:compound-content v) i)]
      [(signal? v) (lift #t vector-ref v i)]
      [else (vector-ref v i)]))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make-struct-type + define-struct Macros
  
  
  (define (frp:make-struct-type name-symbol super-struct-type init-field-k auto-field-k . args)
    (let-values ([(desc ctor pred acc mut)
                  (apply make-struct-type name-symbol super-struct-type init-field-k auto-field-k
                         args)])
      (values
       desc
       (lambda fields
         (if (ormap behavior? fields)
             (apply procs->signal:compound
                    ctor
                    (lambda (strct idx)
                      (lambda (val)
                        (mut strct idx val)))
                    fields)
             (apply ctor fields)))
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
               (parameterize ([current-inspector insp])
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
             (syntax-case c (lifted lifted:nonstrict as-is:unchecked as-is frlibs)
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
               [(frlibs str ...)
                #'(begin
                    clause ...
                    (require (lib str "frtime") ...))]
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
           (rename frp:if if)
           (rename frp:lambda lambda)
           (rename frp:case-lambda case-lambda)
           ;(rename frp:apply apply)
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
           (rename frp:require require)))
