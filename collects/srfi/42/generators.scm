;;;
;;; NORMAL GENERATORS
;;;


(module generators mzscheme
  (provide (all-defined))
  (require "expansion.scm")
  (require-for-syntax "expansion.scm")
  (require-for-template mzscheme)
  
  
  (define-generator :list
    (lambda (form-stx)
      (syntax-case form-stx (index)
        [(_ var (index i) expr ...)
         (add-index form-stx #'(_ var expr ...) #'i)]
        [(_ var expr1 expr2 expr ...)
         ; TODO IMPROVE: something better than append ?
         #'(_ var (append expr1 expr2 expr ...))]
        [(_ var expr)
         (begin
           (unless (identifier? #'var)
             (raise-syntax-error ':list "expected identifier, got " #'var))
           ; (ob* oc* lb* ne1 ib* ic* ne2 ls*)
           (make-loop  #'(() () ((xs expr)) (not (null? xs))
                             (((var) (car xs))) () #t ((cdr xs)))))]
        [_ (raise-syntax-error 
            ':list 
            "Expected either (:list <expr> ...) or (:list (index <var>) expr ...), got: "
            form-stx)])))
  
  (define-generator (:integers form-stx)
    (syntax-case form-stx (index)
      [(_ var (index i))
       ; (ob* oc* lb* ne1 ib* ic* ne2 ls*)
       (make-loop  #'(() () ((var 0)) #t (((i) var)) () #t ((add1 var))))]
      [(_ var)
       ; (ob* oc* lb* ne1 ib* ic* ne2 ls*)
       (make-loop  #'(() () ((var 0)) #t () () #t ((add1 var))))]
      [_
       (raise-syntax-error
        ':integers
        "expected (:integers <var> (index <var>)) where (index <var>) is optional, got: "
        form-stx)]))
  
  
  (define (ec-:vector-filter vecs)
    ; filter zero-length vectors
    (if (null? vecs)
        '()
        (if (zero? (vector-length (car vecs)))
            (ec-:vector-filter (cdr vecs))
            (cons (car vecs) (ec-:vector-filter (cdr vecs))) )))
  
  ; The expansion below uses name-append to turn
  ;       (:name var expr1 expr2 ...)
  ; into
  ;       (:name var (name-append expr1 expr2 ...))
  
  ; If your indexed sequence doesn't have an append operation
  ; (or it is too expensive to use) then use
  ; define-indexed-generator instead.
  
  (define-syntax (define-indexed-generator-with-append stx)
    (syntax-case stx ()
      [(__ :name (name? name-ref name-length name-append name-type))
       #'(define-generator (:name form-stx)
           (syntax-case form-stx (index)
             [(_ var (index i) expr (... ...))
              (add-index form-stx #'(:name var expr (... ...)) #'i)]
             [(_ var expr)
              (begin
                (unless (identifier? #'var)
                  (raise-syntax-error
                   ':name
                   "expected a variable to bind"
                   #'var))
                #'(:do (let ((seq expr) (len 0))
                         (set! len (name-length seq)))
                       ((i 0))
                       (< i len)
                       (let ((var (name-ref seq i))))
                       #t
                       ((+ i 1)) ))]
             [(_ var expr (... ...))
              #`(:name var (let ([es (list expr (... ...))])
                             (unless (andmap name? es)
                               (error
                                ':name 
                                (format "expected ~as, but got: ~~a " name-type)
                                es))
                             ; TODO: use raise-syntax-error above (how?)
                             (apply name-append es)))]
             [_
              (raise-syntax-error
               ':name
               (format "expected (~a <var> (index i) <expr> <expr> ...) where (index i) is optional, got: "
                       ':name)
               form-stx)]))]))
  
  (define-indexed-generator-with-append :string 
    (string? string-ref string-length string-append "string"))
  
  (define-indexed-generator-with-append :bytes 
    (bytes? bytes-ref bytes-length bytes-append "byte-string"))
  
  ; The expansion below basically turns 
  ;       (:name var expr1 expr2 ...)
  ; into (nested (: xs (list expr1 expr2 ...)
  ;              (:name var xs))
  ; except we need to write it as a do-loop.
  
  (define-syntax (define-indexed-generator-without-append stx)
    (syntax-case stx ()
      [(__ :name (name? name-ref name-length name-type))
       #'(define-generator (:name form-stx)
           (syntax-case form-stx (index)
             [(_ var (index i) expr (... ...))
              (add-index form-stx #'(:name var expr (... ...)) #'i)]
             [(_ var expr)
              (begin
                (unless (identifier? #'var)
                  (raise-syntax-error
                   ':name
                   "expected a variable to bind"
                   #'var))
                #`(:do (let ((seq expr) (len 0))
                         (set! len (name-length seq)))
                       ((i 0))
                       (< i len)
                       (let ((var #,(syntax/loc form-stx (name-ref seq i)))))
                       #t
                       ((+ i 1)) ))]
             [(_ var expr (... ...))
              #'(:do (let ([es ; filter zero-length sequences away
                            (let lp ([es (list expr (... ...))])
                              (cond
                                [(null? es)                     '()]
                                [(zero? (name-length (car es))) (lp (cdr es))]
                                [else (cons (car es) (lp (cdr es)))]))]
                           [current        #f]
                           [current-length 0]))
                     ((k 0))
                     (if (< k current-length)
                         #t
                         (if (null? es)
                             #f
                             (begin (set! current (car es))
                                    (set! es (cdr es))
                                    (set! current-length (name-length current))
                                    (set! k 0)
                                    #t)))
                     (let ((var (name-ref current k))))
                     #t
                     ((+ k 1)))]
             [_
              (raise-syntax-error
               ':name
               (format "expected (~a <var> (index i) <expr> <expr> ...) where (index i) is optional, got: "
                       ':name)
               form-stx)]))]))
  
  (define-indexed-generator-without-append :vector
    (vector? vector-ref vector-length  "vector"))
  
  
  (define-generator (:range form-stx)
    (syntax-case form-stx (index)
      ; handle index variable and add optional args
      ((:range var (index i) arg1 arg ...)
       (add-index form-stx #'(:range var arg1 arg ...) #'i))
      ((:range var arg1)
       #'(:range var 0 arg1 1) )
      ((:range var arg1 arg2)
       #'(:range var arg1 arg2 1) )
      
      ; special cases (partially evaluated by hand from general case)
      ((:range var 0 arg2 1)
       #'(:do (let ((b arg2))
                (if (not (and (integer? b) (exact? b)))
                    (error 
                     "arguments of :range are not exact integer "
                     "(use :real-range?)" 0 b 1 )))
              ((var 0))
              (< var b)
              (let ())
              #t
              ((+ var 1)) ))
      
      ((:range var 0 arg2 -1)
       #'(:do (let ((b arg2))
                (if (not (and (integer? b) (exact? b)))
                    (error 
                     "arguments of :range are not exact integer "
                     "(use :real-range?)" 0 b 1 )))
              ((var 0))
              (> var b)
              (let ())
              #t
              ((- var 1)) ))
      
      ((:range var arg1 arg2 1)
       #'(:do (let ((a arg1) (b arg2))
                (if (not (and (integer? a) (exact? a)
                              (integer? b) (exact? b) ))
                    (error 
                     "arguments of :range are not exact integer "
                     "(use :real-range?)" a b 1 )) )
              ((var a))
              (< var b)
              (let ())
              #t
              ((+ var 1)) ))
      
      ((:range var arg1 arg2 -1)
       #'(:do (let ((a arg1) (b arg2) (s -1) (stop 0))
                (if (not (and (integer? a) (exact? a)
                              (integer? b) (exact? b) ))
                    (error 
                     "arguments of :range are not exact integer "
                     "(use :real-range?)" a b -1 )) )
              ((var a))
              (> var b)
              (let ())
              #t
              ((- var 1)) ))
      
      ; the general case
      
      ((:range var arg1 arg2 arg3)
       #'(:do (let ((a arg1) (b arg2) (s arg3) (stop 0))
                (if (not (and (integer? a) (exact? a)
                              (integer? b) (exact? b)
                              (integer? s) (exact? s) ))
                    (error 
                     "arguments of :range are not exact integer "
                     "(use :real-range?)" a b s ))
                (if (zero? s)
                    (error "step size must not be zero in :range") )
                (set! stop (+ a (* (max 0 (ceiling (/ (- b a) s))) s))) )
              ((var a))
              (not (= var stop))
              (let ())
              #t
              ((+ var s)) ))))
  
  
  (define-generator (:real-range form-stx)
    (syntax-case form-stx (index)
      ; add optional args and index variable
      ((:real-range var arg1)
       #'(:real-range var (index i) 0 arg1 1) )
      ((:real-range var (index i) arg1)
       #'(:real-range var (index i) 0 arg1 1) )
      ((:real-range var arg1 arg2)
       #'(:real-range var (index i) arg1 arg2 1) )
      ((:real-range var (index i) arg1 arg2)
       #'(:real-range var (index i) arg1 arg2 1) )
      ((:real-range var arg1 arg2 arg3)
       #'(:real-range var (index i) arg1 arg2 arg3) )
      
      ; the fully qualified case
      ((:real-range var (index i) arg1 arg2 arg3)
       #'(:do (let ((a arg1) (b arg2) (s arg3) (istop 0))
                (if (not (and (real? a) (real? b) (real? s)))
                    (error "arguments of :real-range are not real" a b s) )
                (if (and (exact? a) (or (not (exact? b)) (not (exact? s))))
                    (set! a (exact->inexact a)) )
                (set! istop (/ (- b a) s)) )
              ((i 0))
              (< i istop)
              (let ((var (+ a (* s i)))))
              #t
              ((+ i 1)) ))))
  
  ; Comment: The macro :real-range adapts the exactness of the start
  ;   value in case any of the other values is inexact. This is a
  ;   precaution to avoid (list-ec (: x 0 3.0) x) => '(0 1.0 2.0).
  
  (define-generator (:char-range form-stx)
    (syntax-case form-stx (index)
      [(_ var (index i) expr1 expr2)
       (add-index form-stx #'(:char-range var expr1 expr2) #'i)]
      [(_ var expr1 expr2)
       #'(:do (let ((imax (char->integer expr2))))
              ((i (char->integer expr1)))
              (<= i imax)
              (let ((var (integer->char i))))
              #t
              ((+ i 1)) )]
      [_
       (raise-syntax-error
        ':char-range
        "expected (:char-range <var> (index <var>) <expr> <expr>) where the index is optional, got: "
        form-stx)]))
  
  (define-generator (:port form-stx)
    (syntax-case form-stx (index)
      ((:port var (index i) arg1 arg ...)
       (add-index form-stx #'(:port var arg1 arg ...) #'i))
      ((:port var arg)
       #'(:port var arg read) )
      ((:port var arg1 arg2)
       #'(:do (let ((port arg1) (read-proc arg2)))
              ((var (read-proc port)))
              (not (eof-object? var))
              (let ())
              #t 
              ((read-proc port)) ))
      (_
       (raise-syntax-error
        ':port
        "expected (:port <var> (index i) <reader-expr>) where index is optional, and the <reader-expr> defaults to read, got:"
        form-stx))))
  
  
  ;;;
  ;;; SPECIAL GENERATORS
  ;;;
  
  (define-generator (:let form-stx)
    (syntax-case form-stx (index)
      ; ($ loop ob* oc* lb* ne1 ib* ic* ne2 ls*)
      [(_ var (index i) expr)
       (make-loop #'(() () ((var expr) (i 0)) #t () () #f ()))]
      [(_ var expr)
       (make-loop #'(() () ((var expr)) #t () () #f ()))]
      [_ 
       (raise-syntax-error 
        ':let "expected (:let <var> <expr>) or (:let <var> (index <var>) <expr>), got:" 
        form-stx)]))
  
  (require-for-syntax (lib "match.ss"))
  
  (define-generator (:parallel form-stx)
    ; TODO: Check that all subforms are generators
    (syntax-case form-stx (index)
      [(_ (index i) q ...)
       (add-index form-stx #'(_ q ...) #'i)]
      [(_ gen)
       (generator->loop #'gen)]
      [(_ gen1 gen2)
       (syntax-case (list (loop-stx (generator->loop #'gen1))
                          (loop-stx (generator->loop #'gen2))) ()
         [(((ob ...) (oc ...) (lb ...) ne1 (ib ...) (ic ...) ne2 (ls ...))
           ((ob2 ...) (oc2 ...) (lb2 ...) ne12 (ib2 ...) (ic2 ...) ne22 (ls2 ...)))
          (make-loop
           #'((ob ... ob2 ...) 
              (oc ... oc2 ...)  (lb ... lb2 ...)
              (and ne1 ne12)    (ib ... ib2 ...)
              (ic ... ic2 ...) (and ne2 ne22) 
              (ls ... ls2 ...)))])]
      [(_ gen1 gen2 gen3 ...)
       #'(:parallel (:parallel gen1 gen2) gen3 ...)]))
  
  (define-generator (:until form-stx)
    (syntax-case form-stx (index)
      [(_ gen test-expr)
       (unless (generator-clause? #'gen)
         (raise-syntax-error
          ':until "expected <generator> in " #'gen))
       (syntax-case (loop-stx (generator->loop #'gen)) ()
         [(obs ocs lbs ne1 ibs ics ne2 lss)
          (make-loop #'(obs ocs lbs ne1 ibs ics 
                            (and ne2 (not test-expr))
                            lss))])]
      [_
       (raise-syntax-error 
        ':until "expected (:until <generator> <expression>), got: "
        form-stx)]))
  
  
  (define-generator (:do form-stx)
    (syntax-case form-stx (let let-values)
      ; short form -> fill in default value
      [(_ ((lv le) ...) ne1? (expr ...))
       #'(:do (let ()) ((lv le) ...) ne1? (let ()) #t (expr ...))]
      ; convert (let _) variants to (let-values _)
      [(_   (let        ((on oe) ...) <oc> ...)       (<lb> ...) <ne1?> (let        ((in ie) ...)       <ic> ...) <ne2?> (<ls> ...))
       #'(_ (let-values (((on) oe) ...) <oc> ...)     (<lb> ...) <ne1?> (let-values ([(in) ie] ...)     <ic> ...) <ne2?> (<ls> ...))]
      [(_   (let        ((on oe) ...) <oc> ...)       (<lb> ...) <ne1?> ilet                                      <ne2?> (<ls> ...))
       #'(_ (let-values (((on) oe) ...) <oc> ...)     (<lb> ...) <ne1?> ilet                                      <ne2?> (<ls> ...))]
      [(_   olet                                      (<lb> ...) <ne1?> (let        ((in ie) ...)       <ic> ...) <ne2?> (<ls> ...))
       #'(_ olet                                      (<lb> ...) <ne1?> (let-values ([(in) ie] ...)     <ic> ...) <ne2?> (<ls> ...))]
      
      ; now both outer bindings and inner bindings must be let-values bindings
      [(_ olet lbs ne1? ilet ne2? lss)
       (begin
         ; check syntax of subforms
         (syntax-case #'olet (let-values)
           [(let-values (((i ...) e) ...) oc ...) 'ok]
           [_ (raise-syntax-error 
               ':do (string-append "expected (let ((<id> <expr>) ...) <command> ...)  or\n"
                                   "(let-values ([(<id> ...) <expr>] ...) <command> ...) , got ")
               #'olet)])
         (syntax-case #'ilet (let-values)
           [(let-values (((i ...) e) ...) ic ...) 'ok]
           [_ (raise-syntax-error 
               ':do (string-append "expected (let ((<id> <expr>) ...) <command> ...) or\n"
                                   "(let-values ([(<id> ...) <expr>] ...) <command> ...), got ")
               #'ilet)])
         (syntax-case #'lbs ()
           [((i b) ...)
            (for-each (lambda (i) 
                        (unless (identifier? i)
                          (raise-syntax-error ':do "expected an identifier, got: " i)))
                      (syntax->list #'(i ...)))]
           [_ (raise-syntax-error 
               ':do
               "expected loop bindings of the form ((<id> <expr>) ...), got: " #'lbs)])
         (syntax-case #'lss ()
           [(expr ...) 'ok]
           [_ (raise-syntax-error "expected loop steppers: (<expr> ...), got: " #'lss)])
         ; everything is ok
         (syntax-case form-stx (let-values)
           [(_ (let-values ([(oi ...) oe] ...) oc ...) lbs ne1? (let-values ([(ii ...) ie] ...) ic ...) ne2? lss)
            ; (ob* oc* lb* ne1 ib* ic* ne2 ls*)
            (make-loop #'((((oi ...) oe) ...) (oc ...) lbs ne1? (((ii ...) ie) ...) (ic ...) ne2? lss))]))]
      [_
       (raise-syntax-error
        ':do 
        "TODO fix message: expected (:do (let ((<id> <expr>) ...) <cmd> ...) <ne1?> (let ((<id> <expr>) ...) <cmd> ...) (<expr> ...)), got "
        form-stx)]))
  
  (define-generator (:while form-stx)
    (syntax-case form-stx ()
      [(_ gen test)
       (begin
         (unless (generator-clause? #'gen)
           (raise-syntax-error
            ':while "expected a generator clause, got: " #'gen))
         (let ([loop (generator->loop #'gen)])
           (syntax-case (loop-stx loop) ()
             [((ob ...) (oc ...) (lb ...) ne1 (((ib-var ...) ib-rhs) ...) (ic ...) ne2 (ls ...))
              (with-syntax ([(ib-tmp ...) (generate-temporaries #'(ib-var ... ...))]
                            [(false ...)  (map (lambda (x) #'f) (syntax->list #'(ib-var ... ...)))])
                ; this trickery is necessary to make ib-vars visible in test
                (make-loop #'((ob ... ((ib-tmp) #f) ...)
                              (oc ...)
                              (lb ...)
                              (let ([ne1-val ne1])
                                (and ne1-val
                                     (let-values ([(ib-var ...) ib-rhs] ...)
                                       ic ...
                                       (set! ib-tmp ib-var) ... ...
                                       (and ne1-val test))))
                              (((ib-var ...) ib-tmp) ...)
                              ()
                              ne2
                              (ls ...))))])))]
      [_ 
       (raise-syntax-error
        ':while
        "expected (:while <generator> <expr>) got: "
        form-stx)]))
  
  )
