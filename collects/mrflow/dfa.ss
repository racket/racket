(module dfa (lib "mrflow.ss" "mrflow")
  (require (lib "match.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
           (prefix list: (lib "list.ss"))
           (prefix cst: "constants.ss")
           
           "set-hash.ss"
           "types.ss"
           "util.ss"
           "env.ss"
           
           (lib "class.ss"))
  
  (provide
   (struct dfa (stnum->state))
   
   (struct state (number))
   
   ;; All base types i.e. type-cst, type-empty, struct-type-states are always handle states
   (struct handle-state (handle))
   (struct cons-state (car cdr))
   (struct case-lambda-state (rest-arg?s req-args argss exps))
   (struct promise-state (value))
   (struct struct-value-state (label types))
   (struct union-state (elements))
   (struct values-state (type))
   (struct vector-state (element))
   
   state-number?   
   dfa-state->list
   dfa->list
   
   greatest-handle
   get-ordered-states
   get-state-numbers
   get-states
   get-dfa-size
   
   create-dfa-from-type
   minimize-dfa
   
   ;; debug
   all-handles-referenced?
   )
  
  ;; 
  ;; States
  ;;
  (define state-number? natural?)
  
  (define-struct state (number) (make-inspector))
  
  ;; All base types i.e. type-cst, type-empty, struct-type-states are always handle states
  (define-struct (handle-state state) (handle) (make-inspector))
  (define-struct (cons-state state) (car cdr) (make-inspector))
  ; case-lambda-states has vectors for all fields, in contrast union-states and 
  ;   struct-value-states both use lists.
  (define-struct (case-lambda-state state)
    (rest-arg?s   ; (vectorof boolean)
     req-args     ; (vectorof natural)
     argss        ; (vectorof (vectorof any))
     exps)        ; (vectorof any)
    (make-inspector))
  (define-struct (promise-state state) (value) (make-inspector))
  (define-struct (struct-value-state state) (label types) (make-inspector))
  (define-struct (union-state state) (elements) (make-inspector))
  (define-struct (values-state state) (type) (make-inspector))
  (define-struct (vector-state state) (element) (make-inspector))
  
  ;;
  ;; DFAs
  ;;
  (define-struct dfa (stnum->state canonical-ordering))
  (set! make-dfa
        (let ([old-make-dfa make-dfa])
          (opt-lambda ([ordering #f]) (old-make-dfa (make-hash-table) ordering))))
  
  (define/contract make-ordered-dfa ((listof state?) . -> . dfa?)
    (lambda (states)
      (let* ([dfa (make-dfa)]
             [stnum->state (dfa-stnum->state dfa)]
             [ordered-stnums (map (lambda (state)
                                    (let ([stnum (state-number state)])
                                      (hash-table-put! stnum->state stnum state)
                                      stnum))
                                  states)])
        (set-dfa-canonical-ordering! dfa ordered-stnums)
        dfa)))
  
  (define/contract get-dfa-size (dfa? . -> . natural?)
    (lambda (dfa)
      (let ([size 0])
        (hash-table-for-each (dfa-stnum->state dfa) (lambda (stnum state) (set! size (add1 size))))
        size)))
  
  (define/contract has-state-number? (dfa? state-number? . -> . boolean?)
    (lambda (dfa state-number)
      (if (hash-table-get (dfa-stnum->state dfa) state-number cst:thunk-false) #t #f)))
  
  (define/contract maybe-add-state! (dfa? state? . -> . void?)
    (lambda (dfa state)
      (let ([stnum->state (dfa-stnum->state dfa)]
            [stnum (state-number state)])
        (unless (hash-table-has-key? stnum->state stnum)
          (hash-table-put! stnum->state stnum state)))))
  
  (define/contract lookup (dfa? state-number? . -> . state?)
    (lambda (dfa state-number)
      (hash-table-get (dfa-stnum->state dfa) state-number)))
  
  (define/contract greatest-handle (dfa? . -> . (or/c false/c handle?))
    (lambda (dfa)
      (let ([greatest-handle -1])
        (hash-table-for-each (dfa-stnum->state dfa)
                             (lambda (stnum state)
                               (when (and (handle-state? state) (> (handle-state-handle state) greatest-handle))
                                 (set! greatest-handle (handle-state-handle state)))))
        (if (= greatest-handle -1) #f greatest-handle))))
  
  (define/contract get-ordered-states (dfa? . -> . (listof state?))
    (lambda (dfa)
      (map (lambda (stnum) (lookup dfa stnum)) (dfa-canonical-ordering dfa))))
  
  (define/contract get-states (dfa? . -> . (listof state?))
    (lambda (dfa)
      (hash-table-map (dfa-stnum->state dfa) (lambda (stnum state) state))))
  
  (define/contract get-state-numbers (dfa? . -> . (listof state-number?))
    (lambda (dfa)
      (hash-table-map (dfa-stnum->state dfa) (lambda (stnum state) stnum))))
  
  ;; When this function is called all of the label types in present
  ;; must belong to a strongly connected graph.  This works by first
  ;; annotating all label types with a state number, variables and
  ;; base types are not given a state number. Then the graph is
  ;; traversed again with a type environment. When a rec-type is
  ;; encountered the variable/state bindings are added to the type
  ;; environment. When a variable is encountered its state looked up.
  ;; Labeled states are created w/ the states of their children and
  ;; added to the DFA.
  ;;
  ;; tenv : tvar -> handle ... if have a handle it may not have a state in the dfa
  ;; senv : tvar -> state
  (define/contract create-dfa-from-type
    ((type-rec? tenv?) . ->d* .
                       (lambda (type tenv)
                         (unless (type-var? (type-rec-body type))
                           (error 'create-dfa-from-type
                                  "type-rec should have type-var for body"))
                         (for-each (lambda (type)
                                     (when (type-var? type)
                                       (error 'create-dfa-from-type "DFA has variable on right side of binder"))
                                     (when (and (type-union? type) (andmap type-var? (type-union-elements type)))
                                       (error 'create-dfa-from-type "DFA has union with only variables on right side of binder")))
                                   (type-rec-types type))
                         (values dfa? (listof state-number?))))
    (lambda (type var->handle)
      (let* ([dfa (make-dfa)]
             [annotations (make-hash-table 'equal)]
             [add-annotation!
              (let ([ann -1])
                (lambda (type)
                  (set! ann (add1 ann))
                  (hash-table-put! annotations type ann)))]
             [add-state! (lambda (state)
                           (maybe-add-state! dfa state)
                           (state-number state))]
             [get-annotation
              (lambda (type) (hash-table-get annotations type))]
             [maybe-get-annotation
              (lambda (type) (hash-table-get annotations type cst:thunk-false))])
        (letrec ([annotate
                  (lambda (type)
                    (unless (maybe-get-annotation type)
                      (cond
                        [(handle? type)
                         (add-annotation! type)]
                        [(type-cons? type)
                         (annotate (type-cons-car type))
                         (annotate (type-cons-cdr type))
                         (add-annotation! type)]
                        [(type-case-lambda? type)
                         (for-each-vov (lambda (ty) (annotate ty)) (type-case-lambda-argss type))
                         (for-each-vector (lambda (ty) (annotate ty)) (type-case-lambda-exps type))
                         (add-annotation! type)]
                        [(type-promise? type)
                         (annotate (type-promise-value type))
                         (add-annotation! type)]    
                        [(type-rec? type) 
                         (for-each (lambda (var ty)
                                     (or (maybe-lookup-symbol var->handle (type-var-name var)) (annotate ty)))
                                   (type-rec-vars type) (type-rec-types type))
                         (annotate (type-rec-body type))]
                        [(type-struct-value? type)
                         (for-each annotate (type-struct-value-types type))
                         (add-annotation! type)]
                        [(type-union? type)
                         (for-each annotate (type-union-elements type))
                         (add-annotation! type)]
                        [(type-var? type)
                         (let ([handle (maybe-lookup-symbol var->handle (type-var-name type))])
                           (when handle
                             (add-annotation! type)))]
                        [(type-values? type)
                         (annotate (type-values-type type))
                         (add-annotation! type)]
                        [(type-vector? type)
                         (annotate (type-vector-element type))
                         (add-annotation! type)]
                        [else
                         (error 'create-dfa-from-type "Type ~a should already have been hashconsed" type)])))]
                 [create-dfa
                  (lambda (type var->state)
                    (cond
                      [(handle? type)
                       (add-state! (make-handle-state (get-annotation type) type))]
                      [(type-cons? type)
                       (let* ([hd (create-dfa (type-cons-car type) var->state)]
                              [tl (create-dfa (type-cons-cdr type) var->state)]
                              [state (make-cons-state (get-annotation type) hd tl)])
                         (add-state! state))]
                      [(type-case-lambda? type) ; rest-arg?s req-args argss exps)
                       (let* ([argss (map-vector-of-vector (lambda (type) (create-dfa type var->state)) (type-case-lambda-argss type))]
                              [exps (map-vector (lambda (type) (create-dfa type var->state)) (type-case-lambda-exps type))]
                              [state-number (get-annotation type)]
                              [state (make-case-lambda-state state-number
                                                             (type-case-lambda-rest-arg?s type)
                                                             (type-case-lambda-req-args type)
                                                             argss exps)])
                         (add-state! state)
                         state-number)]
                      [(type-promise? type)
                       (let* ([value (create-dfa (type-promise-value type) var->state)]
                              [state-number (get-annotation type)])
                         (add-state! (make-promise-state state-number value)))]
                      [(type-rec? type) ; vars types body)
                       (let* ([vars (type-rec-vars type)]
                              [types (type-rec-types type)]
                              [body (type-rec-body type)]
                              [binder-states (map (lambda (v ty)
                                                    (or (maybe-get-annotation v)
                                                        (get-annotation ty)))
                                                  vars types)]
                              [new-env (extend-tenv var->state (map type-var-name vars) binder-states)])
                         (for-each (lambda (var type)
                                     (if (maybe-get-annotation var)
                                         (create-dfa var new-env)
                                         (create-dfa type new-env)))
                                   vars types)
                         binder-states)]
                      [(type-struct-value? type) 
                       (let ([types (map (lambda (ty) (create-dfa ty var->state)) (type-struct-value-types type))]
                             [label (type-struct-value-type-label type)])
                         (add-state! (make-struct-value-state (get-annotation type) label types)))]
                      [(type-union? type)
                       (let* ([elements (map (lambda (ty) (create-dfa ty var->state)) (type-union-elements type))])
                         (add-state! (make-union-state (get-annotation type) (min-list-numbers elements))))]
                      [(type-vector? type)
                       (let ([element (create-dfa (type-vector-element type) var->state)])
                         (add-state! (make-vector-state (get-annotation type) element)))]
                      [(type-var? type)
                       (let ([state-number (maybe-get-annotation type)]
                             [name (type-var-name type)])
                         (if state-number
                             (add-state! (make-handle-state state-number (lookup-symbol var->state name)))
                             (lookup-symbol var->state name)))]
                      [(type-values? type)
                       (let ([ty (create-dfa (type-values-type type) var->state)])
                         (add-state! (make-values-state (get-annotation type) ty)))]
                      [else
                       (error 'create-dfa-from-type "Type ~a should already have been hashconsed" type)]))])
          (annotate type)
          (values dfa (create-dfa type (create-tenv)))))))
  
  ;;
  ;; Printing functions
  ;;
  
  (define/contract dfa-state-number->list (opt-> (state-number?) (dfa?) list?)
    (opt-lambda (stnum [dfa #f])
      (dfa-state->list (lookup dfa stnum) dfa)))
  
  (define/contract dfa-state->list (opt-> (state?) (dfa?) list?)
    (opt-lambda (dfa-state [dfa #f])
      (letrec
          ([state->var (make-hash-table)]
           [state->binding (make-hash-table)]
           [get-next-var! (let ([i 0]) (lambda (state)
                                         (let ([str (string->symbol (format "a~a" i))])
                                           (set! i (add1 i))
                                           (hash-table-put! state->var state str)
                                           str)))]
           [statify
            (lambda (sym state) ;((or/c symbol? list?) state-number? . -> . (or/c symbol? list?))
              (let* ([first (if (list? sym) (car sym) sym)]
                     [first (string->symbol
                             (string-append (symbol->string first) ":" (number->string state)))])
                (if (list? sym) (cons-immutable first (cdr sym)) first)))]
           [to-list
            (lambda (dfa-state ancest) ;(state? (listof state-number?) . -> . list?)
              (letrec ([expand
                        (if dfa
                            (lambda (x) (loop x ancest))
                            (lambda (x) x))])
                (match dfa-state
                  [($ handle-state state handle)
                   (list 'handle handle)]
                  [($ cons-state state car cdr)
                   (list 'cons (expand car) (expand cdr))]
                  [($ case-lambda-state state rest-arg?s req-args argss exps)
                   (list 'case-lambda
                         (foldr-case-lambda-vector
                          (lambda (rest-arg? req-arg args exp acc)
                            (cons (list (map expand (vector->list args))
                                        (if rest-arg? '*-> '->)
                                        (expand exp))
                                  acc))
                          null
                          rest-arg?s req-args argss exps))]
                  [($ promise-state state value) 
                   (list 'promise (expand value))]
                  [($ struct-value-state state label types)
                   (list 'struct (map expand types))]
                  [($ union-state state elements)
                   (cons 'union (map expand elements))]
                  [($ values-state state type)
                   (list 'values (expand type))]
                  [($ vector-state state type)
                   (list 'vector (expand type))]
                  [x (error 'dfa-state->string "Unmatched type ~a\n" x)])))]
           [loop (lambda (stnum ancest)
                   (if (memq stnum ancest)
                       (if (hash-table-has-key? state->var stnum)
                           (hash-table-get state->var stnum)
                           (get-next-var! stnum))
                       (let* ([state (lookup dfa stnum)]
                              [l (to-list state (cons stnum ancest))])
                         (if (hash-table-has-key? state->var stnum)
                             (begin
                               (hash-table-put! state->binding stnum (statify l stnum))
                               (hash-table-get state->var stnum))
                             (statify l stnum)))))])
        (if dfa (let ([rec-body (loop (state-number dfa-state) null)]
                      [var-bindings
                       (hash-table-map state->var (lambda (s v)
                                                    (list v (hash-table-get state->binding s))))])
                  (if (null? var-bindings) rec-body
                      (list (statify 'rec-state (state-number dfa-state))
                            var-bindings
                            rec-body)))
            (statify (to-list dfa-state null) (state-number dfa-state))))))
  
  
  (define/contract dfa->list (dfa? . -> . any)
    (lambda (dfa)
      (list
       (list:sort (hash-table-map (dfa-stnum->state dfa) (lambda (k v) (list k '-> (dfa-state->list v dfa))))
                  (lambda (x y) (> (car x) (car y))))
       (dfa-canonical-ordering dfa)
       )))
  
  (define print-dfa
    (lambda (dfa)
      (pretty-display (dfa->list dfa))))
  
  ;;
  ;; Minimization
  ;;
  
  ;; Should equiv-class and block should be merged
  (define-struct equiv-class (type number length classes) (make-inspector))
  
  ;; A non-empty list of dfa-states, representing an equivalence class
  (define block? (listof state?))
  
  ;; A list of disjoint blocks
  (define partition? (listof block?))
  
  (define make-state->equiv-class
    (lambda (num-states)
      (make-vector num-states #f)))
  
  ;; Used only for debugging, make each handle state in the dfa is
  ;; referenced by a label state
  (define all-handles-referenced?
    (lambda (dfa)
      (letrec
          ([tbl (make-hash-table)]
           [seen (make-hash-table)]
           [loop
            (lambda (stnum)
              (unless (hash-table-get seen stnum cst:thunk-false)
                (hash-table-put! seen stnum #t)
                (match (lookup dfa stnum)
                  [($ handle-state state handle)
                   (hash-table-put! tbl handle #t)]
                  [($ cons-state state car cdr)
                   (loop car) (loop cdr)]
                  [($ case-lambda-state state rest-arg?s req-args argss exps)
                   (for-each-vov loop argss)
                   (for-each-vector loop exps)]
                  [($ promise-state state value) 
                   (loop value)]
                  [($ struct-value-state state label types)
                   (for-each loop types)]
                  [($ union-state state elements)
                   (for-each loop elements)]
                  [($ values-state state type)
                   (loop type)]
                  [($ vector-state state type)
                   (loop type)])))]
           [state (car (list:filter (lambda (x) (not (handle-state? x))) (get-ordered-states dfa)))]
           [handle-states (list:filter handle-state? (get-ordered-states dfa))])
        (loop (state-number state))
        (andmap (lambda (hs) (hash-table-get tbl (handle-state-handle hs) cst:thunk-false)) handle-states))))
  
  ; Minimize DFA sets up the partition table and the gross equivalence
  ; classes for hopcrofts algorithm, as well as replacing the states
  ; numbers with their equivalence classes after minimization.
  (define/contract minimize-dfa ((dfa? (listof state-number?)) . ->* .
                                                               (dfa? (listof state-number?)))
    (lambda (dfa original-states)
      (let* ([highest-equiv-class (new counter%)]
             [get-matching-states
              (let ([states (get-states dfa)])
                (lambda (pred)
                  (list:filter pred states)))]
             [state-numbers (get-state-numbers dfa)]
             [stnum->ecnum (make-stnum->ecnum% (apply max state-numbers))]
             [partitions (make-partitions% (get-dfa-size dfa))]
             
             [add-minimum-dfa-state!
              (let ([min '()])
                (lambda (state)
                  (when (member state min)
                    (error 'add-minimum-dfa-state! "Should never add the same state ~a to minimal DFA" state))
                  (set! min (cons-immutable state min))))]
             
             [make-minimized-state
              (match-lambda
                [($ handle-state state handle)
                 (make-handle-state (send stnum->ecnum lookup state) handle)]
                [($ case-lambda-state state  rest-arg?s req-args argss exps)
                 (make-case-lambda-state (send stnum->ecnum lookup state)
                                         rest-arg?s req-args
                                         (for-each-vov! (lambda (arg) (send stnum->ecnum lookup arg)) argss)
                                         (for-each-vector! (lambda (exp) (send stnum->ecnum lookup exp)) exps))]
                [($ cons-state state car cdr)
                 (make-cons-state (send stnum->ecnum lookup state)
                                  (send stnum->ecnum lookup car)
                                  (send stnum->ecnum lookup cdr))]
                [($ promise-state state value)
                 (make-promise-state (send stnum->ecnum lookup state)
                                     (send stnum->ecnum lookup value))]
                [($ struct-value-state state label types)
                 (make-struct-value-state (send stnum->ecnum lookup state)
                                          label
                                          (map (lambda (ty) (send stnum->ecnum lookup ty)) types))]
                [($ union-state state elements)
                 (make-union-state (send stnum->ecnum lookup state)
                                   (min-list-numbers (map (lambda (el)
                                                            (send stnum->ecnum lookup el))
                                                          elements)))]
                [($ values-state state type)
                 (make-values-state (send stnum->ecnum lookup state)
                                    (send stnum->ecnum lookup type))]
                [($ vector-state state element)
                 (make-vector-state (send stnum->ecnum lookup state)
                                    (send stnum->ecnum lookup element))]
                [x (error 'make-minimized-state "Unmatched type ~a" x)])]
             [case-lambda-partition
              (split-case-lambda-states (get-matching-states case-lambda-state?))]
             [struct-value-partition
              (split-struct-value-states (get-matching-states struct-value-state?))]
             [union-partition
              (split-union-states (get-matching-states union-state?))]
             [handle-partition
              (map list (list:sort (get-matching-states handle-state?)
                                   (lambda (x y) (< (handle-state-handle x) (handle-state-handle y)))))]
             [setup-equiv-class
              (lambda (type)
                (lambda(states)
                  (if (null? states) #f
                      (let* ([equiv-class-number (send highest-equiv-class next!)]
                             [equiv-class (make-equiv-class type equiv-class-number (length states) states)])
                        (send partitions place-new-equiv-class equiv-class)
                        (send stnum->ecnum set-states! equiv-class states)
                        equiv-class-number))))]
             
             [handle-partition-numbers (map (setup-equiv-class 'handle) handle-partition)]
             [cl-partition-numbers (map (setup-equiv-class 'case-lambda) case-lambda-partition)]
             [struct-value-numbers (map (setup-equiv-class 'struct-value) struct-value-partition)]
             [union-numbers (map (setup-equiv-class 'union) union-partition)]
             
	     [cons-number ((setup-equiv-class 'cons) (get-matching-states cons-state?))]
             [promise-number ((setup-equiv-class 'promise) (get-matching-states promise-state?))]
             [values-number ((setup-equiv-class 'values) (get-matching-states values-state?))]
             [vector-number ((setup-equiv-class 'vector) (get-matching-states vector-state?))])
        ;; There is no position ordering on the elements of a union so we
        ;; impose one on the equivalence classes of the elements
        (for-each (lambda (block)
                    (for-each (lambda (state)
                                (set-union-state-elements!
                                 state
                                 (list:sort (union-state-elements state)
                                            (lambda (a b)
                                              (> (send stnum->ecnum lookup a)
                                                 (send stnum->ecnum lookup b))))))
                              block))
                  union-partition)
        (hopcroft state-numbers
                  (list:filter cst:id
                               (append handle-partition-numbers cl-partition-numbers struct-value-numbers union-numbers
                                       (list cons-number promise-number values-number vector-number)))
                  partitions stnum->ecnum highest-equiv-class)
	(let* ([_  ; (void)
                ;; ensure unions with only one state when minimized are not added to the dfa
                (send partitions for-each
                      (lambda (partition)
                        (when (and partition (eq? (equiv-class-type partition) 'union))
                          (let* ([block (equiv-class-classes partition)]
                                 [representative (car block)]
                                 [elements (union-state-elements representative)]
                                 [min-stnums (min-list-numbers (map (lambda (stnum)
                                                                      (send stnum->ecnum lookup stnum))
                                                                    elements))])
                            (when (length-one? min-stnums)
                              (for-each (lambda (state)
                                          (send stnum->ecnum set!
                                                (send partitions get-equiv-class (car min-stnums))
                                                state))
                                        block))))))]
               
               [states (send partitions fold
                             (lambda (p acc)
                               (let ([min-state (make-minimized-state (car (equiv-class-classes p)))])
                                 (if (and (union-state? min-state) (length-one? (union-state-elements min-state)))
                                     acc
                                     (cons-immutable min-state acc))))
                             null)]
               [min-binder-states
                (map (lambda (stnum) (send stnum->ecnum lookup stnum)) original-states)]
               [min-dfa (make-ordered-dfa states)]
               [has-useless-union (ormap (lambda (state) (and (union-state? state)
                                                              (length-one? (union-state-elements state))))
                                         states)])
          (if has-useless-union
              (minimize-dfa min-dfa min-binder-states)
              (values min-dfa min-binder-states)))
        )))
  
  (define stnum->ecnum%
    (class object%
      (init-field highest-state)
      
      (define stnum->ecnum (make-vector (add1 highest-state) #f))
      
      (define/public lookup
        (lambda (stnum)
          (vector-ref stnum->ecnum stnum)))
      
      (define/public set!
        (lambda (equiv-class state)
          (vector-set! stnum->ecnum
                       (if (state? state)
                           (state-number state) state)
                       (equiv-class-number equiv-class))))
      
      (define/public set-states!
        (lambda (equiv-class states)
          (for-each (lambda (state)
                      (vector-set! stnum->ecnum
                                   (state-number state)
                                   (equiv-class-number equiv-class)))
                    states)))
      
      (super-new)))
  
  (define make-stnum->ecnum%
    (lambda (k)
      (let ()
        (define/contract stnum->ecnum 
          (object-contract (lookup (natural? . -> . natural?))
                           (set! (equiv-class? (union state? natural?) . -> . void?))
                           (set-states! (equiv-class? (listof state?) . -> . void?)))
          (new stnum->ecnum% (highest-state k)))
        stnum->ecnum)))
  
  (define partitions%
    (class object%
      (init-field number-states)
      
      ; Each element of the partitions table contains either an equivalence class,
      ; or false if it has not been used or the equivalence class it contains has
      ; been split. 
      ;
      ; We split at most num-states - 1 times, but we never reuse the
      ; states in an old partition so allocate twice the number of states.
      (define partitions (make-vector (* 2 number-states) #f))
      
      (define/public place-new-equiv-class
        (lambda (eq-class)
          (vector-set! partitions (equiv-class-number eq-class) eq-class)))
      
      (define/public split
        (lambda (k)
          (vector-set! partitions k #f)))
      
      (define/public get-equiv-class
        (lambda (i) (vector-ref partitions i)))
      
      (define/public fold
        (lambda (f init)
          (foldr-vector (lambda (x acc)
                          (if x (f x acc) acc))
                        init partitions)))
      
      (define/public for-each
        (lambda (f)
          (send this fold (lambda (x acc) (f x)) (void))))
      
      (super-new)
      ))
  
  (define make-partitions%
    (lambda (k)
      (let ()
        (define/contract p 
          (object-contract (place-new-equiv-class (equiv-class? . -> . any))
                           (split (natural? . -> . void?))
                           (get-equiv-class (natural? . -> . equiv-class?))
                           (fold ((equiv-class? any/c . -> . any) any/c . -> . any))
                           (for-each ((equiv-class? . -> . any) . -> . any)))
          (new partitions% (number-states k)))
        p)))
  
  
  
  ; Hopcrofts DFA minimization algorithm. First generate letters for each
  ; partition individually as the have different types and shapes. Next while
  ; there are still letters which may split an equivalence class, try to split
  ; each equivalence class by the letter. Most times the split will fail, but if
  ; it succeeds then replace the old equivalence class with the new split
  ; equivalence classes.  The letter/equiv class pairs will need to be changed
  ; to point to the new equivalence classes
  (define/contract hopcroft
    ((listof natural?) (listof natural?) (is-a?/c partitions%) (is-a?/c stnum->ecnum%) (is-a?/c counter%) . -> . any)
    (lambda (states 
             partition-nums 
             partitions 
             state->equiv-class 
             get-next-equiv-class)
      (if (null? partition-nums) (void)
          (let* ([l (set-make 'equal)]
                 [largest-number-cl-exps -1]
                 [largest-number-cl-args -1]
                 [largest-number-union-elements -1]
                 [largest-number-struct-value-types -1]
                 [_ (send partitions for-each
                          (lambda (ec)
                            (cond [(eq? 'case-lambda (equiv-class-type ec))
                                   (let* ([cl (car (equiv-class-classes ec))]
                                          [argss (case-lambda-state-argss cl)]
                                          [exps (case-lambda-state-exps cl)])
                                     (when (> (vector-length exps) largest-number-cl-exps)
                                       (set! largest-number-cl-exps (vector-length exps)))
                                     (when (> (foldr-vector (lambda (c acc) (max (vector-length c) acc)) -1 argss)
                                              largest-number-cl-args)
                                       (set! largest-number-cl-args (vector-length exps))))]
                                  [(eq? 'union (equiv-class-type ec))
                                   (let* ([union (car (equiv-class-classes ec))]
                                          [len (length (union-state-elements union))])
                                     (when (> len largest-number-union-elements)
                                       (set! largest-number-union-elements len)))]
                                  [(eq? 'struct-value (equiv-class-type ec))
                                   (let* ([struct-value (car (equiv-class-classes ec))]
                                          [len (length (struct-value-state-types struct-value))])
                                     (when (> len largest-number-struct-value-types)
                                       (set! largest-number-struct-value-types len)))])))]
                 [letters
                  (let* ([letters
                          (list:foldr
                           (lambda (equiv-class-num letters)
                             (let* ([equiv-class (send partitions get-equiv-class equiv-class-num)]
                                    [state (car (equiv-class-classes equiv-class))])
                               (cond [(cons-state? state)
                                      (cons '(cons car) (cons '(cons cdr) letters))]
                                     [(promise-state? state)
                                      (cons '(promise) letters)]
                                     [(values-state? state)
                                      (cons '(values) letters)]
                                     [(vector-state? state)
                                      (cons '(vector) letters)]
                                     [else
                                      letters])))
                           '() partition-nums)]
                         [letters   ;; add letters for case-lambda
                          (if (= largest-number-cl-args -1) letters
                              (let ([w-argss
                                     (list:foldr (lambda (row acc)
                                                   (coalesce-lists (list:foldr (lambda (col acc)
                                                                                 (cons (list 'case-lambda 'argss row col) acc))
                                                                               '()
                                                                               (iota largest-number-cl-args))
                                                                   acc))
                                                 letters
                                                 (iota largest-number-cl-exps))])
                                (coalesce-lists
                                 (map (lambda (row) (list 'case-lambda 'exps row)) (iota largest-number-cl-exps))
                                 w-argss)))]
                         [letters ;; add letters for unions
                          (if (= largest-number-union-elements -1) letters
                              ;(unfold-onto (lambda (x) (= x largest-number-union-elements))
                              ;             (lambda (i) (list 'union i))
                              ;             add1
                              ;             0
                              (cons '(union) letters))])
                    (if (= largest-number-struct-value-types -1) letters
                        (coalesce-lists 
                         (map (lambda (i) (list 'struct-value i)) (iota largest-number-struct-value-types))
                         letters)))]
                 ; This is a cheesy way to remove a random element from the set.  XXX ?
                 [get-next! (lambda ()
                              (let ([eq&letter (let/ec return (set-for-each l (lambda (elem) (return elem))) #f)])
                                (when eq&letter
                                  (set-remove l eq&letter))
                                eq&letter))]
                 [add-to-L!
                  (lambda (eq letter)
                    (set-set l (cons eq letter)))]
                 [print-L
                  (lambda ()
                    (printf "(L=") (set-for-each l display)(printf ")"))]
                 [remove! (lambda (eq-class-num letter)
                            (set-remove l (cons eq-class-num letter)))]
                 [number-states (length states)]
                 [eq&letter-present? (lambda (eq-class-num letter)
                                       (set-in? l (cons eq-class-num letter)))])
            (for-each (lambda (eq&letter)
                        (set-set l eq&letter))
                      (cross2 partition-nums letters))
            (let while-letters ([eq&letter (get-next!)] [partition-nums partition-nums])
              (when eq&letter
                (let* ([q1 (car eq&letter)]
                       [a (cdr eq&letter)]
                       [number-partitions 0]
                       [new-partition-nums '()]
                       [add-equiv-class (lambda (ec)
                                          (begin
                                            (set! new-partition-nums (cons ec new-partition-nums))
                                            (set! number-partitions (add1 number-partitions))))])
                  (for-each
                   (lambda (q0)
                     (let-values ([(equiv-class-a equiv-class-b)
                                   (split q0 q1 a partitions state->equiv-class get-next-equiv-class)])
                       (if equiv-class-a ;; when the split is successful
                           (begin
                             (add-equiv-class equiv-class-b)
                             (add-equiv-class equiv-class-a)
                             (for-each (lambda (b)
                                         (if (eq&letter-present? q0 b)
                                             (begin
                                               (remove! q0 b)
                                               (add-to-L! equiv-class-a b)
                                               (add-to-L! equiv-class-b b))
                                             (begin
                                               (add-to-L!
                                                (if (< (equiv-class-length (send partitions get-equiv-class equiv-class-a))
                                                       (equiv-class-length (send partitions get-equiv-class equiv-class-b)))
                                                    equiv-class-a
                                                    equiv-class-b)
                                                b))))
                                       letters))
                           (add-equiv-class q0))))
                   partition-nums)
                  (when (< number-partitions number-states)
                    (while-letters (get-next!) new-partition-nums)))))))))
  
  ;;
  ;; Utility functions
  ;;
  
  (define cross2
    (lambda (xs ys)
      (list:foldl (lambda (x xacc)
                    (list:foldl (lambda (y yacc) (cons-immutable (cons-immutable x y) yacc)) xacc ys))
                  null xs)))
  
  (define coalesce-lists
    (lambda xs
      (letrec ([reverse-onto
                (lambda (xs ys)
                  (if (null? xs) ys
                      (reverse-onto (cdr xs) (cons (car xs) ys))))])
        (if (null? xs) '()
            (list:foldl reverse-onto (car xs) (cdr xs))))))
  
  
  ;;
  ;; Equivalence class utilities
  ;;
  
  (define/contract set-equiv-class-of-state-number! 
    ((vectorof (or/c false/c natural?)) equiv-class? state-number? . -> . void?)
    (lambda (classes equiv-class stnum)
      (vector-set! classes stnum (equiv-class-number equiv-class))))
  
  ;; A function extracting some value from a dfa-state. Discriminators
  ;; are used when comparing two states
  (define discriminator? (state? . -> . (or/c integer? boolean?)))
  
  (define/contract block->partition (block? . -> . partition?)
    list-immutable)
  
  ; split q0 into 2 equivalence classes, those which transitions to q1 from
  ; letter b and those which don't transition to q1
  ;
  ; The 'letter' b depends on the type of partiton we're splitting. A letter
  ; consists of a place within the type E.g. A type-cons letter has a 'position'
  ; indicator of 'car or 'cdr to distinguish which position in a partition of
  ; type-cons we're going to split by.
  (define/contract split
    (natural? natural? list? (is-a?/c partitions%) (is-a?/c stnum->ecnum%) (is-a?/c counter%) . -> . any)
    (lambda (q0-num q1-num b partitions stnum->ecnum highest-equiv-class)
      (let* ([q0 (send partitions get-equiv-class q0-num)]
             [type1 (equiv-class-type q0)]
             [transition-on-letter
              (lambda (q0 b)
                (cond
                  [(eq? b 'handle) #f]
                  [(and (eq? (car b) 'case-lambda)
                        (eq? (cadr b) 'exps))
                   (let ([row (caddr b)])
                     (and (case-lambda-state? q0)
                          (< row (vector-length (case-lambda-state-exps q0)))
                          (send stnum->ecnum lookup
                                (vector-ref (case-lambda-state-exps q0) row))))]
                  [(and (eq? (car b) 'case-lambda) (eq? (cadr b) 'argss))
                   (let ([row (caddr b)]
                         [col (cadddr b)])
                     (and (case-lambda-state? q0)
                          (let ([argss (case-lambda-state-argss q0)])
                            (and (< row (vector-length argss))
                                 (< col (vector-length (vector-ref argss row)))
                                 (send stnum->ecnum lookup
                                       (vector-ref (vector-ref argss row) col))))))]
                  [(eq? 'cons (car b))
                   (let ([pos (cadr b)])
                     (and (cons-state? q0)
                          (send stnum->ecnum lookup
                                ((if (eq? pos 'car) cons-state-car cons-state-cdr) q0))))]
                  [(eq? 'promise (car b))
                   (and (promise-state? q0)
                        (send stnum->ecnum lookup (promise-state-value q0)))]
                  [(eq? 'struct-value (car b))
                   (let ([pos (cadr b)])
                     (and (struct-value-state? q0)
                          (< (length (struct-value-state-types q0)) pos)
                          (send stnum->ecnum lookup (list-ref (struct-value-state-types q0) pos))))]
                  [(eq? 'union (car b))
                   (error 'transition-on-letter "Should have already handled union case")]
                  [(eq? 'values (car b))
                   (and (type-values? q0)
                        (send stnum->ecnum lookup (type-values-type q0)))]
                  [(eq? 'vector (car b))
                   (and (type-vector? q0)
                        (send stnum->ecnum lookup (type-vector-element q0)))]))]
             [any-union-element-transitions-to
              (lambda (q0 q1-num)
                (and (union-state? q0)
                     (ormap (lambda (x)
                              (= (send stnum->ecnum lookup x) q1-num))
                            (union-state-elements q0))))])
        ; this always makes a new list, even if not splittable. probably
        ; faster to switch to new list midway through
        (if (eq? (equiv-class-type q0) (car b)) 
            (let loop ([q0 (equiv-class-classes q0)]
                       [to-q1 '()] [to-q1-length 0]
                       [not-to-q1 '()] [not-to-q1-length 0])
              (cond [(null? q0)
                     (if (and (not (null? to-q1)) (not (null? not-to-q1)))
                         (let* ([to-q1-num (send highest-equiv-class next!)]
                                [to-q1 (make-equiv-class type1 to-q1-num to-q1-length to-q1)]
                                [not-to-q1-num (send highest-equiv-class next!)]
                                [not-to-q1 (make-equiv-class type1 not-to-q1-num
                                                             not-to-q1-length not-to-q1)])
                           (send partitions place-new-equiv-class to-q1)
                           (send partitions place-new-equiv-class not-to-q1)
                           (send stnum->ecnum set-states! to-q1 (equiv-class-classes to-q1))
                           (send stnum->ecnum set-states! not-to-q1 (equiv-class-classes not-to-q1))
                           (send partitions split q0-num)
                           (values to-q1-num not-to-q1-num))
                         (values #f #f))]
                    [(eq? 'union (car b))
                     (if (any-union-element-transitions-to (car q0) q1-num)
                         (loop (cdr q0) (cons (car q0) to-q1) (add1 to-q1-length) not-to-q1 not-to-q1-length)
                         (loop (cdr q0) to-q1 to-q1-length (cons (car q0) not-to-q1) (add1 not-to-q1-length)))]
                    [(eq? (transition-on-letter (car q0) b) q1-num)
                     (loop (cdr q0) (cons (car q0) to-q1) (add1 to-q1-length) not-to-q1 not-to-q1-length)]
                    ;; q0 does not transition on b
                    [else
                     (loop (cdr q0) to-q1 to-q1-length (cons (car q0) not-to-q1) (add1 not-to-q1-length))]))
            (values #f #f)))))
  
  ;; To split a block of dfa-states, use the value projected from a
  ;; dfa-state as a hashtable-key which is associated with the list of
  ;; dfa-states with identical values
  (define/contract split-set (discriminator? block? . -> . partition?)
    (lambda (f xs)
      (if (length-one? xs) (block->partition xs)
          (let ([accs (make-hash-table)])
            (for-each (lambda (x) (hash-table-prepend! accs (f x) x)) xs)
            (let ([keys (hash-table-map accs (lambda (k v) k))])
              (if (null? keys) '()
                  (let* ([gt (cond [(boolean? (car keys))
                                    (lambda (a b) (cond [(eq? a b) #f] [a #f] [b #t]))]
                                   [(integer? (car keys)) >]
                                   [else (error 'split-set "Unknown type ~a" (car keys))])]
                         [keys (list:sort keys gt)])
                    (map (lambda (k) (hash-table-get accs k)) keys))))))))
  
  ;; list list list -> list list
  (define unnest
    (lambda (xsss)   ;; there is probably a better way of doing this, but its not a big time hit
      (let loop ([xsss xsss] [acc '()])
        (if (null? xsss) acc (loop (cdr xsss) (append acc (car xsss)))))))
  
  ; Split each a partition by a block splitter
  (define/contract split-partition-by
    ((block? . -> . partition?) partition? . -> . partition?)
    (lambda (partition-block partition)
      (unnest (map (lambda (block) (partition-block block)) partition))))
  
  ; Split each a partition by a discriminator
  (define/contract split-partition (discriminator? partition? . -> . partition?)
    (lambda (f xss)
      (unnest (map (lambda (xs) (split-set f xs)) xss))))
  
  ; Split a block by the values  
  (define/contract split-by-vector-values
    ((state? . -> . vector?) (integer? . -> . discriminator?) . -> . (block? . -> . partition?))
    (lambda (list-accessor discriminator)
      (lambda (block)
        (list:foldr (lambda (i xss) (split-partition (discriminator i) xss))
                    (block->partition block)                          ; initial partition
                    (iota (vector-length (list-accessor (car block))))))))   ; split for each member in the list
  
  (define/contract split-by-vector-vector-values
    ((state? . -> . (vectorof vector?))
     (state? . -> . vector?)
     (integer? integer? . -> . discriminator?)
     . -> . (block? . -> . partition?))
    (lambda (vector-accessor vector-vector-accessor discriminator)
      (lambda (type-list)
        (list:foldr (lambda (row acc)
                      (list:foldr (lambda (col acc2)
                                    (split-partition (discriminator row col) acc2))
                                  acc
                                  (iota (vector-length (vector-vector-accessor (car type-list))))))
                    (list type-list)
                    (iota (vector-length (vector-accessor (car type-list))))))))
  
  ;; Split a block of unions into a paritions with each union in a
  ;; block has the same number of elements.
  (define/contract split-union-states ((listof union-state?) . -> . (listof (listof union-state?)))
    (lambda (unions)
      (let* ([number-elements-discriminator
              (lambda (union) (length (type-union-elements union)))])
        ;; TBD Spliting by number of elements in a union cannot possibly be right
        ;(split-set number-elements-discriminator unions))))
        (list unions))))
  
  (define/contract split-struct-value-states ((listof struct-value-state?) . -> . (listof (listof struct-value-state?)))
    (lambda (structs)
      (let ([number-elements-discriminator
             (lambda (struct) (length (type-struct-value-types struct)))])
        ; a total ordering must be imposed on labels
        (split-partition (lambda (sv) (eq-hash-code (type-struct-value-type-label sv)))
                         (split-set number-elements-discriminator structs)))))
  
  ;; Split a block of case-lambda-states into a partition with each
  ;; block having the same number of expressions, each parameter list
  ;; having the same length, each rest and req arg lists have the same
  ;; length and values.
  (define/contract split-case-lambda-states
    ((listof case-lambda-state?) . -> . (listof (listof case-lambda-state?)))
    (lambda (cls)
      (letrec
          ([get-number-args
            ;(lambda (i) (lambda (cl) (vector-length (vector-ref (type-case-lambda-argss cl) i))))]
            (lambda (i) (lambda (cl) (vector-length (vector-ref (case-lambda-state-argss cl) i))))]
           [get-number-exps
            ;(lambda (cl) (vector-length (type-case-lambda-exps cl)))]
            (lambda (cl) (vector-length (case-lambda-state-exps cl)))]
           [get-rest-arg
            ;(lambda (i) (lambda (cl) (vector-ref (type-case-lambda-rest-arg?s cl) i)))]
            (lambda (i) (lambda (cl) (vector-ref (case-lambda-state-rest-arg?s cl) i)))]
           [get-req-arg
            ;(lambda (i) (lambda (cl) (vector-ref (type-case-lambda-req-args cl) i)))]
            (lambda (i) (lambda (cl) (vector-ref (case-lambda-state-req-args cl) i)))]
           [req-arg-gt (lambda (xs ys)
                         (cond [(and (null? xs) (null? ys)) #f]
                               [(= (car xs) (car ys)) (req-arg-gt (cdr xs) (cdr ys))]
                               [(> (car xs) (car ys)) #t]
                               [(< (car xs) (car ys)) #f]
                               [else (error 'lex "Differing lengths")]))]
           [rest-arg-gt (lambda (xs ys)
                          (cond [(and (null? xs) (null? ys)) #f]
                                [(= (car xs) (car ys)) (rest-arg-gt (cdr xs) (cdr ys))]
                                [(car xs) #t]
                                [else #f]))])
        (split-partition-by
         (split-by-vector-values case-lambda-state-req-args get-req-arg)
         (split-partition-by
          (split-by-vector-values case-lambda-state-rest-arg?s get-rest-arg)
          (split-partition-by
           (split-by-vector-values         ; block -> partition
            case-lambda-state-argss        ; (any/c . -> . list?)  ;  case-lambda -> args
            get-number-args)               ; (integer? . -> . (any/c . -> . any))
           (split-set get-number-exps cls)))) )))
  
  )

