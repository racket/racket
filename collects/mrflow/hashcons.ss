(module hashcons (lib "mrflow.ss" "mrflow")
  (require (prefix list: (lib "list.ss"))
           (lib "match.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
           (prefix string: (lib "string.ss"))
           
           (prefix cst: "constants.ss")
           "dfa.ss"
           "trie.ss"
           "labels.ss"
           "types.ss"
           "set-hash.ss"
           "env.ss"
           "util.ss")
  
  (provide
   ;; Create a new hashcons table
   make-hashcons-table
   
   ;; Convert a type to a handle
   hashcons-type
   ;; Get the type of a handle
   get-type
   
   ;; Get a pretty string from a handle
   handle->string
   handle->list
   
   hashcons-table->list
   hashcons-table->dot
   handles->dot
   
   ;; contract functions
   hashcons-table?
   hashcons-type?
   
   ;; functions used in testing
   ;hashcons-table-size
   
   ;; debug functions
   ;hashcons-acyclic-subtrees
   )
  
  ;; Debugging settings
  ;(print-struct #t)
  ;(print-hash-table #t)
  ;(pretty-print-columns 105)
  
  ;; Some predicates for contracts
  (define label-type? (or/c type-case-lambda? type-cons? type-promise?
                            type-struct-value? type-union? type-values? type-vector?))
  (define base-type? (or/c type-empty? type-cst? type-struct-type?))
  (define hashcons-type? (or/c label-type? base-type? type-rec?))
  
  ;;
  ;; Hashcons tables
  ;;
  (define-struct hashcons-table
    (from-handle     ;; handle -> (or/c dfa label base-type)
     from-dfa        ;; dfa -> handle
     from-label      ;; label -> handle
     from-base-type  ;; base-type -> handle
     dfa-trie        ;; type -> trie, handle -> handle
     number-handles)
    (make-inspector))
  
  (set! make-hashcons-table
	(let ([old-make-hashcons-table make-hashcons-table])
	  (lambda ()
	    (old-make-hashcons-table
	     (make-hash-table)
	     (make-hash-table 'equal) 
	     (make-hash-table 'equal) 
	     (make-hash-table 'equal)
	     (make-trie)
	     0))))
  
  (define list-of-handles? (lambda (xs) (and (list? xs) (andmap handle? xs))))
  
  (define get-next-handle
    (lambda (tbl)
      (let ([x (hashcons-table-number-handles tbl)])
        (set-hashcons-table-number-handles! tbl (+ x 1))
        x)))
  
  (define hashcons-table-size
    (lambda (tbl)
      (hashcons-table-number-handles tbl)))
  
  (define get-type-handle
    (lambda (tbl type)
      (hash-table-get
       (hashcons-table-from-base-type tbl) type
       (lambda ()
         (hash-table-get
          (hashcons-table-from-label tbl) type
          (lambda () (hash-table-get (hashcons-table-from-dfa tbl) type
                                     (lambda ()
				       (error 'get-type-handle "Type ~a not in hashcons table: ~a"
                                              type (hashcons-table->list tbl))))))))))
  
  (define/contract get-type (hashcons-table? handle? . -> . hashcons-type?)
    (lambda (tbl handle)
      (hash-table-get (hashcons-table-from-handle tbl) handle
                      (lambda () (error 'get-type "Handle: ~a not in hashcons table" handle)))))
  
  (define has-handle?
    (lambda (tbl handle)
      (hash-table-get (hashcons-table-from-handle tbl) handle cst:thunk-false)))
  
  (define has-base-type?
    (lambda (tbl base-type)
      (hash-table-get (hashcons-table-from-base-type tbl) base-type cst:thunk-false)))
  
  (define has-label-type?
    (lambda (tbl label-type)
      (hash-table-get (hashcons-table-from-label tbl) label-type cst:thunk-false)))
  
  (define has-dfa-type?
    (lambda (tbl dfa-type)
      (hash-table-get (hashcons-table-from-dfa tbl) dfa-type cst:thunk-false)))
  
  (define has-type?
    (lambda (tbl type)
      (or (has-base-type? tbl type) (has-label-type? tbl type) (has-dfa-type? tbl type))))
  
  (define/contract add-base-type
    (hashcons-table? base-type? . ->d .
                     (lambda (tbl base-type)
                       (when (has-base-type? tbl base-type)
                         (error 'add-base-type "Already have hashconsed ~a" base-type))
                       handle?))
    (lambda (tbl base-type)
      (let ([h (get-next-handle tbl)])
        (hash-table-put! (hashcons-table-from-handle tbl) h base-type)
        (hash-table-put! (hashcons-table-from-base-type tbl) base-type h)
        h)))
  
  (define/contract add-label-type
    (hashcons-table? label-type? . ->d . 
                     (lambda (tbl label-type)
                       (when (has-label-type? tbl label-type)
                         (error 'add-label-type "Label Type ~a already present in hashcons table" label-type))
                       (when (has-dfa-type? tbl label-type)
                         (error 'add-label-type "Label Type ~a is equivalent to DFA type" label-type))
                       handle?))
    (lambda (tbl label-type)
      (let ([h (get-next-handle tbl)])
        (hash-table-put! (hashcons-table-from-handle tbl) h label-type)
        (hash-table-put! (hashcons-table-from-label tbl) label-type h)
        h)))
  
  ;; add-dfa-type is slightly different from add-label-type and
  ;; and-base-type in that it needs to take its handle as an argument.
  ;; This is because we need to substitute all state numbers for
  ;; handle numbers in all states of the DFA prior to adding it to
  ;; them hashcons table.
  (define/contract add-dfa-type
    (hashcons-table? label-type? handle? . ->d . 
                     (lambda (tbl dfa-type handle)
                       (when (has-dfa-type? tbl dfa-type)
                         (error 'add-dfa-type "DFA Type ~a already present in ~a" dfa-type (hashcons-table->list tbl)))
                       handle?))
    (lambda (tbl dfa-type handle)
      (hash-table-put! (hashcons-table-from-handle tbl) handle dfa-type)
      (hash-table-put! (hashcons-table-from-dfa tbl) dfa-type handle)
      handle))
  
  (define/contract recall-base-type (hashcons-table? type? . -> . handle?)
    (lambda (tbl base-type)
      (if (has-base-type? tbl base-type)
          (hash-table-get (hashcons-table-from-base-type tbl) base-type)
          (add-base-type tbl base-type))))
  
  (define/contract recall-label-type (hashcons-table? label-type? . -> . handle?)
    (lambda (tbl label-type)
      (cond
        [(has-dfa-type? tbl label-type) (hash-table-get (hashcons-table-from-dfa tbl) label-type)]
        [(has-label-type? tbl label-type) (hash-table-get (hashcons-table-from-label tbl) label-type)]
        [else (add-label-type tbl label-type)])))
  
  ; Hashcons-type is the main function.
  ; 
  ; Hashconsing proceedings in two main stages. We first hashcons as
  ; much as possible in a straight forward, bottom up fashion.  If
  ; there is no recursive types, then we are done and just return the
  ; handle. If there is a recursive type, then it is necessary to
  ; hashcons the recursive type in a bottom up fashion whenever a
  ; type has no free variables.
  (define/contract hashcons-type (hashcons-table? hashcons-type? . -> . handle?)
    (lambda (tbl type)
      (let ([size (hashcons-table-size tbl)]
            [v (let ([type (hashcons-acyclic-subtrees tbl type)])
                 (if (handle? type) type
                     (bottom-up-hashcons tbl type)))])
        v)))
  
  ; Hashcons all subtrees in a type containing no variables. Returns
  ; a type where all subtrees w/o variables are replaced by the
  ; corresponding handle. All label types which remain, have at least
  ; one children which contain a variable.
  ;
  ; After hashconsing all of the children of a type, if there is a
  ; child which has not been replaced by a handle then we have a
  ; recursive type and we do not hashcons the label. If the children
  ; are all handles, we hashcons this label and return its handle in
  ; place of the label.
  (define hashcons-acyclic-subtrees
    (lambda (tbl type)
      ((fold-type
        (lambda (handle) handle)                   ;; handle       :: handle -> b
        (lambda (rest-args req-args argss exps)    ;; case-lambda  :: [bool] [nat] [[b]] [b] -> b
          (let ([new-case-lambda (make-type-case-lambda
                                  (if (list? rest-args) (list->vector rest-args) rest-args)
                                  (if (list? req-args) (list->vector req-args) req-args)
                                  argss exps)])
            (if (and (vector-of-vector-of? handle? argss) (vector-of? handle? exps))
                (recall-label-type tbl new-case-lambda)
                new-case-lambda)))
        (lambda (hd tl)                            ;; cons         :: b b -> b
          (let ([new-type-cons (make-type-cons hd tl)])
            (if (and (handle? hd) (handle? tl))
                (recall-label-type tbl new-type-cons)
                new-type-cons)))
        (lambda (ty)                               ;; cst          :: any/c -> b
          (recall-base-type tbl (make-type-cst ty)))
        (let ((empty (make-type-empty)))           ;; empty        :: -> b
          (lambda ()
            (recall-base-type tbl empty)))
        (lambda (type)                             ;; promise      :: b -> b
          (let ([new-type-promise (make-type-promise type)])
            (if (handle? type)
                (recall-label-type tbl new-type-promise)
                new-type-promise)))
        (lambda (vars types body)                  ;; rec          :: [b] [b] b -> b
          (let* ([new-type-rec (make-type-rec vars types body)])
            (if (and (list-of-handles? types) (handle? body))
                body
                new-type-rec)))
        (lambda (label)                            ;; struct-type  :: label -> b
          (recall-base-type tbl (make-type-struct-type label)))
        (lambda (label types)                      ;; label        :: [b] -> b
          (let ([new-type (make-type-struct-value label types)])
            (if (list-of-handles? types)
                (recall-label-type tbl new-type)
                new-type)))
        (lambda (elements)                         ;; union        :: [b] -> b
	  (cond [(null? elements) (recall-base-type tbl (make-type-empty))]
		[(length-one? elements) (car elements)]
		[(list-of-handles? elements)
		 (let* ([elements (min-list-numbers elements)])
		   (cond
                     [(length-one? elements) (car elements)]
                     [else
                      (recall-label-type tbl (make-type-union elements))]))]
		[else (make-type-union elements)]))
        (lambda (type)                             ;; values       :: b -> b
          (let ([new-type-values (make-type-values type)])
            (if (handle? type)
                (recall-label-type tbl new-type-values)
                new-type-values)))
        make-type-var                              ;; var          :: name boolean boolean -> b
        (lambda (type)                             ;; vector       :: b -> b
          (let ([new-type-vector (make-type-vector type)])
            (if (handle? type)
                (recall-label-type tbl new-type-vector)
                new-type-vector))))
       type)))
  
  ;; After we've hashconsed a recursive type this does the final job
  ;; of adding it to the hashcons table.
  (define/contract hashcons-rec-type-body (hashcons-table? (or/c type? handle?) . -> . (or/c type? handle?))
    (lambda (tbl type)
      (let ([recall-type (lambda (type) (if (has-type? tbl type)
                                            (get-type-handle tbl type)
                                            (recall-label-type tbl type)))])
        ((fold-type
          (lambda (handle) handle)                   ;; handle       :: handle -> b
          (lambda (rest-args req-args argss exps)    ;; case-lambda  :: [bool] [bool] [[b]] [b] -> b
            (recall-type (make-type-case-lambda rest-args req-args argss exps)))
          (lambda (hd tl)                            ;; cons         :: b b -> b
            (recall-type (make-type-cons hd tl)))
          (lambda (ty)                               ;; cst          :: any/c -> b
            (error 'hashcons-rec-type-body "type-cst should have been previously hashconsed"))
          (lambda ()                                 ;; empty        :: -> b
            (error 'hashcons-rec-type-body "type-empty should have been previously hashconsed"))
          (lambda (type)                             ;; promise      :: b -> b
            (recall-type (make-type-promise type)))
          (lambda (vars types body)                  ;; rec          :: [b] [b] b -> b
            (error 'hashcons-rec-type-body "Should not have a type-rec DFA at this point"))
          (lambda (label)                            ;; struct-type
            (error 'hashcons-rec-type-body "struct-type should have been hashcons already"))
          (lambda (label types)                      ;; struct-value
            (recall-type (make-type-struct-value label types)))
          (lambda (elements)                         ;; type-union [b] -> b
	    (let ([elements (min-list-numbers elements)])
	      (cond [(length-one? elements) 
                     ;; TBD seems suspiscious
                     ;; if there is only one element then the union
                     ;; containing it was redundant and perhaps the
                     ;; type should be reconsidered
                     (car elements)]
		    [else
		     (recall-type (make-type-union elements))])))
          (lambda (type)                             ;; type-values       ;; b -> b
            (recall-type (make-type-values type)))
          (lambda (name reach handle)                ;; type-var          ;; b -> b
            (error 'hashcons-rec-type-body "Should not have type-var at this point"))
          (lambda (type)                             ;; type-vector       ;; b -> b
            (recall-type (make-type-vector type))))
         type))))
  
  ;; Almost the same as acyclic hashcons except when we reach a
  ;; rec-type with no free variables we hashcons it.
  ;; 
  ;; Perhaps this could be merged w/ acyclic hashcons. The code is
  ;; almost identical
  (define/contract bottom-up-hashcons
    (hashcons-table? hashcons-type? . ->d . (lambda (tbl type) handle?))
    (lambda (tbl type)
      (let
          ([hashcons
            (fold-type
             (lambda (handle) handle)                 ;; handle       :: handle -> b
             (lambda (rest-args req-args argss exps)  ;; case-lambda  :: [bool] [bool] [[b]] [b] -> b
               (let ([new-case-lambda (make-type-case-lambda rest-args req-args argss exps)])
                 (if (and (vector-of-vector-of? handle? argss)
                          (vector-of? handle? exps))
                     (recall-label-type tbl new-case-lambda)
                     new-case-lambda)))
             (lambda (hd tl)                          ;; cons         :: b b -> b
               (let ([new-type-cons (make-type-cons hd tl)])
                 (if (and (handle? hd) (handle? tl))
                     (recall-label-type tbl new-type-cons)
                     new-type-cons)))
             (lambda (type)                           ;; cst          :: any/c -> b
               (recall-base-type tbl type))
             (lambda ()                               ;; empty        :: -> b
               (recall-base-type tbl (make-type-empty)))
             (lambda (value)                          ;; promise      :: b -> b
               (let ((new-type-promise (make-type-promise value)))
                 (if (handle? value)
                     (recall-label-type tbl new-type-promise)
                     new-type-promise)))
             (bottom-up-hashcons-rec-type tbl)
             (lambda (label)                            ;; type-struct-type 
               (recall-base-type tbl (make-type-struct-type label)))
             (lambda (label types)                      ;; type-struct-value
               (let ([new-type (make-type-struct-value label types)])
                 (if (list-of-handles? types)
                     (recall-label-type tbl new-type)
                     new-type)))
             (lambda (elements)           ;; type-union [b] -> b
               (if (list-of-handles? elements)
                   (let* ([elements (min-list-numbers elements)])
                     (cond
                       [(null? elements) (recall-base-type tbl (make-type-empty))]
                       [(length-one? elements) (car elements)]
                       [else
                        (recall-label-type tbl (make-type-union elements))]))
                   (make-type-union elements)))
             (lambda (type)               ;; type-values       ;; b -> b
               (let ((new-type-values (make-type-values type)))
                 (if (handle? type)
                     (recall-label-type tbl new-type-values)
                     new-type-values)))
             make-type-var
             (lambda (element)            ;; type-vector      ;; b -> b
               (let ((new-type-vector (make-type-vector element)))
                 (if (handle? element)
                     (recall-label-type tbl new-type-vector)
                     new-type-vector))))])
        (hashcons type))))
  
  (define/contract bottom-up-hashcons-rec-type
    (hashcons-table? . -> . ((listof type-var?) (listof (or/c type? handle?)) (or/c type? handle?) . ->d .
                                                (lambda (vars types body)
                                                  (when (has-free-vars? (make-type-rec vars types body))
                                                    (error 'bottom-up-hashcons "~a has free type variables~n" (make-type-rec vars types body)))
                                                  handle?)))
    (lambda (tbl)
      (lambda (vars types body)                ;; rec          :: [b] [b] b -> b
        (let*-values
            ([(vars types body)
              (if (type-var? body)
                  (values vars types body)
                  (let ([newvar (make-type-var (gensym) #f #f)])
                    (values (cons newvar vars) (cons body types) newvar)))]
             [(graph bindings)
              (let ([graph (make-hash-table)]
                    [bindings (make-hash-table)])
                (for-each (lambda (var type)
                            (hash-table-put! graph (type-var-name var) (get-referenced-vars type))
                            (hash-table-put! bindings (type-var-name var) type))
                          vars types)
                (values graph bindings))]
             [(sccs) (strongly-connected-components graph)]
             [(env)
              (list:foldl (lambda (scc env)
                            (hashcons-scc tbl scc graph bindings env))
                          (create-tenv)
                          sccs)])
          (lookup-symbol env (type-var-name body))))))
  
  (define/contract hashcons-scc (hashcons-table? (listof any/c) hash-table? hash-table? tenv? . -> . tenv?)
    (lambda (tbl scc graph bindings env)
      (cond
        ;; The SCC is actually a recursive type
        [(and (length-one? scc) (memq (car scc) (hash-table-get graph (car scc))))
         (let*-values ([(ty) (make-type-rec (list (make-type-var (car scc) #f #f))
                                            (list (hash-table-get bindings (car scc)))
                                            (make-type-var (car scc) #f #f))]
                       [(ty) (subst-handles/vars-if-possible ty env)]
                       [(ty) (hashcons-acyclic-subtrees tbl ty)]                       
                       [(dfa binder-states) (create-dfa-from-type ty env)]
                       [(min-dfa min-binder-stnums) (minimize-dfa dfa binder-states)]
                       [(min-stnum->handle)
                        (let ([greatest-handle (greatest-handle min-dfa)])
                          (if greatest-handle
                              (recursive-with-handle tbl min-dfa
                                                     (map state-number
                                                          (list:filter
                                                           (lambda (s)
                                                             (not (handle-state? s)))
                                                           (get-ordered-states min-dfa)))
                                                     greatest-handle)
                              #f))]
                       [(binder-handles)
                        (if min-stnum->handle
                            (map (lambda (stnum)
                                   (hash-table-get min-stnum->handle stnum))
                                 min-binder-stnums)
                            (let ([min-stnum->handle (recall-entire-dfa tbl min-dfa)])
                              (map (lambda (minstnum)
                                     (hash-table-get min-stnum->handle minstnum))
                                   min-binder-stnums)))]
                       ;(let* ([min-states (get-ordered-states min-dfa)]
                       ;       [all-handles (recall-entire-dfa tbl min-dfa)])
                       ;  (let loop ([all-states min-states]
                       ;             [all-handles all-handles])
                       ;    (if (= (state-number (car all-states)) (car min-binder-stnums))
                       ;        (list (car all-handles))
                       ;        (loop (cdr all-states) (cdr all-handles))))))]
                       )
           (extend-tenv env scc binder-handles))]
        
        [(length-one? scc)
         (let* ([var (car scc)]
                [ty (hash-table-get bindings var)]
                [ty-no-vars (subst-handles/vars ty env)]
                [handle (hashcons-rec-type-body tbl ty-no-vars)])
           (extend-tenv env (list var) (list handle)))]
        
        [else
         (let*-values
             ([(ty) (make-type-rec (map (lambda (v) (make-type-var v #f #f)) scc)
                                   (map (lambda (v) (hash-table-get bindings v)) scc)
                                   (make-type-var (car scc) #f #f))]
              [(ty) (subst-handles/vars-if-possible ty env)]
              [(ty) (hashcons-acyclic-subtrees tbl ty)]
              [(dfa binder-stnums) (create-dfa-from-type ty env)]
              [(min-dfa min-binder-stnums) (minimize-dfa dfa binder-stnums)]
              [(min-states) (get-ordered-states min-dfa)]
              [(stnum->handle)
               (let* ([greatest-handle (greatest-handle min-dfa)])
                 (if greatest-handle
                     (recursive-with-handle tbl min-dfa
                                            (map state-number
                                                 (list:filter
                                                  (lambda (s)
                                                    (not (handle-state? s)))
                                                  min-states))
                                            greatest-handle)
                     #f))]
              [(binder-handles) (if stnum->handle
                                    (map (lambda (stnum) (hash-table-get stnum->handle stnum)) min-binder-stnums)
                                    (let ([stnum->handle (recall-entire-dfa tbl min-dfa)])
                                      (map (lambda (stnum)
                                             (hash-table-get stnum->handle stnum))
                                           min-binder-stnums)))]
              ;[(handles) (if stnum->handle #f (recall-entire-dfa tbl min-dfa))]
              ;[(binder-handles)
              ; (if handles
              ;     (letrec ([position
              ;               (lambda (state-num xs counter)
              ;                 (cond [(null? xs) (error 'not-found)]
              ;                       [(= (state-number (car xs)) state-num) counter]
              ;                       [else (position state-num (cdr xs) (add1 counter))]))])
              ;       (map (lambda (pos) (list-ref handles pos))
              ;            (map (lambda (state) (position state min-states 0)) min-binder-stnums)))
              ;     (map (lambda (stnum) (hash-table-get stnum->handle stnum)) min-binder-stnums))]
              )
           (extend-tenv env scc binder-handles))])))
  
  ;; If this DFA has been hashconsed return its handle, otherwise add it to the
  ;; hashcons table and the trie.
  (define/contract recall-entire-dfa
    (hashcons-table? dfa? . ->d .
                     (lambda (tbl dfa)
                       (lambda (ht)
                         (when (hash-table? ht)
                           (unless (= (get-dfa-size dfa) (hash-table-size ht))
                             (error 'recall-entire-dfa "Missing or extra states in state->handle map~ndfa=~a~nstate->handle=~a" dfa ht))
                           (for-each (lambda (st) (unless (hash-table-get ht (state-number st) cst:thunk-false)
                                                    (error 'recall-entire-dfa
                                                           "No matching state ~s  map~ndfa=~a~nstate->handle=~a" st (dfa->list dfa)
                                                           (hash-table-map ht (lambda (k v) (cons k v))))))
                                     (get-ordered-states dfa)))
                         #t)))
    (lambda (hashcons-table dfa)
      (let* ([states (get-ordered-states dfa)]
             [trie (hashcons-table-dfa-trie hashcons-table)]
             [maybe-handles (dfa-present? trie states)])
        (or (and maybe-handles
                 (make-immutable-hash-table (map (lambda (state handle)
                                                   (cons-immutable (state-number state) handle))
                                                 states maybe-handles)))
            (let* ([new-handles
                    (map (lambda (state) (if (handle-state? state)
                                             (handle-state-handle state)
                                             (get-next-handle hashcons-table)))
                         states)]
                   [stnum->handle
                    (letrec ([tbl (make-hash-table)]
                             [stnum->state (dfa-stnum->state dfa)]
                             [close (lambda (st ancest)
                                      (if (and (union-state? st)
                                               (length-one? (union-state-elements st)))
                                          (if (memq (state-number st) ancest)
                                              (error 'cycle-of-empty-unions-detected)
                                              (close (hash-table-get stnum->state (car (union-state-elements st)))
                                                     (cons (state-number st) ancest)))
                                          st))])
                      (for-each (lambda (state handle)
                                  (hash-table-put! tbl (state-number state) handle))
                                states new-handles)
                      (for-each (lambda (state)
                                  (hash-table-put! tbl (state-number state)
                                                   (hash-table-get tbl (state-number (close state null)))))
                                states)
                      
                      ;; This is mildly funky, if there is a union of length one, we will close it
                      ;; and the state will point to the handle of its single element, but the union
                      ;; will still be a state in the dfa (and therefore a key in the trie, although
                      ;; it will never be refered to in the types representing the dfa i.e. an
                      ;; unused handle)
                      
                      tbl)]
                   [lookup
                    (lambda (stnum) (hash-table-get stnum->handle stnum))]
                   [subst-handle/state
                    (match-lambda
                      [($ cons-state state hd tl)
                       (make-type-cons (lookup hd) (lookup tl))]
                      [($ case-lambda-state state rest-arg?s req-args argss exps)
                       (make-type-case-lambda rest-arg?s
                                              req-args
                                              (map-vector-of-vector lookup argss)
                                              (map-vector lookup exps))]
                      [($ union-state state elements)
                       (let ([handles (min-list-numbers (map lookup elements))])
                         (if (length-one? handles) (car handles)
                             (make-type-union handles)))]  ;; double check this 
                      [($ promise-state state value)
                       (make-type-promise (lookup value))]
                      [($ struct-value-state state label types)
                       (make-type-struct-value label (map lookup types))]
                      [($ values-state state types)
                       (make-type-values (lookup types))]
                      [($ vector-state state element)
                       (make-type-vector (lookup element))]
                      [x (error 'recall-entire-dfa "Unmatched type ~a" x)])])
              (add-dfa-states trie states new-handles)
              (for-each (lambda (dfa-state handle)
                          (unless (handle-state? dfa-state)
                            (let ([handle-or-state (subst-handle/state dfa-state)])
                              (unless (handle? handle-or-state)
                                (add-dfa-type hashcons-table handle-or-state handle)))))
                        states new-handles)
              stnum->handle)))))
  
  ;; Almost a fold, with the exception of the type-rec-type binding variables which we do not
  ;; recurse on. 
  (define fold-type
    (lambda (handlef       ;; handle -> b
             case-lambdaf  ;; [bool] [int] [[b]] [b] -> b
             consf         ;; b b -> b
             cstf          ;; any/c -> b
             emptyf        ;; -> b
             promisef      ;; b -> b
             recf          ;; [b] [b] b -> b
             struct-typef  ;; label -> b
             struct-valuef ;; label [b] -> b
             unionf        ;; [b] -> b
             valuesf       ;; b -> b
             varf          ;; name bool -> b
             vectorf)      ;; b -> b
      (lambda (type)
        (letrec ([foldt (fold-type handlef case-lambdaf consf cstf emptyf
                                   promisef recf struct-typef struct-valuef
                                   unionf valuesf varf vectorf)])
          (cond
            [(handle? type)
             (handlef type)]
            [(type-case-lambda? type)
             ;; When we first get a case-lambda its arguments may be lists,
             ;; so convert them to vectors once and for all here.  This is
             ;; a hack until case-lambda uses vectors in all cases.
             (let* ([rest-arg?s (type-case-lambda-rest-arg?s type)]
                    [req-args (type-case-lambda-req-args type)]
                    [argss (type-case-lambda-argss type)]
                    [exps (type-case-lambda-exps type)]
                    [argss (if (list? argss) (lol->vov argss) argss)]
                    [exps (if (list? exps) (list->vector exps) exps)]
                    [argss (for-each-vov! foldt argss)]
                    [exps (for-each-vector! foldt exps)])
               (case-lambdaf  rest-arg?s req-args argss exps))]
            [(type-cons? type)
             (consf (foldt (type-cons-car type)) (foldt (type-cons-cdr type)))]
            [(type-cst? type)
             (cstf (type-cst-type type))]
            [(type-empty? type)
             (emptyf)]
            [(type-promise? type)
             (promisef (foldt (type-promise-value type)))]
            [(type-rec? type)
             (let ([vars (type-rec-vars type)]    ; <-- Do not recur on variables being bound
                   [types (map foldt (type-rec-types type))]
                   [body (foldt (type-rec-body type))])
               (recf vars types body))]
            [(type-struct-type? type)
             (struct-typef (type-struct-type-type-label type))]
            [(type-struct-value? type) 
             (let ([label (type-struct-value-type-label type)]
                   [types (map foldt (type-struct-value-types type))])
               (struct-valuef label types))]
            [(type-union? type)
             (unionf (map foldt (type-union-elements type)))]
            [(type-values? type)
             (valuesf (foldt (type-values-type type)))]
            [(type-var? type)
             (varf (type-var-name type) (type-var-reach type) (type-var-handle type))]
            [(type-vector? type)
             (vectorf (foldt (type-vector-element type)))]
            [else (error 'fold-type "Unmatched type ~a" type)])))))
  
  ;; Return a type with handles replacing variables
  (define/contract subst-handles/vars ((or/c label-type? handle? type-var?) tenv? . -> . (or/c type? handle?))
    (lambda (type tenv)
      (let subst ([type type])
        (cond
          [(handle? type) type]
          [(type-case-lambda? type); rest-arg?s req-args argss exps)
           (let* ([rest-arg?s (type-case-lambda-rest-arg?s type)]
                  [req-args (type-case-lambda-req-args type)]
                  [argss (for-each-vov! subst (type-case-lambda-argss type))]
                  [exps (for-each-vector! subst (type-case-lambda-exps type))])
             (make-type-case-lambda rest-arg?s req-args argss exps))]
          [(type-cons? type)
           (make-type-cons (subst (type-cons-car type)) (subst (type-cons-cdr type)))]
          [(type-promise? type)
           (make-type-promise (subst (type-promise-value type)))]
          [(type-rec? type) ; vars handle-list body)
           (let ([vars (type-rec-vars type)]
                 [handle-list (type-rec-types type)]
                 [body (type-rec-body type)])
             (for-each (lambda (handle) (unless (handle? handle)
                                          (pretty-print (make-type-rec vars handle-list body))
                                          (error 'type-rec-var-no-handle)))
                       handle-list)
             (subst-handles/vars body
                                 (extend-tenv tenv (map type-var-name vars) handle-list)))]
          [(type-struct-value? type)
           (make-type-struct-value (type-struct-value-type-label type) (map subst (type-struct-value-types type)))]
          [(type-union? type)
           (make-type-union (map subst (type-union-elements type)))]
          [(type-values? type)
           (make-type-values (subst (type-values-type type)))]
          [(type-var? type)
           (lookup-symbol tenv (type-var-name type))]
          [(type-vector? type)
           (make-type-vector (subst (type-vector-element type)))]
          [else (error 'subst-handles/vars "Unmatched type ~a" type)]))))
  
  (define/contract subst-handles/vars-if-possible
    ((or/c hashcons-type? handle? type-var?) tenv? . -> . (or/c type? handle?))
    (lambda (type tenv)
      (let subst ([type type])
        (match type
          [(? handle? type) type]
          [($ type-case-lambda rest-arg?s req-args argss exps)
           (let* ([argss (for-each-vov! subst argss)]
                  [exps (for-each-vector! subst exps)])
             ; for-each-vector set! args and exps in place
             type)]
          [($ type-cons hd tl)
           (set-type-cons-car! type (subst hd))
           (set-type-cons-cdr! type (subst tl))
           type]
          [($ type-promise value)
           (set-type-promise-value! type (subst value))
           type]
          [($ type-rec vars types body)
           ;; maybe we should add the vars/types to the scope iff the type is a handle
           (set-type-rec-types! type (map subst types))
           (set-type-rec-body! type (subst body))
           type]
          [($ type-struct-value label types)
           (set-type-struct-value-types! type (map subst types))
           type]
          [($ type-union elements)
           (set-type-union-elements! type (map subst elements))
           type]
          [($ type-values ty)
           (set-type-values-type! type (subst ty))
           type]
          [($ type-var name reach handle)
           (or (maybe-lookup-symbol tenv name) type)]
          [($ type-vector element)
           (set-type-vector-element! type (subst element))
           type])
        )))
  
  (define/contract has-free-vars? ((or/c type? handle?) . -> . boolean?)
    (lambda (type)
      (let* ([bound-vars (make-hash-table)]
             [bind (lambda (var)
                     (let ([cv (hash-table-get bound-vars var cst:thunk-false)])
                       (hash-table-put! bound-vars var (if cv (add1 cv) 1))))]
             [unbind (lambda (var)
                       (let ([cv (hash-table-get bound-vars var
                                                 (lambda () (error 'unbind "Cannot unbind unbound variable ~a" var)))])
                         (when (= cv 0)
                           (error 'unbind "Cannot unbind variable ~a more times than its bound" var))
                         (hash-table-put! bound-vars var (sub1 cv))))]
             [bound? (lambda (var)
                       (let ([cv (hash-table-get bound-vars var cst:thunk-false)])
                         (and cv (> cv 0))))])
        (let/ec k
          (letrec
              ([list-has-free-vars? (lambda (args) (ormap has-free-vars? args))]
               [has-free-vars?
                (match-lambda
                  [(? handle? type) #f]
                  [($ type-case-lambda rest-arg?s req-args argss exps)
                   (or (vector-of-vector-has? has-free-vars? argss)
                       (vector-has? has-free-vars? exps))]
                  [($ type-cons hd tl)
                   (or (has-free-vars? hd) (has-free-vars? tl))]
                  [($ type-promise value)
                   (has-free-vars? value)]
                  [($ type-rec vars types body)
                   (let* ([vnames (map type-var-name vars)]
                          [_ (for-each bind vnames)]
                          [fv (or (list-has-free-vars? types) (has-free-vars? body))])
                     (for-each unbind vnames)
                     fv)]
                  [($ type-struct-value label types)
                   (list-has-free-vars? types)]
                  [($ type-union elements)
                   (list-has-free-vars? elements)]
                  [($ type-values type)
                   (has-free-vars? type)]
                  [($ type-var name reach handle)
                   (if (bound? name) #f (k #t))]
                  [($ type-vector element)
                   (has-free-vars? element)]
                  [_ (error 'has-free-vars? "Unmatched type ~a" type)])])
            (has-free-vars? type))))))
  
  (define/contract get-referenced-vars ((or/c type? handle?) . -> . (listof symbol?))
    (lambda (type)
      (let ([refed (make-hash-table)])
        (let loop ([type type])
          (match type
            [(? handle? type) cst:void]
            [($ type-case-lambda rest-arg?s req-args argss exps)
             (for-each-vov loop argss)
             (for-each-vector loop exps)]
            [($ type-cons hd tl)
             (loop hd) (loop tl)]
            [($ type-promise value)
             (loop value)]
            [($ type-rec vars handle-list body)
             (error 'get-referenced-vars "Nested type-rec found")]
            [($ type-struct-value label types)
             (for-each loop types)]
            [($ type-union elements)
             (for-each loop elements)]
            [($ type-values type)
             (loop  type)]
            [($ type-var name reach handle)
             (hash-table-put! refed name #t)]
            [($ type-vector element)
             (loop element)])
          (hash-table-map refed (lambda (v _) v))))))
  
  (define/contract same-label-type?
    (hashcons-table? state? (or/c type? handle?) . -> . boolean?)
    (lambda (tbl state type)
      (or (and (handle-state? state) (handle? type) (= (handle-state-handle state) type))
          (and (handle-state? state) (equal? (get-type tbl (handle-state-handle state)) type))
          
          (and (cons-state? state) (type-cons? type))
          (and (union-state? state) (type-union? type))
          (and (vector-state? state) (type-vector? type))
          (and (case-lambda-state? state) (type-case-lambda? type))
          (and (union-state? state) (type-union? type))
          (and (promise-state? state) (type-promise? type))
          (and (struct-value-state? state) (type-struct-value? type))
          )))
  
  (define/contract for-each-child
    (any/c state? type? . -> . any)
    (lambda (f state type)
      (cond [(handle-state? state)
             (void)]
            [(cons-state? state)
             (f (cons-state-car state) (type-cons-car type))
             (f (cons-state-cdr state) (type-cons-cdr type))]
            [(case-lambda-state? state)
             (let* ([sargss (case-lambda-state-argss state)]
                    [targss (type-case-lambda-argss type)]
                    [argss-length (vector-length sargss)])
               (let argss-loop ([argss-i 0])
                 (when (< argss-i argss-length)
                   (let* ([sargs (vector-ref sargss argss-i)]
                          [targs (vector-ref targss argss-i)]
                          [args-length (vector-length sargs )])
                     (let args-loop ([i 0])
                       (when (< i args-length)
                         (f (vector-ref sargs i) (vector-ref targs i))
                         (args-loop (add1 i)))))  
                   (argss-loop (add1 argss-i)))))
             (let* ([texps (type-case-lambda-exps type)]
                    [sexps (case-lambda-state-exps state)]
                    [len (vector-length texps)])
               (let loop ([i 0])
                 (when (< i len)
                   (f (vector-ref sexps i) (vector-ref texps i))
                   (loop (add1 i)))))]
            [(promise-state? state)
             (f (promise-state-value state) (type-promise-value type))]
            [(struct-value-state? state)
             (for-each f (struct-value-state-types state) (type-struct-value-types type))]
            [(union-state? state)
             (error 'union-states-not-sequential)]
            [(values-state? state)
             (f (values-state-type state) (type-values-type type))]
            [(vector-state? state)
             (f (vector-state-element state) (type-vector-element type))]
            [else
             (error 'for-each-child "Unmatched type")])))
  
  ;; See if any of the states in a minimized DFA is recursive with the
  ;; greatest handle in the DFA.
  (define/contract recursive-with-handle
    (hashcons-table? dfa? (nonempty-list-of? state-number?) handle? . ->d .
                     (lambda (hc dfa dfa-stnums handle)
                       (lambda (state->handle)
                         (when state->handle
                           (if (= (hash-table-size state->handle) (length (get-state-numbers dfa)))
                               (or (hash-table? state->handle) (boolean? state->handle))
                               (pretty-error 'missing-state->handles
                                             (list (cons 'dfa->list (dfa->list dfa))
                                                   (cons 'dfa-stnums dfa-stnums)
                                                   (cons 'handle handle)
                                                   (cons 'state->handle state->handle))))))))
    (lambda (hc dfa dfa-stnums handle)
      (define stnum->state (dfa-stnum->state dfa))
      
      (define/contract state-number->state (state-number? . -> . state?)
        (lambda (stnum)
          (hash-table-get stnum->state stnum)))
      
      ;; Return the handle a state is recursive with or false
      (define (state-recursive-with-handle stnum handle acc return-with stnum->handle)
        (if (member (cons stnum handle) acc) stnum->handle
            (let* ([type (get-type hc handle)]
                   [state (state-number->state stnum)]
                   ;; TBD new
                   ; [state (if (and (union-state? state) (length-one? (union-state-elements state)))
                   ;           (state-number->state (car (union-state-elements state)))
                   ;           state)]
                   )
              (if (or 
                   (and (same-label-type? hc state type)
                        (let ([acc (cons (cons stnum handle) acc)])
                          (if (union-state? state)
                              ;; return #f is there exists a dfa state w/o a union element
                              ;; should this be checked for all elements in both the
                              ;; type-union-elements and the union-state-elements?
                              (unless (andmap    
                                       (lambda (stnum) 
                                         (ormap (lambda (handle)
                                                  (state-recursive-with-handle stnum handle acc return-with stnum->handle))
                                                (type-union-elements type)))
                                       (union-state-elements state))
                                (return-with #f))
                              (for-each-child
                               (lambda (state handle)
                                 (unless (state-recursive-with-handle state handle acc return-with stnum->handle)
                                   (return-with #f)))
                               state type))
                          #t))
                   ;; imagine we have a case like
                   ;;   (rec-type:1 ((a0 (cons:1 _ a0))))
                   ;;   (rec-state ((a0 (union:A handle:1 (cons:B a0)))))
                   ;;  the graphs are the same modulo the union, the
                   ;;  following makes sure the state is recursive
                   ;;  with the union
                   (and (union-state? state)
                        (andmap (lambda (stnum) (state-recursive-with-handle stnum handle
                                                                             (cons (cons (state-number state) handle) acc)
                                                                             return-with stnum->handle))
                                (union-state-elements state))))
                  (begin
                    (hash-table-put! stnum->handle stnum handle)
                    stnum->handle)
                  #f))))
      
      (ormap  (lambda (stnum)
                (let/ec escape
                  (state-recursive-with-handle stnum handle null escape (make-hash-table))))
              dfa-stnums)))
  
  ;;
  ;; Printing Functions
  ;;
  
  (define hashcons-table->list
    (lambda (tbl)
      (list 'hashcons-table
            (list:sort
             (hash-table-map (hashcons-table-from-handle tbl)
                             (lambda (h _) (list 'Handle: h  '-> (handle->list tbl h void #t))))
             (lambda (x y) (> (cadr x) (cadr y)))))))
  
  (define/contract hashcons-table->dot (hashcons-table? output-port? . -> . void?)
    (lambda (tbl out)
      (handles->dot tbl
                    (hash-table-map (hashcons-table-from-handle tbl) (lambda (handle type) handle))
                    out)))
  
  (define/contract handles->dot (hashcons-table? (listof handle?) output-port? . -> . void?)
    (lambda (tbl handles out)
      (letrec
          ([type->dot
            (lambda (handle type ancest)
              (match type
                [($ type-empty)
                 (fprintf out "node~a[label = \"<f0>mt ~a\"];\n" handle handle)]
                [($ type-cst type)
                 (fprintf out "node~a[label = \"<f0>~a ~a\"];\n" handle (string:expr->string type) handle)]
                [($ type-struct-type label)
                 (fprintf out "node~a[label = \"<f0>struct ~a\"];\n" handle handle)]
                [($ type-cons hd tl)
                 (fprintf out "node~a[label = \"<f0>cons~a | <f1> | <f2>\"];\n" handle handle)
                 (fprintf out "node~a:f1 -> node~a;\n" handle hd)
                 (fprintf out "node~a:f2 -> node~a;\n" handle tl)
                 (loop hd ancest)
                 (loop tl ancest)]
                [($ type-case-lambda rest-arg?s req-args argss exps)
                 (fprintf out "node~a[label = \"<f0>lambda ~a | {" handle handle)
                 (for-each-vector (lambda (rest-arg) (fprintf out "| ~a" rest-arg)) rest-arg?s)
                 (fprintf out "} | {")
                 (for-each-vector (lambda (req-arg) (fprintf out "| ~a" req-arg)) req-args)
                 (fprintf out "} | {")
                 (for-each (lambda (args i)
                             (fprintf out "| {")
                             (for-each (lambda (arg j)
                                         (fprintf out "| <argr~ac~a> ~a" i j arg))
                                       (vector->list args) (iota (vector-length args)))
                             (fprintf out "}"))
                           (vector->list argss) (iota (vector-length argss)))
                 (fprintf out "} | {")
                 (for-each (lambda (exp i) (fprintf out "| <exp~a> ~a" i exp)) (vector->list exps) (iota (vector-length exps)))
                 (fprintf out "}\"];\n")
                 
                 (for-each (lambda (args i)
                             (for-each (lambda (arg j)
                                         (fprintf out "node~a:argr~ac~a -> node~a;\n" handle i j arg))
                                       (vector->list args) (iota (vector-length args))))
                           (vector->list argss) (iota (vector-length argss)))
                 (for-each (lambda (exp i)
                             (fprintf out "node~a:exp~a -> node~a;\n" handle i exp))
                           (vector->list exps) (iota (vector-length exps)))
                 
                 (for-each-vov (lambda (arg) (loop arg ancest)) argss)
                 (for-each-vector (lambda (exp) (loop exp ancest)) exps)]
                [($ type-promise value)
                 (fprintf out "node~a:[label = <f0>promise ~a];\n" handle handle)
                 (fprintf out "node~a:f0 -> node~a;\n" handle value)
                 (loop value ancest)]
                [($ type-struct-value label types)
                 (fprintf out "node~a[label = \"<f0>struct-value ~a\"];\n" handle handle)
                 (for-each (lambda (i)
                             (fprintf out "node~a:f -> node~a;\n" handle i)) types)
                 (for-each (lambda (i)
                             (loop i ancest)) types)]
                [($ type-values values-type)
                 (fprintf out "node~a[label = \"<f0>values ~a\"];\n" handle handle)
                 (fprintf out "node~a:f -> node~a;\n" handle values-type)
                 (loop values-type ancest)]
                [($ type-vector element)
                 (fprintf out "node~a[label = \"<f0>vector ~a\"];\n" handle handle)
                 (fprintf out "node~a:f -> node~a;\n" handle element)
                 (loop element ancest)]
                [($ type-union elements)
                 (fprintf out "node~a[label = \"<f0>union ~a\"];\n" handle handle)
                 (for-each (lambda (el i)
                             (fprintf out "node~a:f~a -> node~a;\n" handle i el)) elements (iota (length elements)))
                 (for-each (lambda (el)
                             (loop el ancest)) elements)]
                [else (error 'hashcons-type-string "~a Not implemented yet" type)]))]
           [loop (lambda (handle ancest)
                   (unless (set-in? ancest handle) ;; if we've already come across this node
                     (let* ([type (get-type tbl handle)]
                            [str (type->dot handle type (set-set ancest handle))])
                       str
                       ; (set-remove ancest handle) ;; imperative sets
                       )))])
        (fprintf out "digraph g {\n")
        (fprintf out "node[shape = record];\n")
        (fprintf out "/* Hashtable size = ~a */\n" (hashcons-table-size tbl))
        (let ([set (set-make)]) (for-each (lambda (h) (loop h set)) handles))
        (fprintf out "}\n"))))
  
  (define/contract handle->string
    (hashcons-table? handle? (handle? handle? . -> . boolean?) . -> . string?)
    (lambda (tbl handle union-elements-equal?)
      (let ([out (open-output-string)])
        ; the following gets rid of the newline at the end of the type, but
        ; it also prevents newlines *inside* the type...  This can probably be
        ; fixed using a custom pretty-print-print-line that uses the default
        ; liner except for the last line.  Should be good enough for now.
        (pretty-print-columns 'infinity)
        (pretty-display (handle->list tbl handle union-elements-equal?
                                      #f) ;; #t to show handles
                        out)
        (get-output-string out))))
  
  (define/contract handle->string-old
    (hashcons-table? handle? (handle? handle? . -> . boolean?) . -> . string?)
    (lambda (tbl handle union-elements-equal?)
      (letrec
          ([handle->var (make-hash-table)]
           [handle->binding (make-hash-table)]
           [get-next-var! (let ([*i* 0]) (lambda (handle)
                                           (let ([new-i (+ *i* 1)]
                                                 [str (format "a~a" *i*)])
                                             (set! *i* (+ *i* 1))
                                             (hash-table-put! handle->var handle str)
                                             str)))]
           [type->string
            (lambda (type ancest)
              (match type
                [($ type-empty)
                 "_"]
                [($ type-cst type)
                 (string:expr->string type)]
                [($ type-struct-type label)
                 (string-append "#<struct-type:" (symbol->string (label-struct-type-name label)) ">")]
                [($ type-cons hd tl)
                 (string-append "(cons " (loop hd ancest) " " (loop tl ancest) ")")]
                [($ type-case-lambda rest-arg?s req-args argss exps)
                 (string-append "(case-lambda "
                                (foldr-case-lambda-vector (lambda (rest-arg? req-arg args exp acc)
                                                            (string-append
                                                             "["
                                                             (foldr-vector (lambda (arg acc)
                                                                             (string-append (loop arg ancest) " " acc))
                                                                           (if rest-arg? "*-> " "-> ")
                                                                           args)
                                                             (loop exp ancest) "]" acc))
                                                          ")"
                                                          rest-arg?s req-args argss exps))]
                [($ type-promise value)
                 (string-append "(promise " (loop value ancest) ")")]
                [($ type-struct-value label types)
                 (string-append "#(struct:"
                                (symbol->string (if (label-struct-type? label)  (label-struct-type-name label) label))
                                " "
                                (list:foldr
                                 (lambda (elt-type str)
                                   (string-append
                                    (loop elt-type ancest)
                                    (if (string=? str ")") ""  " ")
                                    str))
                                 ")"
                                 types))]
                [($ type-values values-type)
                 (cond
                   [(type-empty? values-type)
                    (loop values-type ancest)]
                   [(and (type-cst? values-type) (eq? (type-cst-type values-type) 'top))
                    (loop values-type ancest)]
                   [else
                    (string-append "(values " (loop values-type ancest) ")")])]
                [($ type-vector element)
                 (string-append "(vector " (loop element ancest) ")")]
                [($ type-union elements)
                 (let* ([els (list:foldr (lambda (x ys)
                                           (if (ormap (lambda (y)
                                                        (union-elements-equal? x y)) ys)
                                               ys
                                               (cons x ys)))
                                         '()
                                         elements)]
                        [els (list:foldl (lambda (x ys)
                                           (if (ormap (lambda (y)
                                                        (union-elements-equal? x y)) ys)
                                               ys
                                               (cons x ys)))
                                         '()
                                         els)])
                   (if (length-one? els)
                       (loop (car els) ancest)
                       (string-append "(union"
                                      (list:foldr (lambda (el acc)
                                                    (string-append " " (loop el ancest) acc))
                                                  ")"
                                                  elements))))]
                [else (error 'hashcons-type-string "~a Not implemented yet" type)]))]
           [loop (lambda (handle ancest)
                   (if (set-in? ancest handle) ;; if we've already come across this node
                       ;; Add a back reference
                       (if (hash-table-get handle->var handle cst:thunk-false)
                           (hash-table-get handle->var handle)
                           (get-next-var! handle))
                       (let* ([type (get-type tbl handle)]
                              [str (type->string type (set-set ancest handle))])
                         (set-remove ancest handle) ;; imperative sets
                         (if (hash-table-has-key? handle->var handle)
                             (begin
                               (hash-table-put! handle->binding handle str)
                               (hash-table-get handle->var handle))
                             str))))])
        (let* ([rec-body (loop handle (set-make))]
               [var-bindings
                (list:foldr
                 (lambda (cur acc) (string-append cur (if (string=? acc "") "" "\n") acc)) ""
                 (hash-table-map handle->var
                                 (lambda (handle var)
                                   (string-append (number->string handle) "[" var " "
                                                  (hash-table-get handle->binding handle
                                                                  (lambda () (error 'handle->string
                                                                                    "No binding for var handle ~a" handle)))
                                                  "]"))))])
          (format "~s:~a" handle
                  (if (string=? "" var-bindings) rec-body
                      (string-append "(rec-type (" var-bindings ") " rec-body ")")))))))
  
  (define handle->list 
    (opt-lambda (tbl handle union-elements-equal? [show-handles #t])
      (letrec
          ([handle->var (make-hash-table)]
           [handle->binding (make-hash-table)]
           [get-next-var! (let ([i 0]) (lambda (handle)
                                         (let ([str (string->symbol (format "a~a" i))])
                                           (set! i (add1 i))
                                           (hash-table-put! handle->var handle str)
                                           str)))]
           [handlify
            (lambda (str handle) ;(any/c handle? . -> . (or/c symbol? (cons/p symbol? any/c)))
              (let* ([first (if (list? str) (car str) str)]
                     [first-handle
                      (string->symbol
                       (let ([str (cond [(string? first) first]
                                        [else (string:expr->string first)])])
                         (if show-handles
                             (string-append str ":" (string:expr->string handle))
                             str)))])
                (if (list? str) (cons first-handle (cdr str)) first-handle)))]
           [type->list
            (lambda (type ancest)
              (match type
                [($ type-empty) '_]
                [($ type-cst type)
                 (cond [(null? type) 'null]
                       [(symbol? type) type]
                       [(boolean? type) type]
                       [(number? type) type]
                       [else (string->symbol (string:expr->string type))])]
                [($ type-struct-type label)
                 (string->symbol (string-append
                                  "#<struct-type:"
                                  (symbol->string (label-struct-type-name label)) ">"))]
                [($ type-cons hd tl)
                 (list 'cons (loop hd ancest) (loop tl ancest))]
                [($ type-case-lambda rest-arg?s req-args argss exps)
                 (list 'case-lambda 
                       (foldr-case-lambda-vector
                        (lambda (rest-arg? req-arg args exp acc)
                          (cons
                           (append
                            (map (lambda (arg) (loop arg ancest)) (vector->list args))
                            (list (if rest-arg? '*-> '->)
                                  (loop exp ancest)))
                           acc))
                        null
                        rest-arg?s req-args argss exps))]
                [($ type-promise value)
                 (list 'promise (loop value ancest))]
                [($ type-struct-value label types)
                 (list (string->symbol
                        (string-append
                         "#(struct:"
                         (symbol->string
                          (if (label-struct-type? label)  (label-struct-type-name label) label))))
                       (map (lambda (ty) (loop ty ancest)) types))]
                [($ type-values values-type)
                 (cond
                   [(type-empty? values-type)
                    (loop values-type ancest)]
                   [(and (type-cst? values-type) (eq? (type-cst-type values-type) 'top))
                    (loop values-type ancest)]
                   [else
                    (list 'values (loop values-type ancest))])]
                [($ type-vector element)
                 (list 'vector (loop element ancest))]
                [($ type-union elements)
                 (let* ([simplify
                         (lambda (x ys)
                           (let ([can-drop-x-for?
                                  (lambda (y)
                                    (if (= x y)
                                        #f 
                                        (union-elements-equal? x y)))])
                             (if (ormap can-drop-x-for? ys) ys (cons x ys))))]
                        [els (list:foldr simplify null elements)]
                        [els (list:foldl simplify null els)])
                   (let ([retme
                          (cond
                            [show-handles
                             (cons 'union (map (lambda (x) (loop x ancest)) els))]
                            [(> (length els) 1)
                             (cons 'union (map (lambda (x) (loop x ancest)) els))]
                            [(= 1 (length els))
                             (loop (car els) ancest)]
                            [else (error 'union-without-elemetns
                                         "elements=~a els=~a" elements els)])])
                     retme)
                   )]
                [else (error 'handle->list "Unmatched type: ~a" type)]))]
           [loop (lambda (handle ancest) ;(handle? any/c . -> . any)
                   (if (memq handle ancest) ;; if we've already come across this node
                       ;; Add a back reference
                       (if (hash-table-has-key? handle->var handle)
                           (hash-table-get handle->var handle)
                           (get-next-var! handle))
                       (let* ([type (get-type tbl handle)]
                              [str (type->list type (cons handle ancest))])
                         (if (hash-table-has-key? handle->var handle)
                             (begin
                               (hash-table-put! handle->binding handle (handlify str handle))
                               (hash-table-get handle->var handle))
                             (handlify str handle)))))]) ;; changed here
        (let* ([rec-body (loop handle null)]
               [var-bindings
                (hash-table-map handle->var
                                (lambda (handle var)
                                  (list var (hash-table-get handle->binding handle))))])
          (if (null? var-bindings) rec-body
              (list (handlify 'rec-type handle) var-bindings rec-body))))))
  
  
  ;;
  ;; Graph algorithms
  ;;
  
  ;; get a list of strongly connected components in reverse
  ;; topological order, taken from CLR
  (define/contract strongly-connected-components
    (hash-table? . ->d .
                 (lambda (h)
                   (lambda (list-of-sccs)
                     (= (hash-table-size h) (apply + (map length list-of-sccs))))))
    (lambda (graph)
      (letrec
          ;; finished nodes from most recently finished to first finished
          ([finished-nodes (box ())]  
           [color (make-hash-table)]
           
           [transpose-graph
            (lambda (graph)
              (let ([new-graph (make-hash-table)])
                (hash-table-for-each graph (lambda (node adj-list)
                                             (hash-table-put! new-graph node null)))
                (hash-table-for-each graph (lambda (node adj-list)
                                             (for-each (lambda (adj-node)
                                                         (hash-table-put! new-graph adj-node
                                                                          (cons node (hash-table-get new-graph adj-node))))
                                                       adj-list)))
                new-graph))]
           [dfs-visit (lambda (graph u visited-nodes)
                        (hash-table-put! color u 'gray)
                        (let* ([adj (hash-table-get graph u)]
                               [new-nodes
                                (list:foldl (lambda (v visited)
                                              (if (eq? (hash-table-get color v) 'white)
                                                  (dfs-visit graph v visited)
                                                  visited))
                                            visited-nodes
                                            adj)])
                          (hash-table-put! color u 'black)
                          (set-box! finished-nodes (cons u (unbox finished-nodes)))
                          (cons u new-nodes)))]
           [dfs (lambda (graph nodes-to-visit)
                  (hash-table-for-each graph
                                       (lambda (u _) (hash-table-put! color u 'white)))
                  (let ([sccs (list:foldl (lambda (u sccs)
                                            (if (eq? (hash-table-get color u) 'white)
                                                (cons (dfs-visit graph u null) sccs)
                                                sccs))
                                          '()
                                          nodes-to-visit)])
                    sccs))])
        (dfs graph (hash-table-map graph (lambda (k adj) k)))
        (dfs (transpose-graph graph) (unbox finished-nodes)))))
  )
