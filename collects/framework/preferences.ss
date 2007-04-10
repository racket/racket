
#|

There are three attributes for each preference:

  - default set, or not
  - marshalling function set, or not
  - initialization still okay, or not

the state transitions / contracts are:

  get(true, _, _) -> (true, _, false)
  get(false, _, _) -> error default not yet set

  set is just like get.

  set-default(false, _, true) -> set-default(true, _, true)
  set-default(true, _, _) -> error default already set
  set-default(_, _, false) -> initialization not okay anymore  /* cannot happen, I think */

  set-un/marshall(true, false, true) -> (true, true, true)
  .. otherwise error

  for all syms: 
   prefs-snapshot(_, _, _) -> (_, _, false) 

|#

(module preferences mzscheme
  (require (lib "file.ss")
	   (lib "etc.ss")
           (lib "contract.ss"))
  
  (provide exn:struct:unknown-preference)
  
  (define-struct (exn:unknown-preference exn) ())
  
  ;; these two names are for consistency
  (define exn:make-unknown-preference make-exn:unknown-preference)
  (define exn:struct:unknown-preference struct:exn:unknown-preference)
  
  (define-syntax (provide/contract/docs stx)
    (syntax-case stx ()
      [(_ (name contract docs ...) ...)
       (syntax (provide/contract (name contract) ...))]))

  
  (define old-preferences-symbol 'plt:framework-prefs)
  (define old-preferences (make-hash-table))
  (let ([old-prefs (get-preference old-preferences-symbol (λ () '()))])
    (for-each
     (λ (line) (hash-table-put! old-preferences (car line) (cadr line)))
     old-prefs))
  
  (define (add-pref-prefix p) (string->symbol (format "plt:framework-pref:~a" p)))
  
  ;; preferences : hash-table[sym -o> any]
  ;; the current values of the preferences
  (define preferences (make-hash-table))
  
  ;; marshalled : hash-table[sym -o> any]
  ;; the values of the preferences, as read in from the disk
  ;; each symbol will only be mapped in one of the preferences
  ;; hash-table and this hash-table, but not both.
  (define marshalled (make-hash-table))
  
  ;; marshall-unmarshall : sym -o> un/marshall
  (define marshall-unmarshall (make-hash-table))
  
  ;; callbacks : sym -o> (listof (sym TST -> boolean))
  (define callbacks (make-hash-table))
  
  ;; defaults : hash-table[sym -o> default]
  (define defaults (make-hash-table))
  
  ;; these four functions determine the state of a preference
  (define (pref-un/marshall-set? pref) (hash-table-bound? marshall-unmarshall pref))
  (define (pref-default-set? pref) (hash-table-bound? defaults pref))
  (define (pref-can-init? pref) 
    (and (not snapshot-grabbed?)
         (not (hash-table-bound? preferences pref))))
  
  ;; type un/marshall = (make-un/marshall (any -> prinable) (printable -> any))
  (define-struct un/marshall (marshall unmarshall))
  
  ;; type pref = (make-pref any)
  (define-struct pref (value))
  
  ;; type default  = (make-default any (any -> bool))
  (define-struct default (value checker))
  
  ;; pref-callback : (make-pref-callback (union (weak-box (sym tst -> void)) (sym tst -> void)))
  ;; this is used as a wrapped to deal with the problem that different procedures might be eq?.
  (define-struct pref-callback (cb))
  
  ;; get : symbol -> any
  ;; return the current value of the preference `p'
  ;; exported
  (define (preferences:get p)
    (cond
      [(pref-default-set? p)
       
       ;; unmarshall, if required
       (when (hash-table-bound? marshalled p)
         ;; if `preferences' is already bound, that means the unmarshalled value isn't useful.
         (unless (hash-table-bound? preferences p)
           (hash-table-put! preferences p (unmarshall-pref p (hash-table-get marshalled p))))
         (hash-table-remove! marshalled p))
       
       ;; if there is no value in the preferences table, but there is one
       ;; in the old version preferences file, take that:
       (unless (hash-table-bound? preferences p)
         (when (hash-table-bound? old-preferences p)
           (hash-table-put! preferences p (unmarshall-pref p (hash-table-get old-preferences p)))))
       
       ;; clear the pref from the old table (just in case it was taking space -- we don't need it anymore)
       (when (hash-table-bound? old-preferences p)
         (hash-table-remove! old-preferences p))
       
       ;; if it still isn't set, take the default value
       (unless (hash-table-bound? preferences p)
         (hash-table-put! preferences p (default-value (hash-table-get defaults p))))
       
       (hash-table-get preferences p)]
      [(not (pref-default-set? p))
       (raise-unknown-preference-error
        'preferences:get
        "tried to get a preference but no default set for ~e"
        p)]))
  
  ;; set : symbol any -> void
  ;; updates the preference
  ;; exported
  (define (preferences:set p value) (multi-set (list p) (list value)))
  
  ;; set : symbol any -> void
  ;; updates the preference
  ;; exported

  (define (multi-set ps values)
    (for-each
     (λ (p value)
       (cond
         [(pref-default-set? p)
          (let ([default (hash-table-get defaults p)])
            (unless ((default-checker default) value)
              (error 'preferences:set
                     "tried to set preference ~e to ~e but it does not meet test from preferences:set-default"
                     p value))
            (check-callbacks p value)
            (hash-table-put! preferences p value)
            (void))]
         [(not (pref-default-set? p))
          (raise-unknown-preference-error
           'preferences:set "tried to set the preference ~e to ~e, but no default is set"
           p
           value)]))
     ps values)
    
    ((preferences:low-level-put-preferences)
     (map add-pref-prefix ps) 
     (map (λ (p value) (marshall-pref p value))
          ps
          values))
    
    (void))
  
  (define preferences:low-level-put-preferences (make-parameter put-preferences))
  
  (define (raise-unknown-preference-error sym fmt . args)
    (raise (exn:make-unknown-preference
            (string-append (format "~a: " sym) (apply format fmt args))
            (current-continuation-marks))))

  ;; unmarshall-pref : symbol marshalled -> any
  ;; unmarshalls a preference read from the disk
  (define (unmarshall-pref p data)
    (let/ec k
      (let* ([unmarshall-fn (un/marshall-unmarshall
                             (hash-table-get marshall-unmarshall
                                             p
                                             (λ () (k data))))]
             [default (hash-table-get defaults p)]
             [result (unmarshall-fn data)])
        (if ((default-checker default) result)
            result
            (default-value default)))))
  
  ;; add-callback : sym (-> void) -> void
  (define preferences:add-callback 
    (opt-lambda (p callback [weak? #f])
      (let ([new-cb (make-pref-callback (if weak?
                                            (make-weak-box callback)
                                            callback))])
        (hash-table-put! callbacks
                         p 
                         (append 
                          (hash-table-get callbacks p (λ () null))
                          (list new-cb)))
        (λ ()
          (hash-table-put!
           callbacks
           p
           (let loop ([callbacks (hash-table-get callbacks p (λ () null))])
             (cond
               [(null? callbacks) null]
               [else 
                (let ([callback (car callbacks)])
                  (cond
                    [(eq? callback new-cb)
                     (loop (cdr callbacks))]
                    [else
                     (cons (car callbacks) (loop (cdr callbacks)))]))])))))))
  
  ;; check-callbacks : sym val -> void
  (define (check-callbacks p value)
    (let ([new-callbacks
           (let loop ([callbacks (hash-table-get callbacks p (λ () null))])
             (cond
               [(null? callbacks) null]
               [else 
                (let* ([callback (car callbacks)]
                       [cb (pref-callback-cb callback)])
                  (cond
                    [(weak-box? cb)
                     (let ([v (weak-box-value cb)])
                       (if v
                           (begin 
                             (v p value)
                             (cons callback (loop (cdr callbacks))))
                           (begin
                             (printf "lost a ~s callback\n" p)
                             (loop (cdr callbacks)))))]
                    [else
                     (cb p value)
                     (cons callback (loop (cdr callbacks)))]))]))])
      (if (null? new-callbacks)
          (hash-table-remove! callbacks p)
          (hash-table-put! callbacks p new-callbacks))))
  
  (define (preferences:set-un/marshall p marshall unmarshall)
    (cond
      [(and (pref-default-set? p)
            (not (pref-un/marshall-set? p))
            (pref-can-init? p))
       (hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))]
      [(not (pref-default-set? p))
       (error 'preferences:set-un/marshall
              "must call set-default for ~s before calling set-un/marshall for ~s"
              p p)]
      [(pref-un/marshall-set? p)
       (error 'preferences:set-un/marshall
              "already set un/marshall for ~e" 
              p)]
      [(not (pref-can-init? p))
       (error 'preferences:set-un/marshall "the preference ~e cannot be configured any more" p)]))
  
  (define (hash-table-bound? ht s)
    (let/ec k
      (hash-table-get ht s (λ () (k #f)))
      #t))
  
  (define (preferences:restore-defaults)
    (hash-table-for-each
     defaults
     (λ (p def) (preferences:set p (default-value def)))))
  
  ;; set-default : (sym TST (TST -> boolean) -> void
  (define (preferences:set-default p default-value checker)
    (cond
      [(and (not (pref-default-set? p))
            (pref-can-init? p))
       (let ([default-okay? (checker default-value)])
         (unless default-okay?
           (error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n"
                  p checker default-okay? default-value))
         (hash-table-put! defaults p (make-default default-value checker))
         (let/ec k
           (let ([m (get-preference (add-pref-prefix p) (λ () (k (void))))])
             ;; if there is no preference saved, we just don't do anything.
             ;; `get' notices this case.
             (hash-table-put! marshalled p m))))]
      [(not (pref-can-init? p))
       (error 'preferences:set-default
              "tried to call set-default for preference ~e but it cannot be configured any more"
              p)]
      [(pref-default-set? p)
       (error 'preferences:set-default
              "preferences default already set for ~e" p)]
      [(not (pref-can-init? p))
       (error 'preferences:set-default
              "can no longer set the default for ~e" p)]))
  
  ;; marshall-pref : symbol any -> (list symbol printable)
  (define (marshall-pref p value)
    (let/ec k
      (let* ([marshaller
              (un/marshall-marshall
               (hash-table-get marshall-unmarshall p (λ () (k value))))])
        (marshaller value))))
  
  (define-struct preferences:snapshot (x))
  (define snapshot-grabbed? #f)
  (define (preferences:get-prefs-snapshot)
    (set! snapshot-grabbed? #t)
    (make-preferences:snapshot (hash-table-map defaults (λ (k v) (cons k (preferences:get k))))))
    
  (define (preferences:restore-prefs-snapshot snapshot)
    (multi-set (map car (preferences:snapshot-x snapshot))
               (map cdr (preferences:snapshot-x snapshot)))
    (void))
  
  
  (provide/contract/docs
   (preferences:snapshot? 
    (-> any/c boolean?)
    (arg)
    "Determines if its argument is a preferences snapshot."
    ""
    "See also "
    "@flink preferences:get-prefs-snapshot"
    " and "
    "@flink preferences:restore-prefs-snapshot %"
    ".")
   (preferences:restore-prefs-snapshot 
    (-> preferences:snapshot? void?)
    (snapshot)
    "Restores the preferences saved in \\var{snapshot}."
    ""
    "See also "
    "@flink preferences:get-prefs-snapshot %"
    ".")
   
   (preferences:get-prefs-snapshot 
    (-> preferences:snapshot?)
    ()
    "Caches all of the current values of the preferences and returns them."
    ""
    "See also "
    "@flink preferences:restore-prefs-snapshot %"
    ".")
   
   (exn:make-unknown-preference 
    (string? continuation-mark-set? . -> . exn:unknown-preference?)
    (message continuation-marks)
    "Creates an unknown preference exception.")
   (exn:unknown-preference? 
    (any/c . -> . boolean?)
    (exn)
    "Determines if a value is an unknown preference exn.")

   (preferences:low-level-put-preferences
    any/c
    ()
    "This is a parameter (see "
    "\\Mzhyperref{parameters}{mz:parameters} for information about parameters)"
    "which is called when a preference is saved. Its interface should "
    "be just like mzlib's \\scheme|put-preference|.")
   
   (preferences:get
    (symbol? . -> . any/c)
    (symbol)
    "See also"
    "@flink preferences:set-default %"
    "."
    ""
    "\\rawscm{preferences:get} returns the value for the preference"
    "\\var{symbol}. It raises"
    "\\scmindex{exn:unknown-preference}\\rawscm{exn:unknown-preference}"
    "if the preference's default has not been set.")
   (preferences:set
    (symbol? any/c . -> . void?)
    (symbol value)
    "See also"
    "@flink preferences:set-default %"
    "."
    ""
    "\\rawscm{preferences:set-preference} sets the preference"
    "\\var{symbol} to \\var{value}. This should be called when the"
    "users requests a change to a preference."
    ""
    "This function immediately writes the preference value to disk."
    ""
    "It raises"
    "\\scmindex{exn:unknown-preference}\\rawscm{exn:unknown-preference}"
    "if the preference's default has not been set.")
   (preferences:add-callback
    (opt-> (symbol? 
            
            ;; important that this arg only has a flat contract
            ;; so that no wrapper is created, so that
            ;; the weak box stuff works ...
            (λ (x) (and (procedure? x) (procedure-arity-includes? x 2))))
           (boolean?)
           (-> void?))
    ((p f)
     ((weak? #f)))
    "This function adds a callback which is called with a symbol naming a"
    "preference and it's value, when the preference changes."
    "\\rawscm{preferences:add-callback} returns a thunk, which when"
    "invoked, removes the callback from this preference."
    ""
    "If \\var{weak?} is true, the preferences system will only hold on to"
    "the callback weakly."
    ""
    "The callbacks will be called in the order in which they were added."
    ""
    "If you are adding a callback for a preference that requires"
    "marshalling and unmarshalling, you must set the marshalling and"
    "unmarshalling functions by calling"
    "\\iscmprocedure{preferences:set-un/marshall} before adding a callback."
    ""
    "This function raises"
    "\\scmindex{exn:unknown-preference}\\rawscm{exn:unknown-preference}"
    "if the preference has not been set.")
   (preferences:set-default
    (symbol? any/c (any/c . -> . any) . -> . void?)
    (symbol value test)
    "This function must be called every time your application starts up, before any call to"
    "@flink preferences:get %"
    ", "
    "@flink preferences:set"
    "(for any given preference)."
    ""
    "If you use"
    "@flink preferences:set-un/marshall %"
    ", you must call this function before calling it."
    ""
    "This sets the default value of the preference \\var{symbol} to"
    "\\var{value}. If the user has chosen a different setting,"
    "the user's setting"
    "will take precedence over the default value."
    ""
    "The last argument, \\var{test} is used as a safeguard. That function is"
    "called to determine if a preference read in from a file is a valid"
    "preference. If \\var{test} returns \\rawscm{\\#t}, then the preference is"
    "treated as valid. If \\var{test} returns \\rawscm{\\#f} then the default is"
    "used.")
   (preferences:set-un/marshall
    (symbol? (any/c . -> . printable/c) (printable/c . -> . any/c) . -> . void?)
    (symbol marshall unmarshall)
    "\\rawscm{preference:set-un/marshall} is used to specify marshalling and"
    "unmarshalling functions for the preference"
    "\\var{symbol}. \\var{marshall} will be called when the users saves their"
    "preferences to turn the preference value for \\var{symbol} into a"
    "printable value. \\var{unmarshall} will be called when the user's"
    "preferences are read from the file to transform the printable value"
    "into it's internal representation. If \\rawscm{preference:set-un/marshall}"
    "is never called for a particular preference, the values of that"
    "preference are assumed to be printable."
    ""
    "If the unmarshalling function returns a value that does not meet the"
    "guard passed to "
    "@flink preferences:set-default"
    "for this preference, the default value is used."
    ""
    "The \\var{marshall} function might be called with any value returned"
    "from \\scheme{read} and it must not raise an error (although it"
    "can return arbitrary results if it gets bad input). This might"
    "happen when the preferences file becomes corrupted, or is edited"
    "by hand."
    ""
    "\\rawscm{preference:set-un/marshall} must be called before calling"
    "@flink preferences:get %"
    ", "
    "@flink preferences:set %"
    ".")
   
   (preferences:restore-defaults
    (-> void?)
    ()
    "\\rawscm{(preferences:restore-defaults)} restores the users's configuration to the"
    "default preferences.")))
