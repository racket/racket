#lang at-exp racket/base
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

(require scribble/srcdoc
         racket/contract/base racket/file)
(require/doc racket/base scribble/manual (for-label racket/serialize))

(define-struct (exn:unknown-preference exn) ())

;; these two names are for consistency
(define exn:make-unknown-preference make-exn:unknown-preference)
(define exn:struct:unknown-preference struct:exn:unknown-preference)

(define preferences:low-level-put-preferences (make-parameter put-preferences))
(define preferences:low-level-get-preference  (make-parameter get-preference))

(define (add-pref-prefix p) (string->symbol (format "plt:framework-pref:~a" p)))

;; preferences : hash-table[sym -o> any]
;; the current values of the preferences
(define preferences (make-hasheq))

;; marshall-unmarshall : sym -o> un/marshall
(define marshall-unmarshall (make-hasheq))

;; callbacks : sym -o> (listof (sym TST -> boolean))
(define callbacks (make-hasheq))

;; defaults : hash-table[sym -o> default]
(define defaults (make-hasheq))

;; these four functions determine the state of a preference
(define (pref-un/marshall-set? pref) (hash-has-key? marshall-unmarshall pref))
(define (preferences:default-set? pref) (hash-has-key? defaults pref))
(define (pref-can-init? pref)
  (and (not snapshot-grabbed?)
       (not (hash-has-key? preferences pref))))

;; type un/marshall = (make-un/marshall (any -> prinable) (printable -> any))
(define-struct un/marshall (marshall unmarshall))

;; type pref = (make-pref any)
(define-struct pref (value))

;; type default  = (make-default any (-> any bool) (listof symbol) (listof (-> any any)))
(define-struct default (value checker aliases rewrite-aliases))

;; pref-callback : (make-pref-callback (union (weak-box (sym tst -> void)) (sym tst -> void)))
;; this is used as a wrapped to deal with the problem that different procedures might be eq?.
(define-struct pref-callback (cb))

;; used to detect missing hash entries
(define none (gensym 'none))

;; get : symbol -> any
;; return the current value of the preference `p'
;; exported
(define (preferences:get p)
  (define v (hash-ref preferences p none))
  (cond
    ;; if this is found, we can just return it immediately
    [(not (eq? v none))
     v]
    ;; first time reading this, check the file & unmarshall value, if
    ;; it's not there, use the default
    [(preferences:default-set? p)
     (let* (;; try to read the preference from the preferences file
            [v (read-pref-from-file p)]
            [v (if (eq? v none)
                 ;; no value read, take the default value
                 (default-value (hash-ref defaults p))
                 ;; found a saved value, unmarshall it
                 (unmarshall-pref p v))])
       ;; set the value for future reference and return it
       (hash-set! preferences p v)
       v)]
    [(not (preferences:default-set? p))
     (raise-unknown-preference-error
      'preferences:get
      "tried to get a preference but no default set for ~e"
      p)]))

;; read-pref-from-file : symbol -> (or/c any none)
;; reads the preference saved in the low-level preferences
;; file, first checking 'p' and then checking the aliases (in order)
(define (read-pref-from-file p)
  (let ([defaults (hash-ref defaults p)])
    (let loop ([syms (cons p (default-aliases defaults))]
               [rewriters (cons values (default-rewrite-aliases defaults))])
      (cond
        [(null? syms) none]
        [else
         (let/ec k
           ((car rewriters)
            ((preferences:low-level-get-preference)
             (add-pref-prefix (car syms))
             (lambda () (k (loop (cdr syms) (cdr rewriters)))))))]))))

;; set : symbol any -> void
;; updates the preference
;; exported
(define (preferences:set p value) (multi-set (list p) (list value)))

;; set : symbol any -> void
;; updates the preference
;; exported
(define (multi-set ps values)
  (dynamic-wind
   (λ () 
     (call-pref-save-callbacks #t))
   (λ ()
     (for-each
      (λ (p value)
        (cond
          [(preferences:default-set? p)
           (define default (hash-ref defaults p))
           (define checker? (default-checker default))
           (unless (checker? value)
             (error 'preferences:set
                    (string-append
                     "new value doesn't satisfy preferences:set-default predicate\n"
                     "  pref sym: ~v\n"
                     "  given: ~e\n"
                     "  predicate: ~e")
                     p value checker?))
             (check-callbacks p value)
             (hash-set! preferences p value)]
          [(not (preferences:default-set? p))
           (raise-unknown-preference-error
            'preferences:set
            (string-append
             "cannot set preference before setting default"
             "  pref sym: ~e\n"
             "  given: ~e")
            p
            value)]))
      ps values)
     ((preferences:low-level-put-preferences)
      (map add-pref-prefix ps)
      (map (λ (p value) (marshall-pref p value))
           ps
           values))
     (void))
   (λ ()
     (call-pref-save-callbacks #f))))

(define pref-save-callbacks '())

(define (preferences:register-save-callback f)
  (define key (gensym))
  (set! pref-save-callbacks (cons (list key f) pref-save-callbacks))
  key)

(define (preferences:unregister-save-callback k)
  (set! pref-save-callbacks
        (let loop ([callbacks pref-save-callbacks])
          (cond
            [(null? callbacks) '()]
            [else
             (let ([cb (car callbacks)])
               (if (eq? (list-ref cb 0) k)
                   (cdr callbacks)
                   (cons cb (loop (cdr callbacks)))))]))))

(define (call-pref-save-callbacks b)
  (for ([cb (in-list pref-save-callbacks)])
    ((list-ref cb 1) b)))

(define (raise-unknown-preference-error sym fmt . args)
  (raise (exn:make-unknown-preference
          (string-append (format "~a: " sym) (apply format fmt args))
          (current-continuation-marks))))

;; add-callback : sym (-> void) -> void
(define preferences:add-callback 
  (lambda (p callback [weak? #f])
    (let ([new-cb (make-pref-callback (if weak?
                                          (impersonator-ephemeron callback)
                                          callback))])
      (hash-set! callbacks
                 p 
                 (append 
                  (hash-ref callbacks p '())
                  (list new-cb)))
      (λ ()
        (hash-set!
         callbacks
         p
         (let loop ([callbacks (hash-ref callbacks p '())])
           (cond
             [(null? callbacks) '()]
             [else 
              (let ([callback (car callbacks)])
                (cond
                  [(eq? callback new-cb)
                   (loop (cdr callbacks))]
                  [else
                   (cons (car callbacks) (loop (cdr callbacks)))]))])))))))

;; check-callbacks : sym val -> void
(define (check-callbacks p value)
  (define new-callbacks
    (let loop ([callbacks (hash-ref callbacks p '())])
      (cond
        [(null? callbacks) null]
        [else 
         (define callback (car callbacks))
         (define cb (pref-callback-cb callback))
         (cond
           [(ephemeron? cb)
            (define v (ephemeron-value cb))
            (cond
              [v
               (v p value)
               (cons callback (loop (cdr callbacks)))]
              [else
               (loop (cdr callbacks))])]
           [else
            (cb p value)
            (cons callback (loop (cdr callbacks)))])])))
  (if (null? new-callbacks)
      (hash-remove! callbacks p)
      (hash-set! callbacks p new-callbacks)))

(define (preferences:set-un/marshall p marshall unmarshall)
  (cond
    [(and (preferences:default-set? p)
          (not (pref-un/marshall-set? p))
          (pref-can-init? p))
     (hash-set! marshall-unmarshall p (make-un/marshall marshall unmarshall))]
    [(not (preferences:default-set? p))
     (error 'preferences:set-un/marshall
            "must call set-default for ~s before calling set-un/marshall for ~s"
            p p)]
    [(pref-un/marshall-set? p)
     (error 'preferences:set-un/marshall
            "already set un/marshall for ~e" 
            p)]
    [(not (pref-can-init? p))
     (error 'preferences:set-un/marshall "the preference ~e cannot be configured any more" p)]))

(define (preferences:restore-defaults)
  (hash-for-each
   defaults
   (λ (p def) (preferences:set p (default-value def)))))

;; set-default : (sym TST (TST -> boolean) -> void
(define (preferences:set-default p default-value checker 
                                 #:aliases [aliases '()]
                                 #:rewrite-aliases [rewrite-aliases (map (lambda (x) values) aliases)])
  (cond
    [(and (not (preferences:default-set? p))
          (pref-can-init? p))
     (define default-okay? (checker default-value))
     (unless default-okay?
       (error 'set-default 
              (string-append
               "checker doesn't match default\n"
               "  default: ~e\n"
               "  pref sym: ~e\n"
               "  checker: ~e")
              p default-value checker))

     (unless (= (length aliases) (length rewrite-aliases))
       (error 'preferences:set-default 
	      "expected equal length lists for the #:aliases and #:rewrite-aliases arguments, got ~e and ~e"
	      aliases rewrite-aliases))
     (hash-set! defaults p (make-default default-value checker aliases rewrite-aliases))]
    [(not (pref-can-init? p))
     (error 'preferences:set-default
            "tried to call set-default for preference ~e but it cannot be configured any more"
            p)]
    [(preferences:default-set? p)
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
             (hash-ref marshall-unmarshall p (λ () (k value))))])
      (marshaller value))))

;; unmarshall-pref : symbol marshalled -> any
;; unmarshalls a preference read from the disk
(define (unmarshall-pref p data)
  (let* ([un/marshall (hash-ref marshall-unmarshall p #f)]
         [result (if un/marshall
                     ((un/marshall-unmarshall un/marshall) data)
                     data)]
         [default (hash-ref defaults p)])
    (if ((default-checker default) result)
        result
        (default-value default))))

;; copy-pref-value : sym any -> any
;; uses the marshalling code to copy a preference. If there
;; is not marshaller set, then no copying happens.
(define (copy-pref-value p value)
  (let/ec k
    (let* ([un/marshaller (hash-ref marshall-unmarshall p (λ () (k value)))]
           [default (hash-ref defaults p)]
           [marsh (un/marshall-marshall un/marshaller)]
           [unmarsh (un/marshall-unmarshall un/marshaller)]
           [marshalled (marsh value)]
           [copy (unmarsh marshalled)])
      (if ((default-checker default) copy)
          copy
          value))))
  
(define-struct preferences:snapshot (x))
(define snapshot-grabbed? #f)
(define (preferences:get-prefs-snapshot)
  (set! snapshot-grabbed? #t)
  (make-preferences:snapshot 
   (hash-map defaults 
             (λ (k v) (cons k (copy-pref-value k (preferences:get k)))))))

(define (preferences:restore-prefs-snapshot snapshot)
  (multi-set (map car (preferences:snapshot-x snapshot))
             (map cdr (preferences:snapshot-x snapshot)))
  (void))


(provide/doc
 (proc-doc/names
  preferences:get
  (symbol? . -> . any/c)
  (symbol)
  @{See also @racket[preferences:set-default].
        
        @racket[preferences:get] returns the value for the preference
        @racket[symbol]. It raises an exception matching
        @racket[exn:unknown-preference?]
        if the preference's default has not been set.})

 (proc-doc/names
  preferences:set
  (symbol? any/c . -> . void?)
  (symbol value)
  @{Sets the preference
    @racket[symbol] to @racket[value]. It should be called when the
    users requests a change to a preference.
    
    @racket[preferences:set] immediately writes the preference value to disk.    
    It raises an exception matching
    @racket[exn:unknown-preference?]
    if the preference's default has not been set.
    
    See also @racket[preferences:set-default].})
 
 (proc-doc/names
  preferences:add-callback
  (->* (symbol? (-> symbol? any/c any)) 
       (boolean?)
       (-> void?))
  ((p f)
   ((weak? #f)))
  @{This function adds a callback which is called with a symbol naming a
    preference and its value, when the preference changes.
    @racket[preferences:add-callback] returns a thunk, which when
    invoked, removes the callback from this preference.
    
    If @racket[weak?] is true, the preferences system will only hold on to
    the callback weakly.
    
    The callbacks will be called in the order in which they were added.
    
    If you are adding a callback for a preference that requires
    marshalling and unmarshalling, you must set the marshalling and
    unmarshalling functions by calling
    @racket[preferences:set-un/marshall] before adding a callback.
    
    This function raises an exception matching
    @racket[exn:unknown-preference?]
    if the preference has not been set.})
 (proc-doc/names
  preferences:set-default
  (->* (symbol? any/c (any/c . -> . any))
       (#:aliases (listof symbol?)
        #:rewrite-aliases (listof (-> any/c any)))
      void?)
  ((symbol value test)
   ((aliases '()) (rewrite-aliases (map (lambda (x) values) aliases))))
  @{This function must be called every time your application starts up, before
    any call to @racket[preferences:get] or @racket[preferences:set]
    (for any given preference).
    
    If you use @racket[preferences:set-un/marshall],
    you must call this function before calling it.
    
    This sets the default value of the preference @racket[symbol] to
    @racket[value]. If the user has chosen a different setting,
    the user's setting will take precedence over the default value.
    
    The @racket[test] argument is used as a safeguard. That function is
    called to determine if a preference read in from a file is a valid
    preference. If @racket[test] returns @racket[#t], then the preference is
    treated as valid. If @racket[test] returns @racket[#f] then the default is
    used.

    The @racket[aliases] and @racket[rewrite-aliases] arguments aids
    in renaming preferences. If @racket[aliases] is present, it is 
    expected to be a list of symbols that correspond to old versions
    of the preferences. It defaults to @racket['()]. If @racket[rewrite-aliases]
    is present, it is used to adjust the old values of the preferences
    when they are present in the saved file.})
 
 (proc-doc/names
  preferences:default-set?
  (-> symbol? boolean?)
  (pref)
  @{Returns @racket[#t] if @racket[pref] has been passed to
            @racket[preferences:set-default], @racket[#f]
            otherwise})

 (proc-doc/names
  preferences:set-un/marshall
  (symbol? (any/c . -> . printable/c) (printable/c . -> . any/c) . -> . void?)
  (symbol marshall unmarshall)
  @{@racket[preferences:set-un/marshall] is used to specify marshalling and
    unmarshalling functions for the preference
    @racket[symbol]. @racket[marshall] will be called when the users saves their
    preferences to turn the preference value for @racket[symbol] into a
    printable value. @racket[unmarshall] will be called when the user's
    preferences are read from the file to transform the printable value
    into its internal representation. If @racket[preferences:set-un/marshall]
    is never called for a particular preference, the values of that
    preference are assumed to be printable.
    
    If the unmarshalling function returns a value that does not meet the
    guard passed to @racket[preferences:set-default]
    for this preference, the default value is used.
    
    The @racket[marshall] function might be called with any value returned
    from @racket[read] and it must not raise an error 
    (although it can return arbitrary results if it gets bad input). This might
    happen when the preferences file becomes corrupted, or is edited
    by hand.
    
    @racket[preferences:set-un/marshall] must be called before calling
    @racket[preferences:get],@racket[preferences:set].

    See also @racket[serialize] and @racket[deserialize].
   })
 
 (proc-doc/names
  preferences:restore-defaults
  (-> void?)
  ()
  @{@racket[(preferences:restore-defaults)] restores the users' configuration
    to the default preferences.})
 
 (proc-doc/names
  preferences:register-save-callback
  (-> (-> boolean? any) symbol?)
  (callback)
  @{Registers @racket[callback] to run twice for each call to @racket[preferences:set]---once 
              before the preferences file is written, with @racket[#t], and once after it is written, with
              @racket[#f]. Registration returns a key for use with @racket[preferences:unregister-save-callback]. 
              Caveats:
              @itemize{@item{The callback occurs on whichever thread happened to call @racket[preferences:set].}
                       @item{Pre- and post-write notifications are not necessarily paired; unregistration
                             may cancel the post-write notification before it occurs.}}})
 
 (proc-doc/names
  preferences:unregister-save-callback
  (-> symbol? void?)
  (key)
  @{Unregisters the save callback associated with @racket[key].})
 
 (proc-doc/names
  exn:make-unknown-preference 
  (string? continuation-mark-set? . -> . exn:unknown-preference?)
  (message continuation-marks)
  @{Creates an unknown preference exception.})
 
 (proc-doc/names
  exn:unknown-preference? 
  (any/c . -> . boolean?)
  (exn)
  @{Determines if a value is an unknown preference exn.})
 
 (thing-doc
  exn:struct:unknown-preference
  struct-type?
  @{The struct type for the unknown preference exn.})

 
 (parameter-doc
  preferences:low-level-put-preferences
  (parameter/c ((listof symbol?) (listof any/c) . -> . any))
  put-preferences
  @{This parameter's value is called to save preference the preferences file.
    Its interface should be just like mzlib's @racket[put-preferences].
    
    The default value calls @racket[put-preferences] and, if there is an error,
    then starts using a hash-table to save the preferences instead.
    See also @racket[]})
 
 (parameter-doc
  preferences:low-level-get-preference
  (parameter/c (->* (symbol?) [(-> any)] any))
  get-preference
  @{This parameter's value is called to get a preference from the preferences
    file. Its interface should be just like @racket[get-preference].
    
    The default value calls @racket[get-preferences] and, if there is an error,
    then starts using a hash-table to save the preferences instead.})
  
 (proc-doc/names
  preferences:snapshot? 
  (-> any/c boolean?)
  (arg)
  @{Determines if its argument is a preferences snapshot.
    
    See also @racket[preferences:get-prefs-snapshot] and
    @racket[preferences:restore-prefs-snapshot].})
 (proc-doc/names
  preferences:restore-prefs-snapshot 
  (-> preferences:snapshot? void?)
  (snapshot)
  @{Restores the preferences saved in @racket[snapshot].
    
    See also @racket[preferences:get-prefs-snapshot].})
 
 (proc-doc/names
  preferences:get-prefs-snapshot 
  (-> preferences:snapshot?)
  ()
  @{Caches all of the current values of the preferences and returns them.
    For any preference that has marshalling and unmarshalling set
    (see @racket[preferences:set-un/marshall]), the preference value is
    copied by passing it through the marshalling and unmarshalling process.
    Other values are not copied, but references to them are instead saved.

    See also @racket[preferences:restore-prefs-snapshot].}))
