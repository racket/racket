#|

save needs contracts

showing the dialog needs preferences.

There are four attributes for each preference (technically, "read from 
disk" is global, but we can just think of it happening to each one 
independently, but simultaneously):

  - read from disk, or not
  - default set, or not
  - marshalling function set, or not
  - initialization still okay, or not

the state transitions / contracts are:

  get(true, true, _, _) -> (true, true, _, false)
  get(false, _, _, _) -> error not yet read from disk
  get(_, false, _, _) -> error default not yet set

  set is just like get.

  set-default(true, false, true, true) -> set-default(true, true, _, true)
  set-default(false, _, _, _) -> error not yet read from disk
  set-default(_, true, _, _) -> error default already set
  set-default(_, _, _, false) -> initialization not okay anymore  /* cannot happen, I think */

  set-un/marshall(true, true, false, true) -> (true, true, true, true)
  .. otherwise error

  read(false, _, _, true) -> (true, _, _, true)
  read(true, _, _, _) -> error, already read from disk
  read(_, _, _, false) -> initialization phase over /* cannot happen */

  for all syms: 
   prefs-snapshot(true, _, _, _) -> (true, _, _, false) 

for the last one, need a global "no more initialization can happen" flag.

|#

(module preferences mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "file.ss")
	   (lib "etc.ss")
           "sig.ss"
           "../gui-utils.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "pretty.ss")
	   (lib "list.ss"))
  
  (provide preferences@)
  (define preferences@
    (unit/sig framework:preferences^
      (import mred^
              [exn : framework:exn^]
              [exit : framework:exit^]
              [panel : framework:panel^]
              [frame : framework:frame^])

      (rename [-read read])
      
      (define main-preferences-symbol 'plt:framework-prefs)
      
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
      (define (pref-read?) read?)
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
      (define (get p)
        (cond
          [(and (pref-read?)
                (pref-default-set? p))
           (hash-table-get preferences
                           p
                           (λ ()
                             (cond
                               [(hash-table-bound? marshalled p)
                                (hash-table-put! preferences p (unmarshall p (hash-table-get marshalled p)))
                                (hash-table-remove! marshalled p)]
                               [else
                                (let* ([def (hash-table-get defaults p)]
                                       [def-val (default-value def)])
                                  (hash-table-put! preferences p def-val))])
                             (hash-table-get preferences p)))]
          [(not (pref-read?))
           (error
            'preferences:get
            "tried to get a preference but the disk preferences have not been read yet ~e"
            p)]
          [(not (pref-default-set? p))
           (raise-unknown-preference-error
            'preferences:get
            "tried to get a preference but no default set for ~e"
            p)]))
          
      ;; set : symbol any -> void
      ;; updates the preference
      ;; exported
      (define (set p value)
        (cond
          [(and (pref-read?)
                (pref-default-set? p))
           (let ([default (hash-table-get defaults p)])
             (unless ((default-checker default) value)
               (error 'preferences:set
                      "tried to set preference ~e to ~e but it does not meet test from preferences:set-default"
                      p value))
             (check-callbacks p value)
             (hash-table-remove! marshalled p)
             (hash-table-put! preferences p value))]
          [(not (pref-read?))
           (error
            'preferences:set
            "tried to get a preference but the disk preferences have not been read yet ~e"
            p)]
          [(not (pref-default-set? p))
           (raise-unknown-preference-error
            'preferences:set "tried to set the preference ~e to ~e, but no default is set"
            p
            value)]))

      (define (raise-unknown-preference-error sym fmt . args)
        (raise (exn:make-unknown-preference
                (string->immutable-string (string-append (format "~a: " sym) (apply format fmt args)))
                (current-continuation-marks))))
      
      ;; unmarshall : symbol marshalled -> any
      ;; unmarshalls a preference read from the disk
      (define (unmarshall p data)
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
      (define add-callback 
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
                               (loop (cdr callbacks))))]
                        [else
                         (cb p value)
                         (cons callback (loop (cdr callbacks)))]))]))])
          (if (null? new-callbacks)
              (hash-table-remove! callbacks p)
              (hash-table-put! callbacks p new-callbacks))))
      
      (define (set-un/marshall p marshall unmarshall)
        (cond
          [(and (pref-read?)
                (pref-default-set? p)
                (not (pref-un/marshall-set? p))
                (pref-can-init? p))
           (hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))]
          [(not (pref-read?))
           (error 'preferences:set-un/marshall
                  "preferences not yet read from disk for ~e" 
                  p)]
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
      
      (define restore-defaults
        (λ ()
          (hash-table-for-each
           defaults
           (λ (p v) (set p v)))))
      
      ;; set-default : (sym TST (TST -> boolean) -> void
      (define (set-default p default-value checker)
        (cond
          [(and (pref-read?)
                (not (pref-default-set? p))
                (pref-can-init? p))
           (let ([default-okay? (checker default-value)])
             (unless default-okay?
               (error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n"
                      p checker default-okay? default-value))
             (hash-table-put! defaults p (make-default default-value checker)))]
          [(not (pref-can-init? p))
           (error 'preferences:set-default
                  "tried to call set-default for preference ~e but it cannot be configured any more"
                  p)]
          [(not (pref-read?))
           (error 'preferences:set-default
                  "preferences not yet read from disk for ~e" p)]
          [(pref-default-set? p)
           (error 'preferences:set-default
                  "preferences default already set for ~e" p)]
          [(not (pref-can-init? p))
           (error 'preferences:set-default
                  "can no longer set the default for ~e" p)]))
      
      (define (save) (raw-save #f))
      (define (silent-save) (raw-save #f))
      
      ;; raw-save : boolean -> boolean
      ;; input determines if there is a dialog box showing the errors (and other msgs)
      ;; and result indicates if there was an error
      (define (raw-save silent?)
        (with-handlers ([exn:fail?
                         (λ (exn)
                           (unless silent?
                             (message-box
                              (string-constant preferences)
                              (format (string-constant error-saving-preferences)
                                      (exn-message exn))))
                           #f)])
          (let ([syms (list main-preferences-symbol)]
                [vals (list (append (hash-table-map preferences marshall-pref)
                                    (hash-table-map marshalled list)))]
                [res #t])
            (put-preferences
             syms vals
             (λ (filename)
               (unless silent?
                 (let* ([d (make-object dialog% (string-constant preferences))]
                        [m (make-object message% (string-constant waiting-for-pref-lock) d)])
                   (thread
                    (λ ()
                      (sleep 2)
                      (send d show #f)))
                   (send d show #t)
                   (put-preferences 
                    syms vals
                    (λ (filename)
                      (set! res #f)
                      (message-box
                       (string-constant preferences)
                       (format (string-constant pref-lock-not-gone) filename))))))))
            res)))
      
      ;; marshall-pref : symbol any -> (list symbol printable)
      (define (marshall-pref p value)
        (let/ec k
          (let* ([marshaller
                  (un/marshall-marshall
                   (hash-table-get marshall-unmarshall p
                                   (λ () (k (list p value)))))]
                 [marshalled (marshaller value)])
            (list p marshalled))))

      (define (read-err input msg)
        (message-box 
         (string-constant preferences)
         (let* ([max-len 150]
                [s1 (format "~s" input)]
                [ell "..."]
                [s2 (if (<= (string-length s1) max-len)
                        s1
                        (string-append
                         (substring s1 0 (- max-len
                                            (string-length ell)))
                         ell))])
           (string-append
            (string-constant error-reading-preferences)
            "\n"
            msg
            "\n"
            s2))))
      
      ;; read : -> void
      (define (-read) 
        (cond
          [(not (pref-read?))
           (set! read? #t)
           (let/ec k
             (let ([sexp (get-preference main-preferences-symbol (λ () (k (void))))])
               (when (andmap (lambda (x)
                               (and (pair? x)
                                    (symbol? (car x))
                                    (pair? (cdr x))
                                    (null? (cddr x))))
                             sexp)
                 (for-each (lambda (pr)
                             (let ([sym (car pr)]
                                   [pref (cadr pr)])
                               (hash-table-put! marshalled sym pref)))
                           sexp))))]
          [(pref-read?)
           (error 'preferences:read "preferences already read from disk")]))
      
      (define read? #f)
      
      (define snapshot-grabbed? #f)
      (define (get-prefs-snapshot)
        (cond
          [(pref-read?)
           (set! snapshot-grabbed? #t)
           (hash-table-map preferences cons)]
          [(not (pref-read?))
           (error 'get-prefs-snapshot
                  "cannot grab snapshot until preferences have been read from disk")]))
      
      (define (restore-prefs-snapshot snapshot)
        (for-each (lambda (lst) (set (car lst) (cdr lst)))
                  snapshot))
      


 
                                          
    ;;    ;           ;;;                 
     ;                  ;                 
     ;                  ;                 
  ;;;;  ;;;    ;;;;     ;     ;;;    ;;; ;
 ;   ;    ;        ;    ;    ;   ;  ;   ; 
 ;   ;    ;     ;;;;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
  ;;; ; ;;;;;   ;;; ; ;;;;;;  ;;;    ;;;; 
                                        ; 
                                        ; 
                                     ;;;  
      
      
      ;; ppanel-tree = 
      ;;  (union (make-ppanel-leaf string (union #f panel) (panel -> panel))
      ;;         (make-ppanel-interior string (union #f panel) (listof panel-tree)))
      (define-struct ppanel (name panel))
      (define-struct (ppanel-leaf ppanel) (maker))
      (define-struct (ppanel-interior ppanel) (children))
      
      ;; ppanels : (listof ppanel-tree)
      (define ppanels null)
      
      (define preferences-dialog #f)
      
      (define (add-panel title make-panel)
        (when preferences-dialog
          (error 'add-panel "preferences dialog already open, cannot add new panels"))
        (let ([titles (if (string? title)
                          (list title)
                          title)])
          (add-to-existing-children
           titles 
           make-panel
           (λ (new-subtree) (set! ppanels (cons new-subtree ppanels))))))
      
      ;; add-to-existing-children : (listof string) (panel -> panel) (ppanel -> void)
      ;; adds the child specified by the path in-titles to the tree.
      (define (add-to-existing-children in-titles make-panel banger)
        (let loop ([children ppanels]
                   [title (car in-titles)]
                   [titles (cdr in-titles)]
                   [banger banger])
          (cond
            [(null? children)
             (banger (build-new-subtree (cons title titles) make-panel))]
            [else
             (let ([child (car children)])
               (if (string=? (ppanel-name child) title)
                   (cond
                     [(null? titles) 
                      (error 'add-child "child already exists with this path: ~e" in-titles)]
                     [(ppanel-leaf? child)
                      (error 'add-child "new child's path conflicts with existing path: ~e" in-titles)]
                     [else
                      (loop
                       (ppanel-interior-children child)
                       (car titles)
                       (cdr titles)
                       (λ (x)
                         (set-ppanel-interior-children! 
                          (cons
                           x
                           (ppanel-interior-children child)))))])
                   (loop 
                    (cdr children)
                    title
                    titles
                    (λ (x)
                      (set-cdr! children
                                (cons x (cdr children)))))))])))
      
      ;; build-new-subtree : (cons string (listof string)) (panel -> panel) -> ppanel
      (define (build-new-subtree titles make-panel)
        (let loop ([title (car titles)]
                   [titles (cdr titles)])
          (cond
            [(null? titles) (make-ppanel-leaf title #f make-panel)]
            [else
             (make-ppanel-interior 
              title
              #f
              (list (loop (car titles) (cdr titles))))])))
           
      
      (define (hide-dialog)
	(when preferences-dialog
	  (send preferences-dialog show #f)))
      
      (define (show-dialog)
	(save)
	(if preferences-dialog
	    (send preferences-dialog show #t)
	    (set! preferences-dialog
		  (make-preferences-dialog))))

      (define (add-can-close-dialog-callback cb)
	(set! can-close-dialog-callbacks
	      (cons cb can-close-dialog-callbacks)))

      (define (add-on-close-dialog-callback cb)
	(set! on-close-dialog-callbacks
	      (cons cb on-close-dialog-callbacks)))

      (define on-close-dialog-callbacks null)

      (define can-close-dialog-callbacks null)
      
      (define (make-preferences-dialog)
        (letrec ([stashed-prefs (get-prefs-snapshot)]
		 [frame-stashed-prefs%
		  (class frame:basic%
                    (define/override (show on?)
                      (when on?
                        (set! stashed-prefs (get-prefs-snapshot)))
		      (super show on?))
		    (super-new))]
                 [frame 
                  (make-object frame-stashed-prefs%
                    (string-constant preferences))]
                 [build-ppanel-tree
                  (λ (ppanel tab-panel single-panel)
                    (send tab-panel append (ppanel-name ppanel))
                    (cond
                      [(ppanel-leaf? ppanel) 
                       ((ppanel-leaf-maker ppanel) single-panel)]
                      [(ppanel-interior? ppanel)
                       (let-values ([(tab-panel single-panel) (make-tab/single-panel single-panel #t)])
                         (for-each
                          (λ (ppanel) (build-ppanel-tree ppanel tab-panel single-panel))
                          (ppanel-interior-children ppanel)))]))]
                 [make-tab/single-panel 
                  (λ (parent inset?)
                    (letrec ([spacer (and inset?
                                          (instantiate vertical-panel% ()
                                            (parent parent)
                                            (border 10)))]
                             [tab-panel (instantiate tab-panel% ()
                                          (choices null)
                                          (parent (if inset? spacer parent))
                                          (callback (λ (_1 _2) 
                                                      (tab-panel-callback
                                                       single-panel
                                                       tab-panel))))]
                             [single-panel (instantiate panel:single% ()
                                             (parent tab-panel))])
                      (values tab-panel single-panel)))]
                 [tab-panel-callback
                  (λ (single-panel tab-panel)
                    (send single-panel active-child
                          (list-ref (send single-panel get-children)
                                    (send tab-panel get-selection))))]
                 [panel (make-object vertical-panel% (send frame get-area-container))]
                 [_ (let-values ([(tab-panel single-panel) (make-tab/single-panel panel #f)])
                      (for-each
                       (λ (ppanel)
                         (build-ppanel-tree ppanel tab-panel single-panel))
                       ppanels)
                      (let ([single-panel-children (send single-panel get-children)])
                        (unless (null? single-panel-children)
                          (send single-panel active-child (car single-panel-children))
                          (send tab-panel set-selection 0)))
                      (send tab-panel focus))]
                 [bottom-panel (make-object horizontal-panel% panel)]
                 [ok-callback (λ args
                                (when (andmap (λ (f) (f))
                                              can-close-dialog-callbacks)
                                  (for-each
                                   (λ (f) (f))
                                   on-close-dialog-callbacks)
                                  (save)
                                  (hide-dialog)))]
                 [cancel-callback (λ (_1 _2)
                                    (hide-dialog)
                                    (restore-prefs-snapshot stashed-prefs))])
          (gui-utils:ok/cancel-buttons
           bottom-panel
           ok-callback
           cancel-callback)
          (make-object grow-box-spacer-pane% bottom-panel)
          (send* bottom-panel
            (stretchable-height #f)
            (set-alignment 'right 'center))
          (send frame show #t)
          frame))
      
      (define (add-to-scheme-checkbox-panel f)
        (set! scheme-panel-procs 
              (let ([old scheme-panel-procs])
                (λ (parent) (old parent) (f parent)))))
      
      (define (add-to-editor-checkbox-panel f)
        (set! editor-panel-procs 
              (let ([old editor-panel-procs])
                (λ (parent) (old parent) (f parent)))))
      
      (define (add-to-warnings-checkbox-panel f)
        (set! warnings-panel-procs 
              (let ([old warnings-panel-procs])
                (λ (parent) (old parent) (f parent)))))

      (define scheme-panel-procs void)
      (define editor-panel-procs void)
      (define warnings-panel-procs void)
      
      (define (add-checkbox-panel label proc)
        (add-panel
         label
         (λ (parent)
           (let* ([main (make-object vertical-panel% parent)])
             (send main set-alignment 'left 'center)
             (proc main)
             main))))
      
      ;; make-check : panel symbol string (boolean -> any) (any -> boolean)
      ;; adds a check box preference to `main'.
      (define (make-check main pref title bool->pref pref->bool)
        (let* ([callback
                (λ (check-box _)
                  (set pref (bool->pref (send check-box get-value))))]
               [pref-value (get pref)]
               [initial-value (pref->bool pref-value)]
               [c (make-object check-box% title main callback)])
          (send c set-value initial-value)
          (add-callback pref
                        (λ (p v)
                          (send c set-value (pref->bool v))))))

      (define (make-recent-items-slider parent)
        (let ([slider (instantiate slider% ()
                        (parent parent)
                        (label (string-constant number-of-open-recent-items))
                        (min-value 1)
                        (max-value 100)
                        (init-value (get 'framework:recent-max-count))
                        (callback (λ (slider y)
                                    (set 'framework:recent-max-count
                                         (send slider get-value)))))])
          (add-callback
           'framework:recent-max-count
           (λ (p v)
             (send slider set-value v)))))
      
      (define (add-scheme-checkbox-panel)
        (letrec ([add-scheme-checkbox-panel
                  (λ ()
                    (set! add-scheme-checkbox-panel void)
                    (add-checkbox-panel
                     (list 
                      (string-constant editor-prefs-panel-label) 
                      (string-constant scheme-prefs-panel-label))
                     (λ (scheme-panel)
                       (make-check scheme-panel
                                   'framework:highlight-parens
                                   (string-constant highlight-parens)
                                   values values)
                       (make-check scheme-panel
                                   'framework:fixup-parens
                                   (string-constant fixup-close-parens)
                                   values values)
                       (make-check scheme-panel
                                   'framework:fixup-open-parens
                                   (string-constant fixup-open-parens)
                                   values values)
                       (make-check scheme-panel
                                   'framework:paren-match
                                   (string-constant flash-paren-match)
                                   values values)
                       (scheme-panel-procs scheme-panel))))])
          (add-scheme-checkbox-panel)))
      
      (define (add-editor-checkbox-panel)
        (letrec ([add-editor-checkbox-panel
                  (λ ()
                    (set! add-editor-checkbox-panel void)
                    (add-checkbox-panel 
                     (list (string-constant editor-prefs-panel-label) 
                           (string-constant general-prefs-panel-label))
                     (λ (editor-panel)
                       (make-recent-items-slider editor-panel)
                       (make-check editor-panel
                                   'framework:autosaving-on? 
                                   (string-constant auto-save-files)
                                   values values)
                       (make-check editor-panel  'framework:backup-files? (string-constant backup-files) values values)
                       (make-check editor-panel  'framework:delete-forward? (string-constant map-delete-to-backspace)
                                   not not)
                       (make-check editor-panel 'framework:show-status-line (string-constant show-status-line) values values)
                       (make-check editor-panel 'framework:col-offsets (string-constant count-columns-from-one) values values)
                       (make-check editor-panel 
                                   'framework:display-line-numbers
                                   (string-constant display-line-numbers)
                                   values values)
                       
                       (make-check editor-panel 
                                   'framework:auto-set-wrap?
                                   (string-constant wrap-words-in-editor-buffers)
                                   values values)
                       (make-check editor-panel 
                                   'framework:search-using-dialog?
                                   (string-constant separate-dialog-for-searching)
                                   values values)
                       (make-check editor-panel 
                                   'framework:open-here?
                                   (string-constant reuse-existing-frames)
                                   values values)
                       (make-check editor-panel 
                                   'framework:menu-bindings
                                   (string-constant enable-keybindings-in-menus)
                                   values values)
                       (make-check editor-panel 
                                   'framework:coloring-active
                                   (string-constant online-coloring-active)
                                   values values)
                       (when (memq (system-type) '(macos macosx))
                         (make-check editor-panel 
                                     'framework:special-option-key
                                     (string-constant option-as-meta)
                                     values values))
                       (unless (eq? (system-type) 'unix) 
                         (make-check editor-panel 
                                     'framework:print-output-mode 
                                     (string-constant automatically-to-ps)
                                     (λ (b) 
                                       (if b 'postscript 'standard))
                                     (λ (n) (eq? 'postscript n))))
                       (editor-panel-procs editor-panel))))])
          (add-editor-checkbox-panel)))

      (define (add-warnings-checkbox-panel)
        (letrec ([add-warnings-checkbox-panel
                  (λ ()
                    (set! add-warnings-checkbox-panel void)
                    (add-checkbox-panel
                     (string-constant warnings-prefs-panel-label)
                     (λ (warnings-panel)
                       (make-check warnings-panel 
                                   'framework:verify-change-format 
                                   (string-constant ask-before-changing-format)
                                   values values)
                       (make-check warnings-panel 
                                   'framework:verify-exit
                                   (string-constant verify-exit)
                                   values values)
                       
                       (warnings-panel-procs warnings-panel))))])
          (add-warnings-checkbox-panel)))
                  
      (define (local-add-font-panel)
        (let* ([font-families-name/const
                (list (list "Default" 'default)
                      (list "Decorative" 'decorative)
                      (list "Modern" 'modern)
                      (list "Roman" 'roman)
                      (list "Script" 'script)
                      (list "Swiss" 'swiss))]
               
               [font-families (map car font-families-name/const)]
               
               [font-size-entry "defaultFontSize"]
               [font-default-string "Default Value"]
               [font-default-size (case (system-type)
                                    [(windows) 10]
                                    [(macosx) 13]
                                    [else 12])]
               [font-section "mred"]
               [build-font-entry (λ (x) (string-append "Screen" x "__"))]
               [font-file (find-graphical-system-path 'setup-file)]
               [build-font-preference-symbol
                (λ (family)
                  (string->symbol (string-append "framework:" family)))]
               
               [set-default
                (λ (build-font-entry default pred)
                  (λ (family)
                    (let ([name (build-font-preference-symbol family)]
                          [font-entry (build-font-entry family)])
                      (set-default name
                                   default
                                   (cond
                                     [(string? default) string?]
                                     [(number? default) number?]
                                     [else (error 'internal-error.set-default "unrecognized default: ~a~n" default)]))
                      (add-callback 
                       name 
                       (λ (p new-value)
                         (write-resource 
                          font-section
                          font-entry
                          (if (and (string? new-value)
                                   (string=? font-default-string new-value))
                              ""
                              new-value)
                          font-file))))))])
          
          (for-each (set-default build-font-entry font-default-string string?)
                    font-families)
          ((set-default (λ (x) x)
                        font-default-size
                        number?)
           font-size-entry)
          (add-panel
           (string-constant default-fonts)
           (λ (parent)
             (letrec ([font-size-pref-sym (build-font-preference-symbol font-size-entry)]
                      [ex-string (string-constant font-example-string)]
                      [main (make-object vertical-panel% parent)]
                      [fonts (cons font-default-string (get-face-list))]
                      [make-family-panel
                       (λ (name)
                         (let* ([pref-sym (build-font-preference-symbol name)]
                                [family-const-pair (assoc name font-families-name/const)]
                                
                                [edit (make-object text%)]
                                [_ (send edit insert ex-string)]
                                [set-edit-font
                                 (λ (size)
                                   (let ([delta (make-object style-delta% 'change-size size)]
                                         [face (get pref-sym)])
                                     (if (and (string=? face font-default-string)
                                              family-const-pair)
                                         (send delta set-family (cadr family-const-pair))
                                         (send delta set-delta-face (get pref-sym)))
                                     
                                     (send edit change-style delta 0 (send edit last-position))))]
                                
                                [horiz (make-object horizontal-panel% main '(border))]
                                [label (make-object message% name horiz)]
                                
                                [message (make-object message%
                                           (let ([b (box "")])
                                             (if (and (get-resource 
                                                       font-section 
                                                       (build-font-entry name)
                                                       b)
                                                      (not (string=? (unbox b) 
                                                                     "")))
                                                 (unbox b)
                                                 font-default-string)) 
                                           horiz)]
                                [button 
                                 (make-object button%
                                   (string-constant change-font-button-label)
                                   horiz
                                   (λ (button evt)
                                     (let ([new-value
                                            (get-choices-from-user
                                             (string-constant fonts)
                                             (format (string-constant choose-a-new-font)
                                                     name)
                                             fonts)])
                                       (when new-value
                                         (set pref-sym (list-ref fonts (car new-value))) 
                                         (set-edit-font (get font-size-pref-sym))))))]
                                [canvas (make-object editor-canvas% horiz
                                          edit
                                          (list 'hide-hscroll
                                                'hide-vscroll))])
                           (set-edit-font (get font-size-pref-sym))
                           (add-callback
                            pref-sym
                            (λ (p new-value)
                              (send horiz change-children
                                    (λ (l)
                                      (let ([new-message (make-object message%
                                                           new-value
                                                           horiz)])
                                        (set! message new-message)
                                        (update-message-sizes font-message-get-widths 
                                                              font-message-user-min-sizes)
                                        (list label 
                                              new-message
                                              button
                                              canvas))))))
                           (send canvas set-line-count 1)
                           (vector set-edit-font
                                   (λ () (send message get-width))
                                   (λ (width) (send message min-width width))
                                   (λ () (send label get-width))
                                   (λ (width) (send label min-width width)))))]
                      [set-edit-fonts/messages (map make-family-panel font-families)]
                      [collect (λ (n) (map (λ (x) (vector-ref x n))
                                                set-edit-fonts/messages))]
                      [set-edit-fonts (collect 0)]
                      [font-message-get-widths (collect 1)]
                      [font-message-user-min-sizes (collect 2)]
                      [category-message-get-widths (collect 3)]
                      [category-message-user-min-sizes (collect 4)]
                      [update-message-sizes
                       (λ (gets sets)
                         (let ([width (foldl (λ (x l) (max l (x))) 0 gets)])
                           (for-each (λ (set) (set width)) sets)))]
                      [size-panel (make-object horizontal-panel% main '(border))]
                      [initial-font-size
                       (let ([b (box 0)])
                         (if (get-resource font-section 
                                           font-size-entry
                                           b)
                             (unbox b)
                             font-default-size))]
                      [size-slider
                       (make-object slider%
                         (string-constant font-size-slider-label)
                         1 127
                         size-panel
                         (λ (slider evt)
                           (set font-size-pref-sym (send slider get-value)))
                         initial-font-size)])
               (update-message-sizes font-message-get-widths font-message-user-min-sizes)
               (update-message-sizes category-message-get-widths category-message-user-min-sizes)
               (add-callback
                font-size-pref-sym
                (λ (p value)
                  (for-each (λ (f) (f value)) set-edit-fonts)
                  (unless (= value (send size-slider get-value))
                    (send size-slider set-value value))
                  #t))
               (for-each (λ (f) (f initial-font-size)) set-edit-fonts)
               (make-object message% (string-constant restart-to-see-font-changes) main)
               main))))
        (set! local-add-font-panel void))
      
      (define (add-font-panel) (local-add-font-panel))
      
      (-read))))
