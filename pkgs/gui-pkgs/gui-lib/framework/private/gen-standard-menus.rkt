#lang at-exp racket/base

;; Generates the framework code for standard menus, and the code for
;; their documentation.  This is done in a naive way (replaces older
;; code that generated files), should really be made into proper macros
;; instead.

(require (for-syntax racket/base racket/list racket/match syntax/strip-context
                     racket/pretty "standard-menus-items.rkt"))

;; build-before-super-item-clause : an-item -> (listof clause)
(define-for-syntax (build-before-super-item-clause item)
  `((define/public ,(an-item->callback-name item) ,(an-item-proc item))
    (define/public (,(an-item->get-item-name item))
      ,(an-item->item-name item))
    (define/public (,(an-item->string-name item))
      ,(an-item-menu-string item))
    (define/public (,(an-item->help-string-name item))
      ,(an-item-help-string item))
    (define/public ,(an-item->on-demand-name item) ,(an-item-on-demand item))
    (define/public (,(an-item->create-menu-item-name item))
      ,(an-item-create item))))

;; build-before-super-clause : ((X -> sym) (X sexp) -> X -> (listof clause))
(define-for-syntax ((build-before-super-clause ->name -procedure) obj)
  `((define/public ,(->name obj)
      ,(case (-procedure obj)
         [(nothing) '(λ (menu) (void))]
         [(separator) '(λ (menu) (make-object separator-menu-item% menu))]
         [(nothing-with-standard-menus)
          '(λ (menu)
             (unless (current-eventspace-has-standard-menus?)
               (make-object separator-menu-item% menu)))]
         [else (error 'gen-standard-menus "unknown between sym: ~e" (-procedure obj))]))))

;; build-before-super-between-clause : between -> (listof clause)
(define-for-syntax build-before-super-between-clause
  (build-before-super-clause between->name between-procedure))

;; build-before-super-before/after-clause : before/after -> (listof clause)
(define-for-syntax build-before-super-before/after-clause
  (build-before-super-clause before/after->name before/after-procedure))

;; build-after-super-item-clause : an-item -> (list clause)
(define-for-syntax (build-after-super-item-clause item)
  (let* ([callback-name (an-item->callback-name item)]
         [create-menu-item-name (an-item->create-menu-item-name item)]
         [callback-name-string (symbol->string callback-name)]
         [key (an-item-shortcut item)])
    `((define ,(an-item->item-name item)
        (and (,create-menu-item-name)
             ,(if (a-submenu-item? item)
                `(new (get-menu%)
                      [label (,(an-item->string-name item))]
                      [parent ,(menu-item-menu-name item)]
                      [help-string (,(an-item->help-string-name item))]
                      [demand-callback (λ (menu-item) (,(an-item->on-demand-name item) menu-item))])
                `(new ,(if (a-checkable-item? item)
                         '(get-checkable-menu-item%)
                         '(get-menu-item%))
                      [label (,(an-item->string-name item))]
                      [parent ,(menu-item-menu-name item)]
                      [callback (let ([,callback-name (λ (item evt) (,callback-name item evt))])
                                  ,callback-name)]
                      [shortcut ,key]
                      [shortcut-prefix ,(an-item-shortcut-prefix item)]
                      [help-string (,(an-item->help-string-name item))]
                      [demand-callback (λ (menu-item) (,(an-item->on-demand-name item) menu-item))])))))))

;; build-after-super-clause : ((X -> symbol) -> X -> (listof clause))
(define-for-syntax ((build-after-super-clause ->name) between/after)
  `((,(->name between/after)
     (,(menu-name->get-menu-name between/after)))))

;; build-after-super-between-clause : between -> (listof clause)
(define-for-syntax build-after-super-between-clause
  (build-after-super-clause between->name))
;; build-after-super-before/after-clause : before/after -> (listof clause)
(define-for-syntax build-after-super-before/after-clause
  (build-after-super-clause before/after->name))

;; build-after-super-generic-clause : generic -> (listof clause)
(define-for-syntax (build-after-super-generic-clause x)
  (cond [(generic-private-field? x)
         `((define ,(generic-name x) ,(generic-initializer x)))]
        [(generic-override? x) '()]
        [(generic-augment? x) '()]
        [(generic-method? x) '()]))

;; build-before-super-generic-clause : generic -> (listof clause)
(define-for-syntax (build-before-super-generic-clause generic)
  (cond [(generic-private-field? generic)
         '()]
        [(generic-override? generic)
         `((define/override ,(generic-name generic)
             ,(generic-initializer generic)))]
        [(generic-augment? generic)
         `((define/augment ,(generic-name generic)
             ,(generic-initializer generic)))]
        [(generic-method? generic)
         `((define/public ,(generic-name generic)
             ,(generic-initializer generic)))]))

(provide generate-standard-menus-code)
(define-syntax (generate-standard-menus-code stx)
  (datum->syntax
   stx
   `(begin
      (define standard-menus<%>
        (interface (basic<%>)
          ,@(append-map
             (λ (x)
               (cond [(an-item? x)
                      (list (an-item->callback-name x)
                            (an-item->get-item-name x)
                            (an-item->string-name x)
                            (an-item->help-string-name x)
                            (an-item->on-demand-name x)
                            (an-item->create-menu-item-name x))]
                     [(between? x)
                      (list (between->name x))]
                     [(or (after? x) (before? x))
                      (list (before/after->name x))]
                     [(generic? x)
                      (if (generic-method? x) (list (generic-name x)) '())]))
             items)))
      (define standard-menus-mixin
        (mixin (basic<%>) (standard-menus<%>)
          (inherit on-menu-char on-traverse-char)
          (define remove-prefs-callback
            (preferences:add-callback
             'framework:menu-bindings
             (λ (p v)
               (let loop ([menu (get-menu-bar)])
                 (when (is-a? menu menu:can-restore<%>)
                   (if v
                     (send menu restore-keybinding)
                     (send menu set-shortcut #f)))
                 (when (is-a? menu menu:can-restore-underscore<%>)
                   (if v
                     (send menu restore-underscores)
                     (send menu erase-underscores)))
                 (when (is-a? menu menu-item-container<%>)
                   (for-each loop (send menu get-items)))))))
          (inherit get-menu-bar show can-close? get-edit-target-object)
          ,@(append-map
             (λ (x)
               (cond [(between? x) (build-before-super-between-clause x)]
                     [(or (after? x) (before? x))
                      (build-before-super-before/after-clause x)]
                     [(an-item? x) (build-before-super-item-clause x)]
                     [(generic? x) (build-before-super-generic-clause x)]))
             items)
          (super-instantiate ())
          ,@(append-map
             (λ (x)
               (cond [(between? x) (build-after-super-between-clause x)]
                     [(an-item? x) (build-after-super-item-clause x)]
                     [(or (after? x) (before? x))
                      (build-after-super-before/after-clause x)]
                     [(generic? x) (build-after-super-generic-clause x)]))
             items)
          (reorder-menus this))))))

(define-for-syntax (generate-item-docs x)
  (cond
    [(generic/docs? x)
     (filter-not string? (generic/docs-documentation x))]
    [(before/after? x)
     `(@defmethod[(,(before/after->name x) [menu (is-a?/c menu-item%)]) void?]{
         This method is called @,(if (before? x) "before" "after") the addition of the
         @tt[,(format "~a" (before/after-name x))] menu-item. Override it to add additional
         menu items at that point. })]
    [(between? x)
     `(@defmethod[(,(between->name x) [menu (is-a?/c menu-item%)]) void?]{
         This method is called between the addition of the
         @tt[,(format "~a" (between-before x))] and the @tt[,(format "~a" (between-after x))] menu-item. 
         Override it to add additional menu items at that point. })]
    [(an-item? x)
     `(@defmethod[(,(an-item->get-item-name x)) (or/c false/c (is-a?/c menu-item%))]{
         This method returns the @racket[menu-item%] object corresponding
         to this menu item, if it has been created (as controlled by
         @method[frame:standard-menus<%> ,(an-item->create-menu-item-name x)]).}
       @defmethod[(,(an-item->create-menu-item-name x)) boolean?]{
         The result of this method determines if the corresponding
         menu item is created. Override it to control the creation of the menu item. 
         @||
         Defaults to @racket[,(an-item-create x)].}
       ,(match (an-item-proc x)
          [`(λ (,item-name ,evt-name) ,bodies ...)
           `@defmethod[(,(an-item->callback-name x)
                        [,item-name (is-a?/c menu-item%)]
                        [,evt-name (is-a?/c control-event%)])
                       void?]{
              Defaults to
              @racketblock[,(if (= 1 (length bodies)) (car bodies) `(begin ,@bodies))] }])
       ,(match (an-item-on-demand x)
          [`(λ (,item-name) ,body)
           `@defmethod[(,(an-item->on-demand-name x)
                        [,item-name (is-a?/c menu-item%)])
                       void?]{
              The menu item's on-demand proc calls this method. 
              @||
              Defaults to @racketblock[,body]}])
       @defmethod[(,(an-item->string-name x)) string?]{
          The result of this method is used as the name of the @racket[menu-item%].
          @||
          Defaults to @racket[,(an-item-menu-string x)].}
       @defmethod[(,(an-item->help-string-name x)) string?]{
         The result of this method is used as the help string
         when the @racket[menu-item%] object is created.
         @||
         Defaults to @racket[,(an-item-help-string x)].})]
    [else '()]))

(provide generate-standard-menus-docs)
(define-syntax (generate-standard-menus-docs stx)
  (define docs
    `(definterface frame:standard-menus<%> (frame:basic<%>)
       ,@(append-map generate-item-docs items)))
  ;; HACK: the rendering includes location information from the source, so
  ;; generating a location-less syntax doesn't work.  Instead, use the pretty
  ;; printer to display the whole thing, then read the output as a syntax with
  ;; location.  Note that the 80 width is a guess that works, since indentation
  ;; includes the documentation forms too (starting from the toplevel
  ;; `definterface' and on.)
  (define o (open-output-string))
  (parameterize ([pretty-print-columns 80] [current-output-port o])
    (pretty-write docs))
  (parameterize ([port-count-lines-enabled #t])
    (replace-context
     stx
     (read-syntax 'gen-standard-menus
                  (open-input-string (get-output-string o))))))
