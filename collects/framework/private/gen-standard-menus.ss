
(module gen-standard-menus mzscheme
  (require mzlib/pretty)
  (require mzlib/list)
  (require "standard-menus-items.ss")
  
  ;; build-before-super-item-clause : an-item -> (listof clause)
  (define build-before-super-item-clause
    (λ (item)
      (list
       `[define/public ,(an-item->callback-name item) ,(an-item-proc item)]
       `(define/public (,(an-item->get-item-name item))
          ,(an-item->item-name item))
       `(define/public (,(an-item->string-name item))
          ,(an-item-menu-string item))
       `(define/public (,(an-item->help-string-name item))
          ,(an-item-help-string item))
       `(define/public ,(an-item->on-demand-name item) ,(an-item-on-demand item))
       `(define/public (,(an-item->create-menu-item-name item))
          ,(an-item-create item)))))
  
  ;; build-before-super-clause : ((X -> sym) (X sexp) -> X -> (listof clause))
  (define build-before-super-clause
    (λ (->name -procedure)
      (λ (obj)
        (list `(define/public ,(->name obj)
                 ,(case (-procedure obj)
                    [(nothing) '(λ (menu) (void))]
                    [(separator) '(λ (menu) (make-object separator-menu-item% menu))]
                    [(nothing-with-standard-menus)
                     '(λ (menu) 
                        (unless (current-eventspace-has-standard-menus?)
                          (make-object separator-menu-item% menu)))]
                    [else (error 'gen-standard-menus "unknown between sym: ~e" (-procedure obj))]))))))
  
  ;; build-before-super-between-clause : between -> (listof clause)
  (define build-before-super-between-clause
    (build-before-super-clause
     between->name
     between-procedure))
  
  ;; build-before-super-before/after-clause : before/after -> (listof clause)
  (define build-before-super-before/after-clause
    (build-before-super-clause
     before/after->name
     before/after-procedure))
  
  ;; build-after-super-item-clause : an-item -> (list clause)
  (define (build-after-super-item-clause item)
    (let* ([callback-name (an-item->callback-name item)]
           [create-menu-item-name (an-item->create-menu-item-name item)]
           [callback-name-string (symbol->string callback-name)]
           [key (an-item-key item)])
      (list `(define
               ,(an-item->item-name item)
               (and (,create-menu-item-name)
                    ,(if (a-submenu-item? item)
                         `(instantiate (get-menu%) ()
                            (label (,(an-item->string-name item)))
                            (parent ,(menu-item-menu-name item))
                            (help-string (,(an-item->help-string-name item)))
                            (demand-callback (λ (menu-item) (,(an-item->on-demand-name item) menu-item))))
                         `(instantiate (get-menu-item%) ()
                            (label (,(an-item->string-name item)))
                            (parent ,(menu-item-menu-name item))
                            (callback (let ([,callback-name (λ (item evt) (,callback-name item evt))])
                                        ,callback-name))
                            (shortcut ,key)
                            (help-string (,(an-item->help-string-name item)))
                            (demand-callback (λ (menu-item) (,(an-item->on-demand-name item) menu-item))))))))))
  
  ;; build-after-super-clause : ((X -> symbol) -> X -> (listof clause))
  (define build-after-super-clause
    (λ (->name)
      (λ (between/after)
        (list 
         `(,(->name between/after)
           (,(menu-name->get-menu-name between/after)))))))
  
  ;; build-after-super-between-clause : between -> (listof clause)
  (define build-after-super-between-clause (build-after-super-clause between->name))
  ;; build-after-super-before/after-clause : before/after -> (listof clause)
  (define build-after-super-before/after-clause (build-after-super-clause before/after->name))
  
  ;; build-after-super-generic-clause : generic -> (listof clause)
  (define (build-after-super-generic-clause x) 
    (cond
      [(generic-private-field? x)
       (list `(define
                ,(generic-name x)
                ,(generic-initializer x)))]
      [(generic-override? x)
       (list)]
      [(generic-augment? x)
       (list)]
      [(generic-method? x)
       null]))
  
  ;; build-before-super-generic-clause : generic -> (listof clause)
  (define (build-before-super-generic-clause generic)
    (cond
      [(generic-private-field? generic)
       null]
      [(generic-override? generic)
       (list `(define/override ,(generic-name generic)
                ,(generic-initializer generic)))]
      [(generic-augment? generic)
       (list `(define/augment ,(generic-name generic)
                ,(generic-initializer generic)))]
      [(generic-method? generic)
       (list `(define/public ,(generic-name generic)
                ,(generic-initializer generic)))]))
  
  
  (define standard-menus.ss-filename (build-path (collection-path "framework" "private") "standard-menus.ss"))
  (printf "writing to ~a~n" standard-menus.ss-filename)  
  
  (call-with-output-file standard-menus.ss-filename
    (λ (port)
      (pretty-print
       `(define standard-menus<%>
          (interface (basic<%>)
            ,@(apply append (map
                             (λ (x)
                               (cond
                                 [(an-item? x) 
                                  (list 
                                   (an-item->callback-name x)
                                   (an-item->get-item-name x)
                                   (an-item->string-name x)
                                   (an-item->help-string-name x)
                                   (an-item->on-demand-name x)
                                   (an-item->create-menu-item-name x))]
                                 [(between? x) (list (between->name x))]
                                 [(or (after? x) (before? x))
                                  (list (before/after->name x))]
                                 [(generic? x) 
                                  (if (generic-method? x)
                                      (list (generic-name x))
                                      null)])) 
                             items))))
       port)
      
      (newline port)
      
      (pretty-print
       `(define standard-menus-mixin
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
            ,@(apply append (map (λ (x)
                                   (cond
                                     [(between? x) (build-before-super-between-clause x)]
                                     [(or (after? x) (before? x)) (build-before-super-before/after-clause x)]
                                     [(an-item? x) (build-before-super-item-clause x)]
                                     [(generic? x) (build-before-super-generic-clause x)]))
                                 items))
            (super-instantiate ())
            ,@(apply append (map (λ (x)
                                   (cond
                                     [(between? x) (build-after-super-between-clause x)]
                                     [(an-item? x) (build-after-super-item-clause x)]
                                     [(or (after? x) (before? x)) (build-after-super-before/after-clause x)]
                                     [(generic? x) (build-after-super-generic-clause x)]))
                                 items))
            (reorder-menus this)))
       port))
    'text
    'truncate))
