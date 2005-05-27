#cs(module wizard mzscheme
     (require (lib "mred.ss" "mred")
              (lib "class.ss")
              (lib "etc.ss")
              (lib "list.ss")
              (lib "string.ss" "srfi" "13")
              (lib "contract.ss"))
     
     (require (file "assoc-list.scm")
              (file "data-defs.scm"))
     
     (require-for-syntax (file "aux-syntax.scm"))
     
     (provide/contract
      [get-class-info
       (() (string? Class) . opt-> . (union false? (list/p Class boolean? boolean?)))]
      [get-union-info
       (-> (union false? (list/p Union boolean? boolean?)))] 
      )
     
     ;; (define/abstract <identifier>) :: <definition> 
     ;; introduce x as an abstract call back that raises an error 
     ;; and set-x as a setter that defines the function finally 
     ;; (mimic overriding)
     (define-syntax (define/abstract e)
       (syntax-case e ()
         [(_ x)
          (with-syntax ([set-x (prefix-id-suffix "set-" (syntax x) "")])
            (syntax 
             (define-values (x set-x)
               (let ([real-x (lambda y (error 'x "not initialized yet"))])
                 (values
                  (lambda y (apply real-x y))
                  (lambda (v) (set! real-x v)))))))]))
     
     #|
     present a dialog to create a single class; 
     if programmer aborts, return #f
     otherwise, produce a class and two booleans, requesting toString and draft 
     templates, respectively
     |#
     (define get-class-info
       (opt-lambda ([a-super null][a-v-class null])
         
         ;; -----------------------------------------------------------------------
         ;; Managing the class
         
         ;; (union false (list Class Boolean Boolean))
         ;; should the dialog return a class representation at the end 
         (define the-class #f)
         
         (define fields (new assoc%))
         
         ;; (Listof (-> (list String String)) ->  (list Class Boolean Boolean)
         ;; produce a class from fields
         (define (produce-class-from-fields fields)
           (with-handlers ([spec-error? (lambda _ #f)]) 
             (let* ([class (string-trim-both (send class-name get-value))]
                    [super (string-trim-both (send super-name get-value))]
                    [field (map (lambda (th) (th)) (send fields list))]
                    [field 
                     (foldr ;; r gives me the right order
                      (lambda (x r)
                        (let* ([v x] ; the privacy information isn't collecte
                               ; [v (cdr x)]  ; cdr means skip privacy attribute
                               [type (string-trim-both (car v))]
                               [name (string-trim-both (cadr v))])
                          (cond
                            [(and (java-id? type) (java-id? name))
                             (cons (list type name) r)]
                            [(java-id? type)
                             (error-message (format "check field name for ~a" type))]
                            [(java-id? name)
                             (error-message (format "check type for ~a" name))]
                            [else r])))
                      '()
                      field)])
               (if (java-id? class)
                   (list (list class super field)
                         (send tostring? get-value)
                         (send template? get-value))
                   (error-message "check class name")))))
         
         ;; -----------------------------------------------------------------------
         ;; the layout 
         
         (define-values (f p tostring? template? set-make-class set-add-field)
           (make-top "Class Wizard" "Insert Class" "Add Field"
                     (lambda (x e) (set! the-class #f) (send f show #f))))
         
         ;; -----------------------------------------------------------------------
         ;; information about the class
         
         (define privacy-modifiers '("no modifier" "public" "private" "protected"))
         
         ;; Panel -> Choice
         (define (make-modifier-menu p)
           (new choice% 
                (label "") (choices privacy-modifiers) (parent p) (callback void)))
         
         
         ;; TextField Event -> Void
         ;; a callback that on return creates a new "add field" panel when 
         ;; it's the bottom most text field 
         (define/abstract send/create-field)
         
         ;; Information about the class in general: 
         (define info-pane (new vertical-panel% (parent p) (style '(border))))
         (define purpose 
           (new text-field% 
                (parent info-pane) (label "// purpose of class: ") (callback void)))
         (define class-pane (add-horizontal-panel info-pane))
         ; (define class-privacy (make-modifier-menu class-pane))
         (define class-name (make-text-field class-pane "class"))
         (define super-name (make-text-field class-pane "extends" send/create-field))
         
         ;; Information about the class's fields:
         (define field-panel (new vertical-panel% (parent info-pane)))
         
         ;; (list Modifier String String) *-> Void
         ;; add a field panel so that a new field for the class can be specified
         ;; if rest arguments, it consists of two strings: 
         ;; one for the type, one for name
         (define (add-field-panel . a-field)
           (let* ([fp (add-horizontal-panel field-panel)]
                  ; [modi (make-modifier-menu fp)]
                  [type (make-text-field fp "type:")]
                  [name (make-text-field fp "name:" send/create-field)]
                  [get-values (lambda ()
                                (list ;(send modi get-string-selection)
                                 (send type get-value)
                                 (send name get-value)))])
             (when (pair? a-field)
               (send type set-value (car a-field))
               (send name set-value (cadr a-field)))
             (add-field-name name)
             (send fields add type get-values)
             (new button% 
                  (label "Delete Field") (parent fp)
                  (callback
                   (lambda (x e)
                     (send fields remove type)
                     (remove-field-name name)
                     (send field-panel change-children (remove-panel fp)))))))
         
         ;; Managing the creation of new "add field" panels
         
         ;; (Listof TextField)
         ;; the list of name TextFields that have been added via (add-field-panel)
         ;; a stack in that the bottom field is always at beginning of list
         ;; if empty, there are no fields
         (define the-last-field-name '())
         
         ;; TextField -> Boolean
         ;; what is the current last 
         (define (should-create-new-add-field? x) 
           (or (null? the-last-field-name) (eq? (car the-last-field-name) x)))
         
         ;; TextField -> Void
         ;; push f on the-last-field-name
         (define (add-field-name f) 
           (set! the-last-field-name (cons f the-last-field-name)))
         
         ;; TextField -> Void
         ;; remove from "stack"
         (define (remove-field-name f)
           (set! the-last-field-name (remove f the-last-field-name)))
         
         (define _stupid_effect
           (set-send/create-field
            (lambda (x e)
              (when (eq? (send e get-event-type) 'text-field-enter)
                (when (should-create-new-add-field? x) (add-field-panel))
                (send f on-traverse-char (new key-event% (key-code #\tab)))))))
         
         ;; -----------------------------------------------------------------------
         
         (define-values (error-message spec-error?) (add-error-panel p))
         
         ;; -----------------------------------------------------------------------
         ;; setting it all up
         
         ;; String -> Void
         ;; set up the super class, uneditable 
         (define (setup-super a-super)
           (send super-name set-value a-super)
           (send (send super-name get-editor) lock #t))
         
         (cond
           [(and (null? a-super) (null? a-v-class))
            (add-field-panel)]
           [(null? a-v-class)
            (add-field-panel)
            (setup-super a-super)]
           [(null? a-super)
            (error 'internal "can't happen: no super, but class provided")]
           [else ; 
            (setup-super a-super)
            (let ([name (car a-v-class)]
                  [fields (cdr a-v-class)])
              (send class-name set-value name)
              (for-each (lambda (f) (apply add-field-panel f)) fields))])
         
         (set-add-field (lambda (x e) (add-field-panel)))
         
         (set-make-class
          (lambda (x e)
            (set! the-class (produce-class-from-fields fields))
            (when the-class (send f show #f))))
         
         (send f show #t)
         
         the-class
         ))
     
     
     #| -> (union #f (list Class Boolean Boolean)
     present a modal dialog to create a union; 
     if programmer aborts, return #f
     otherwise, produce a datatype and two booleans, requesting toString and draft 
     templates, respectively, for the entire datatype
  |#
     (define (get-union-info)
       
       ;; -------------------------------------------------------------------------
       ;; Managing the datatype
       (define the-type #f)
       
       (define variants (new assoc%))
       
       ;; -------------------------------------------------------------------------
       ;; GUI Layout
       
       (define-values (f p toString? template? set-make-union set-add-var)
         (make-top "Union Wizard" "Insert Union" "Add Variant" 
                   (lambda (x e) (set! the-type #f) (send f show #f))))
       
       (define type-pane 
         (new vertical-panel% 
              (parent p) 
              (alignment '(center center)) (style '(border))
              (min-height 50) (stretchable-height #f)))
       
       (define purpose 
         (new text-field% 
              (parent type-pane) (label "// purpose of union: ") (callback void)))
       
       (define type-text (make-text-field type-pane "Type"))
       
       (define vart-pane 
         (new horizontal-panel% 
              (parent p) (alignment '(center center)) (style '(border))
              (min-height 150) (stretchable-height #f)))
       
       (define-values (error-message spec-error?) (add-error-panel p))
       
       ;; -------------------------------------------------------------------------
       ;; Accessing and Mutating GUIs
       
       ;; -> String
       (define (get-type)
         (let ([t (string-trim-both (send type-text get-value))])
           (if (java-id? t) t (error-message "check type name"))))
       
       ;; -> Void
       (define add-variant-panel
         (make-add-variant-panel vart-pane spec-error? variants get-type))
       
       ;; make two variants to boot
       ;; allow people to add and delete a variant 
       (add-variant-panel)
       (add-variant-panel)
       
       (set-make-union
        (lambda (x e)
          (set! the-type
                (with-handlers ([spec-error? (lambda _ #f)])
                  (list 
                   (list (get-type)
                         (foldr (lambda (f r) (if f (cons f r) r)) '()
                                (send variants list)))
                   (send toString? get-value)
                   (send template? get-value))))
          (send f show #f)))
       
       (set-add-var (lambda (x e) (add-variant-panel)))
       
       (send f show #t)
       
       the-type 
       )
     
     
     ;; Pane (Any -> Boolean) Assoc -> (-> Void)
     ;; create a function that adds "variant" panels to the get-union-info dialog
     (define (make-add-variant-panel vart-pane spec-error? variants get-type)
       (lambda ()
         (let* ([vp (new vertical-panel% (parent vart-pane)(style '(border)))]
                [ms (new message% (parent vp) (label "Variant"))]
                [bt (new button% (parent vp) (label "Edit")   
                         (callback 
                          (lambda (bt evt)
                            (with-handlers ([spec-error? void])
                              (let* ([type (get-type)]
                                     [class-in (send variants lookup bt)]
                                     [a-class (if class-in 
                                                  (get-class-info type class-in)
                                                  (get-class-info type))])
                                (when a-class
                                  (let* ([a-class (car a-class)]
                                         [name (car a-class)]
                                         [fields (caddr a-class)])
                                    ;; no supertype needed: remove (cadr a-class)
                                    (send variants update bt (list name fields))
                                    (send ms set-label name))))))))])
           (send variants add bt #f)
           (new button% (parent vp) (label "Delete")
                (callback 
                 (lambda (x e)
                   (send variants remove bt)
                   (send vart-pane change-children (remove-panel vp))))))))
     
     ;; ---------------------------------------------------------------------------
     
     ;; String String String CallBack
     ;; -> 
     ;; Frame Pane CheckBox CheckBox (Callback -> Void) (Callback -> Void) 
     ;; set up the top of the frame 
     (define (make-top title insert add quit-cb)
       (define f (new dialog% (label title) (width 500) (height 300)))
       (define p (new vertical-pane% (parent f)))
       
       (define button-panel (add-horizontal-panel p))
       
       (define quit (add-button button-panel  "Abort" quit-cb))
       
       (define/abstract make-class-cb)
       (define class-button (add-button button-panel insert make-class-cb))
       
       (define/abstract add-field-cb)
       (define add-field-button (add-button button-panel add add-field-cb))
       
       (define switch-pane (add-horizontal-panel p))
       (define toString-check (make-checkbox switch-pane "add toString()"))    
       (define template-check (make-checkbox switch-pane "add method template"))  
       
       (values f p toString-check template-check set-make-class-cb set-add-field-cb))
     
     ;; Panel String [Callback] -> TextField
     (define make-text-field 
       (opt-lambda (p l (c void))
         (new text-field% 
              (parent p) (label l) (callback c)
              (min-width 50) (stretchable-width #f))))
     
     ;; Pane -> HorizontalPanel
     ;; add a fixed-width horizontal panel (50) to p
     (define (add-horizontal-panel p)
       (new horizontal-panel% (parent p) (min-height 50) (stretchable-height #f)))
     
     ;; Panel -> (Panels -> Panels)
     ;; remove vp from cs 
     (define (remove-panel vp)
       (lambda (cs) (filter (lambda (c) (not (eq? vp c))) cs)))
     
     ;; String CallBack -> Button
     (define (add-button bp l cb) ;; to button-panel
       (new button% (label l) (parent bp) (callback cb)))
     
     ;; Panel String -> CheckBox
     (define (make-checkbox p l)
       (new check-box% (parent p) (label l) (callback void)))
     
     ;; Panel -> (String -> Void) (Any -> Boolean) 
     (define (add-error-panel p)
       (define message-size 100)
       (define spec-error (cons 1 2))
       (define message
         (new message%
              (parent (add-horizontal-panel p)) (label (make-string 100 #\space))))
       ;; String -> false
       (define (error-message m)
         (send message set-label (string-append "error: "m))
         (raise spec-error))
       ;; Any -> Boolean 
       (define (spec-error? x) (eq? spec-error x ))
       (values error-message spec-error?))
     
     
     #| run: emulate the actual wizard
     (require (file "class.scm"))
     
     (provide x y)
     
     (define x (get-class-info))
     (if x (printf "~a~n" (apply make-class x)))
     
     (define y (get-union-info))
     (if y (printf "~a~n" (apply make-union y)))
     |#
     
     )
