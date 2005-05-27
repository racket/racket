#cs
(module view mzscheme 
  
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss" "srfi" "13")
           (lib "contract.ss"))
  
  (require (file "assoc-list.scm")
           (file "data-defs.scm")
           (file "aux-class.scm"))
  
  (provide/contract
   [get-class-info (->* []
                        [boolean? (union false/c (list/c Class boolean? boolean?))])]
   [get-union-info (->* []
		        [boolean? (union false/c (list/c Union boolean? boolean?))])])
  
  (define CLASS-WIZARD "The Class Wizard")
  (define UNION-WIZARD "The Union Wizard")
  (define VARIANT-WIZD "The Variant Wizard")
  (define VARIANT      "Variant") 
  (define INSERT-CLASS "Insert Class")
  (define INSERT-UNION "Insert Union")
  (define INSERT-VARNT "Insert Variant")
  (define ADD-FIELD    "Add Field")
  (define ADD-VARIANT  "Add Variant")
  (define ADD-TOSTRING "add toString()")
  (define ADD-TEMPLATE "add method template")
  (define ADD-DIAGRAM  "add class diagram")
  (define PURPOSE-CLASS "// purpose of class: ")
  (define PURPOSE-UNION  "// purpose of union: ")
  (define CLASS        "class")
  (define SUPER        "super")
  (define EXTENDS      "extends")
  (define CHECK-NAME-F "check name of ~a")
  (define CHECK-TYPE-F "check type for ~a")
  (define CHECK-FIELD-NAME-F "check field name for ~a")
  (define TYPE "type") 
  (define NAME "name")
  (define ABORT "Abort")
  (define ERROR: "Error: ")
  (define DELETE "Delete")
  (define EDIT         "edit")
  
  
  #|
     present a dialog to create a single class; 
     if programmer aborts, return #f
     otherwise, produce a class and two booleans, requesting toString and draft 
     templates, respectively
     |#
  
  (define (get-class-info)
    (let ([ci (new class-info% (title CLASS-WIZARD)
                   (insert-str INSERT-CLASS) (add-str ADD-FIELD))])
      (send ci call)))
  
  (define (get-union-info)
    (let ([ui (new union-info% (title UNION-WIZARD)
                   (insert-str INSERT-UNION) (add-str ADD-VARIANT))])
      (send ui call)))
  
  #|
            *---------------------*
            | dialog%             |
            *---------------------*
               |
               |
              / \
            *---------------------*
            | class-union-wizard% |
            *---------------------*
            | tostring?           |
            | template?           |
            | error-message       |
            | an-error?           |
            | call                |
            | A: produce          |
            | A: make-class-cb    |
            | A: add-field-cb     |
            *---------------------*
              |
              |
             / \
        --------------------------------------------
        |                                          |
    *---------------------*                    *---------------------*
    | class-info%         |                    | union-info%         |
    *---------------------*                    *---------------------*
    | field-panel         |--+                 | vart-panel          |--+
    *---------------------*  |                 *---------------------*  |
                             |                                          |          
                             |                                          |          
                             |                                          |          
                             |                                          |          
    *---------------------*  |                 *---------------------*  | 
    | vertical-panel%     |  |                 | horizontal-panel%   |  |          
    *---------------------*  |                 *---------------------*  |          
                      |      |                                   |      |          
                      |      |                                   |      |          
                     / \     |                                  / \     |          
                 *---------------------*                  *---------------------*
                 | field-panel%        |                  | variant-panel%      |
                 *---------------------*                  *---------------------*
                 | add                 |                  | add                 |
                 | add-on-return       |                  | produce             |
                 | produce             |                  *---------------------*
                 *---------------------*                  | acquired:           |
                 | acquired:           |                  |  get-type           |
                 |  window (?)         |                  |  error-message      |
                 |  error-message      |                  |  an-error?          |
                 *---------------------*                  *---------------------* 

    |#
  
  ;; ------------------------------------------------------------------------
  ;; Set up the frame, including the info-panel where subclasses can 
  ;; request specific information. The frame includes buttons for aborting
  ;; the edit process, for packaging up the information in the edit, and for 
  ;; adding some component (field, variant)
  
  ;; String String String -> ClassUnionWizard
  (define class-union-wizard%
    (class dialog% (init-field title insert-str add-str (switches? #t))
      (super-new (label title) (width 500) (height 300)) 
      
      (define p (new vertical-pane% (parent this)))
      
      (define button-panel (add-horizontal-panel p))
      
      (define abort? #t) ;; assume bad things happen
      (define (quit x e) (send this show #f))
      (add-button button-panel ABORT quit)

      (define/abstract make-class-cb)
      (add-button button-panel insert-str 
                  (lambda (x e) (set! abort? #f) (make-class-cb x e)))
      
      (define/abstract add-field-cb)
      (add-button button-panel add-str (lambda (x e) (add-field-cb x e)))
      
      ;; switches for toString methods and template in comments 
      (define-values (string template diagram)
        (if switches? 
            (let ([switch-pane (add-horizontal-panel p)])
              (values (make-checkbox switch-pane ADD-TOSTRING) 
                      (make-checkbox switch-pane ADD-TEMPLATE)
                      (make-checkbox switch-pane ADD-DIAGRAM)))
            (values #f #f #f)))
      (define (get-switch x) (and switches? (send x get-value)))
      (define/public (tostring?) (get-switch string))
      (define/public (template?) (get-switch template))
      (define/public (diagram?) (get-switch diagram))
      
      ;; --------------------------------------------------------------------
      ;; info panel
      (field (info-pane (new vertical-panel% (parent p) (style '(border)))))
      
      ;; --------------------------------------------------------------------
      ;; error panel         
      
      (define message-size 100)
      (define an-error (cons 1 2))
      (define message
        (new message% (parent (add-horizontal-panel p)) 
             (label (make-string message-size #\space))))
      
      ;; String -> false
      (define/public (error-message m)
        (send message set-label (string-append ERROR: m)) 
        (raise an-error))
      ;; Any -> Boolean 
      (define/public (an-error? x) (eq? an-error x ))
      
      ;; TextField (union false String) -> java-id?
      (define/public (produce-name-from-text name msg)
        (let ([x (string-trim-both (send name get-value))])
          (cond
            [(not msg) x]
            [(java-id? x) x]
            [else (error-message (format CHECK-NAME-F msg))])))
      
      ;; --------------------------------------------------------------------
      ;; call in 
      (define/public (call) 
        (send this show #t)
        (values (diagram?) (if abort? #f (produce))))
      
      (define/abstract produce)))
  
  ;; ------------------------------------------------------------------------
  ;; String String String [String] [String] -> ClassUnionWizard
  ;; get information about a class (fields, purpose statement, ...)
  (define class-info%
    (class class-union-wizard%
      (init-field (a-super null) (a-v-class null))
      (super-new)
      (inherit-field info-pane)
      (inherit 
        tostring? template? diagram? 
        error-message an-error? produce-name-from-text)
      
      ;; --------------------------------------------------------------------
      ;; filling the info-pane 
      
      ;; Information about the class in general: 
      (define purpose 
        ;; it is not the kind of textbox that make-text-field creates
        (new text-field%
             (parent info-pane) (label PURPOSE-CLASS) (callback void)))
      
      (define class-pane (add-horizontal-panel info-pane))
      ; (define class-privacy (make-modifier-menu class-pane))
      (define class-name (make-text-field class-pane CLASS))
      (define (super-cb x e) (send field-panel add-on-return x e))
      (define super-name (make-text-field class-pane EXTENDS super-cb))
      
      ;; Information about the class's fields:
      (define field-panel
        (new field-panel%
             (parent info-pane) (window this) 
             (error-message (lambda (x) (error-message x)))))
      
      (define/override (add-field-cb x e) (send field-panel add))
      
      ;; --------------------------------------------------------------------
      ;; creating the class from the specification 
      
      ;; -> (union false (list Class boolean? boolean?))
      (define/override (produce)           
        (with-handlers ([(lambda (x) (an-error? x)) (lambda _ #f)]) 
           (list (list (produce-name-from-text class-name CLASS)
                       (produce-name-from-text
                        super-name (if (null? a-super) #f SUPER))
                       (send field-panel produce)
                       (send purpose get-value))
                 (tostring?) 
                 (template?))))
      
      ;; if the class specification is proper, hide dialog
      (define/override (make-class-cb x e) 
        (when (produce) (send this show #f)))
      
      ;; --------------------------------------------------------------------
      ;; setting it all up
      
      ;; String -> Void
      ;; set up the super class, uneditable 
      (define (setup-super a-super)
        (send super-name set-value a-super)
        (send (send super-name get-editor) lock #t))
      
      ;; init depending on inputs ... 
      (cond
        [(and (null? a-super) (null? a-v-class))
         (send field-panel add)]
        [(null? a-v-class)
         (send field-panel add)
         (setup-super a-super)]
        [(null? a-super)
         (error 'internal "can't happen: no super, but class provided")]
        [else ; 
         (setup-super a-super)
         (let ([name (car a-v-class)]
               [the-fields (cadr a-v-class)])
           (send class-name set-value name)
           (for-each (lambda (f) (send field-panel add f)) the-fields)
           (send purpose set-value (variant-purpose a-v-class)))])
      
      ))
  
  ;; Panel Window (String -> Void) -> FieldPanel 
  ;; manage text fields to add fields to the class in a stack like fashion;
  ;; add one on <return>, allow users to delete one
  ;; produce the field specs on demand 
  (define field-panel%
    (class vertical-panel% 
      (init-field window error-message)
      (super-new)
      
      ;; (Listof TextField)
      ;; the list of name TextFields that have been added via (add)
      ;; a stack in that the bottom field is always at beginning of list
      ;; if empty, there are no fields
      (define the-last-field-name '())
      
      ;; TextField -> Void
      ;; push f on the-last-field-name
      (define (add-field-name f) 
        (set! the-last-field-name (cons f the-last-field-name)))
      
      ;; TextField -> Void
      ;; remove from "stack"
      (define (remove-field-name f)
        (set! the-last-field-name (remove f the-last-field-name)))
      
      ;; TextField Event -> Void
      ;; a callback that on return creates a new "add field" panel when 
      ;; it's the bottom most text field
      (define/public (add-on-return x e)
        (when (eq? (send e get-event-type) 'text-field-enter)
          (when (or (null? the-last-field-name)
                    (eq? (car the-last-field-name) x))
            (add))
          (send window on-traverse-char (new key-event% (key-code #\tab)))))
      
      ;; -> TextField TextField
      ;; (list String String) -> TextField TextField
      ;; add a field panel so that a new field for the class can be specified
      ;; if one argument, it consists of two strings: 
      ;; one for the type, one for name
      (define/public add
        (case-lambda
          [() 
           (let* ([fp (add-horizontal-panel this)]
                  ; [modi (make-modifier-menu fp)]
                  [type (make-text-field fp TYPE)]
                  [name (make-text-field
                         fp NAME (lambda (x e) (add-on-return x e)))]
                  [get-values 
                   (lambda () ; (send modi get-string-selection)
                     (list (send type get-value) (send name get-value)))])
             (add-field-name name)
             (send fields add type get-values)
             (make-delete-button this fp (lambda () 
                                           (send fields remove type)
                                           (remove-field-name name)))
             (values type name))]
          [(a-field)
           (let-values ([(type name) (add)])
             (send type set-value (car a-field))
             (send name set-value (cadr a-field)))]))
      
      ;; --------------------------------------------------------------------
      ;; managing the fields of the class 
      
      (define fields (new assoc%))
      
      (define/public (produce)
        (foldr ;; r gives me the right order
         (lambda (v r)
           (let* ([type (string-trim-both (car v))]
                  [name (string-trim-both (cadr v))])
             (cond
               [(and (java-id? type) (java-id? name))
                (cons (list type name) r)]
               [(java-id? type) 
                (error-message (format CHECK-FIELD-NAME-F type))]
               [(java-id? name)
                (error-message (format CHECK-TYPE-F name))]
               [else r])))
         '()
         (map (lambda (th) (th)) (send fields list))))))
  
  
  ;; ------------------------------------------------------------------------
  ;; -> UnionInfo
  ;; get information about a datatype union 
  (define union-info%
    (class class-union-wizard%
      (super-new)
      (inherit-field info-pane)
      (inherit
        tostring? template? error-message an-error? produce-name-from-text)
      
      ;; --------------------------------------------------------------------
      ;; filling in the info-pane 
      
      (define type-pane 
        (new vertical-panel% (parent info-pane) 
             (alignment '(center center)) (style '(border))
             (min-height 50) (stretchable-height #f)))
      
      (define purpose 
        (new text-field% 
             (parent type-pane) (label PURPOSE-UNION) (callback void)))
      (define type-text (make-text-field type-pane TYPE))
      ;; -> String
      (define (get-type) (produce-name-from-text type-text TYPE))
      
      (define vart-panel 
        (new variant-panel%
             (parent info-pane)
             (get-type (lambda () (get-type)))
             (error-message (lambda (x) (error-message x)))
             (an-error? (lambda (x) (an-error? x)))))
      
      ;; Information about the union's common fields:
      (add-button info-pane "Add Common Field"
                  (lambda (x e) (send field-panel add)))
      (define field-panel
        (new field-panel%
             (parent info-pane) (window this) 
             (error-message (lambda (x) (error-message x)))))
      (send field-panel add)
      
      ;; -> Union 
      (define/override (produce)
        (with-handlers ([(lambda (x) (an-error? x)) (lambda _ #f)])
          (list 
           (make-dt (get-type)
                    (send field-panel produce)
                    (send vart-panel produce)
                    (send purpose get-value))
           (tostring?)
           (template?))))
      
      (define/override (make-class-cb x e)
        (when (produce) (send this show #f)))
      
      (define/override (add-field-cb x e)
        (send vart-panel add))
      
      ;; make two variants to boot
      ;; allow people to add and delete a variant 
      (send vart-panel add)
      (send vart-panel add)))
  
  ;; (-> String) (String -> Void) (Any -> Boolean) -> VariantPanel
  ;; manage the variant panels and their content for union
  (define variant-panel%
    (class horizontal-panel% 
      (super-new (alignment '(center center)) (style '(border))
                 (min-height 150) (stretchable-height #f))
      (init get-type error-message an-error?)
      
      ;; -> Void
      (define/public (add)
        (let* ([vp (new vertical-panel% (parent this)(style '(border)))]
               [ms (new message% (parent vp) (label VARIANT))] 
               [bt (new button% (parent vp) (label EDIT)   
                        (callback (create-variant ms)))])
          (send variants add bt #f)
          (make-delete-button this vp (lambda () (send variants remove bt)))))
      
      ;; Message -> (Button Event -> Void)
      (define (create-variant ms)
        (lambda (bt evt)
          (with-handlers ([an-error? void])
            (let*-values
                ([(type) (get-type)] ;; may raise an error message
                 [(class-in) (send variants lookup bt)]
                 [(b a-class) (send (new class-info% 
                                         (title VARIANT-WIZD)
                                         (insert-str INSERT-VARNT)
                                         (switches? #f)
                                         (add-str ADD-FIELD)
                                         (a-super type)
                                         (a-v-class (if class-in class-in '())))
                                    call)])
              (when a-class
                (let* ([a-class (car a-class)]
                       [name    (car a-class)])
                  ;; no supertype needed: ;; remove (cadr a-class)
                  ;; comments, if any, are in:  (cadddr a-class)
                  (send variants update bt 
                        (list name (caddr a-class) (class-purpose a-class)))
                  (send ms set-label name)))))))
      
      ;; --------------------------------------------------------------------
      ;; Managing the datatype: (list name fields [comment])
      (define variants (new assoc%))
      
      (define/public (produce)
        (foldr (lambda (f r) (if f (cons f r) r)) '()
               (send variants list)))))
  
  ;; ------------------------------------------------------------------------
  ;; Pane -> HorizontalPanel
  ;; add a fixed-width horizontal panel (50) to p
  (define (add-horizontal-panel p)
    (new horizontal-panel% (parent p) (min-height 50) (stretchable-height #f)))
  
  ;; String CallBack -> Button
  (define (add-button bp l cb) ;; to button-panel
    (new button% (label l) (parent bp) (callback cb)))
  
  ;; Panel String -> CheckBox
  (define (make-checkbox p l)
    (new check-box% (parent p) (label l) (callback void)))
  
  ;; Panel String [Callback] -> TextField
  (define make-text-field 
    (opt-lambda (p l (c void))
      (new text-field% 
           (parent p) (label l) (callback c)
           (min-width 50) (stretchable-width #f))))
  
  ;; Panel (-> Void) -> Button
  ;; create a button that deletes the button's immediate container from this
  (define (make-delete-button this vp delete-from-model)
    (new button% (parent vp) (label DELETE)
         (callback (lambda (x e)
                     (delete-from-model)
                     (send this change-children 
                           (lambda (cs)
                             (filter (lambda (c) (not (eq? vp c))) cs)))))))
  
  ;; ------------------------------------------------------------------------
  #| Run, program, run:  
 
  (require (file "class.scm") (file "draw-txt.ss"))
  
  (define-values (b x) (get-class-info))
  (if (and x b) (printf "/*~n~a~n*/~n" (class-draw (car x))))
  (if x (printf "~a~n" (apply make-class x)))
  
  (define-values (c y) (get-union-info))
  (if (and y c) (printf "/*~n~a~n*/~n" (dt-draw (car y))))
  (if y (printf "~a~n" (apply make-union y)))

 |#  
  )
