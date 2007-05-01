(module view mzscheme 
  
  (require "assoc-list.scm"
           "aux-class.scm"
           "data-defs.scm"
           "class.scm"
           "union.ss"
           (lib "mred.ss" "mred")
           (lib "class.ss")           
           (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss" "srfi" "13")
           (lib "contract.ss"))
  
  (provide/contract
   [get-class-info (opt->* () [Language] [boolean? (union false/c (list/c Class boolean? boolean?))])]
   [get-union-info (opt->* () [Language] [boolean? (union false/c (list/c Union boolean? boolean?))])])
  
  (define CLASS-WIZARD "The Class Wizard")
  (define UNION-WIZARD "The Union Wizard")
  (define VARIANT-WIZD "The Variant Wizard")
  (define VARIANT      "Variant") 
  (define INSERT-CLASS "Insert Class")
  (define INSERT-UNION "Insert Union")
  (define INSERT-VARNT "Insert Variant")
  (define ADD-FIELD    "Add Field")
  (define ADD-VARIANT  "Add Variant")
  (define ADD-INTERF   "Add Interface Method")
  (define ADD-TOSTRING "add toString()")
  (define ADD-TEMPLATE "add method template")
  (define ADD-DIAGRAM  "add class diagram")
  (define PURPOSE-CLASS "// purpose of class: ")
  (define PURPOSE-UNION  "// purpose of union: ")
  (define CLASS        "class")
  (define SUPER        "super")
  (define IMPLEMENTS   "implements")
  (define EXTENDS      "extends")
  (define CHECK-NAME-F "check name of ~a")
  (define CHECK-TYPE-F "check type for ~a")
  (define CHECK-FIELD-NAME-F "check field name for ~a")
  (define TYPE "type") 
  (define NAME "name")
  (define ABORT "Cancel")
  (define ERROR "Error")
  (define DELETE "Delete")
  (define EDIT   "Edit")
  
     #|
     present a dialog to create a single class; 
     if programmer aborts, return #f
     otherwise, produce a class and two booleans, requesting toString and draft 
     templates, respectively
     |#
  
  (define (get-class-info . opt)
    (define ci (new class-info% (title CLASS-WIZARD) 
                    (switches? (and (pair? opt) (not (eq? (car opt) BEGINNER))))
                    (insert-str INSERT-CLASS) (add-str ADD-FIELD)))
    (send ci call))
  
  (define (get-union-info . opt)
    (define ui (new union-info% (title UNION-WIZARD)
                    (switches? (and (pair? opt) (not (eq? (car opt) BEGINNER))))
                    (insert-str INSERT-UNION) (add-str ADD-VARIANT)))
    (send ui call))
  
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
            | call                |
            | A: produce          |
            | A: make-class-cb    |
            *---------------------*
              |
              |
             / \
        --------------------------------------------
        |                                          |
    *---------------------*                    *---------------------*
    | class-info%         |                    | union-info%         |
    *---------------------*                    *---------------------*
    |                     |--+                 | vart-panel          |--+
    *---------------------*                    *---------------------*  |
                                                                        |          
                                                                        |          
                                                                        |          
                                                                        |          
    *---------------------*                    *---------------------*  | 
    | vertical-panel%     |                    | horizontal-panel%   |  |          
    *---------------------*                    *---------------------*  |          
                      |                                          |      |          
                      |                                          |      |          
                     / \                                        / \     |          
                 *---------------------*                  *---------------------*
                 | field-panel%        |                  | variant-panel%      |
                 *---------------------*                  *---------------------*
                 | add                 |                  | add                 |
                 | add-on-return       |                  | produce             |
                 | produce             |                  *---------------------*
                 *---------------------*                  | acquired:           |
                 | acquired:           |                  |  get-type           |
                 |  window (?)         |                  |  error-message      |
                 |  error-message      |                  *---------------------* 
                 *---------------------*                  

    |#
  
  ;; ------------------------------------------------------------------------
  ;; Set up the frame, including the info-panel where subclasses can 
  ;; request specific information. The frame includes buttons for aborting
  ;; the edit process, for packaging up the information in the edit, and for 
  ;; adding some component (field, variant)
  
  ;; String String String -> ClassUnionWizard
  (define class-union-wizard%
    (class dialog% (init-field title insert-str add-str (switches? #t) (no-diagram #f))
      (super-new (label title) (width 500) (height 400))
      
      (define p (new vertical-pane% (parent this)))
      
      ;; switches for toString methods and template in comments 
      (define switch-pane (add-horizontal-panel p))
      (define-values (string #;template diagram)
        (cond
          [switches? 
           (values (make-checkbox switch-pane ADD-TOSTRING)
   	           #;
                   (let ([c (make-checkbox switch-pane ADD-TEMPLATE)])
                     (send c set-value #t)
                     c)
                   (make-checkbox switch-pane ADD-DIAGRAM))]
          [no-diagram (values #f #;#f #f)]
          [else (values #f #;#f (make-checkbox switch-pane ADD-DIAGRAM))]))          
      (define (get-switch x) 
        (cond 
          [(eq? x diagram) (and (not no-diagram) (send x get-value))]
          [switches? (send x get-value)]
          [else #f]))
      (define/public (tostring?) (get-switch string))
      (define/public (template?) #;(get-switch template) #f)
      (define/public (diagram?) (get-switch diagram))
      
      ;; --------------------------------------------------------------------
      ;; info panel
      (field (info-pane (new vertical-panel% (parent p) (style '(border)))))
      
      ;; --------------------------------------------------------------------
      ;; error handling
      
      ;; String -> false
      (define/public (error-message ctl m)
	(when (ctl . is-a? . text-field%)
	  (send ctl focus)
	  (let ([e (send ctl get-editor)])
	    (send e set-position 0 (send e last-position))))
	(message-box ERROR
		     m
		     (send ctl get-top-level-window)
		     '(ok stop))
        (raise an-error))

      ;; TextField (union false String) -> java-id?
      (define/public (produce-name-from-text name msg)
        (let ([x (string-trim-both (send name get-value))])
          (cond
            [(not msg) x]
            [(java-id? x) x]
            [else (error-message name (format CHECK-NAME-F msg))])))

      ;; --------------------------------------------------------------------
      ;; Buttons

      (define button-panel
	(new horizontal-panel% (parent p) (stretchable-height #f) (alignment '(right center))))

      (define abort? #t) ;; assume bad things happen
      (define (quit x e) (send this show #f))
      (add-button button-panel ABORT quit)
      
      (define/abstract make-class-cb)
      (new button% (label insert-str) (parent button-panel) 
	   (style '(border))
	   (callback
	    (lambda (x e) 
	      (when (make-class-cb x e)
		(set! abort? #f)))))
      
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
      (inherit tostring? template? diagram? error-message produce-name-from-text)
      
      ;; --------------------------------------------------------------------
      ;; filling the info-pane 
      
      ;; Information about the class in general: 
      (define purpose 
        ;; it is not the kind of textbox that make-text-field creates
        (new text-field% (parent info-pane) (label PURPOSE-CLASS) (callback void)))
      
      (define class-pane (add-horizontal-panel info-pane))
      ; (define class-privacy (make-modifier-menu class-pane))
      (define class-name (make-text-field class-pane CLASS))
      (define (super-cb x e) (send field-panel add-on-return x e))
      (define super-name (make-text-field class-pane IMPLEMENTS super-cb))
      
      ;; Information about the class's fields:
      (define field-pane (new vertical-panel% (parent info-pane) (style '(border))))
      (define field++ (add-button field-pane ADD-FIELD (lambda (x y) (send field-panel add))))
      (define field-panel
        (new field-panel%
             (parent field-pane) (window this) 
             (error-message (lambda (ctl x) (error-message ctl x)))))
      
      ;; --------------------------------------------------------------------
      ;; creating the class from the specification 
      
      ;; -> (union false (list Class boolean? boolean?))
      (define/override (produce)           
        (with-handlers ([an-error? (lambda _ #f)]) 
          (list (list (produce-name-from-text class-name CLASS)
                      (produce-name-from-text
                       super-name (if (null? a-super) #f SUPER))
                      (send field-panel produce)
                      (send purpose get-value))
                (tostring?)
                (template?))))
      
      ;; if the class specification is proper, hide dialog
      (define/override (make-class-cb x e) 
        (and (produce) (send this show #f)))
      
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
           (send purpose set-value (variant-purpose a-v-class)))])))

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
           (send window begin-container-sequence)
           (let* ([fp (add-horizontal-panel this)]
                  ; [modi (make-modifier-menu fp)]
                  [type (make-text-field fp "   ")]
                  [name (make-text-field fp "" (lambda (x e) (add-on-return x e)))]
                  [get-values 
                   (lambda () ; (send modi get-string-selection)
		     (list type name))])
             (send type set-value "<field type>")
             (send name set-value "<field name>")
             (add-field-name name)
             (send fields add type get-values)
             (make-delete-button this fp (lambda () 
                                           (send fields remove type)
                                           (remove-field-name name)))
             (send window end-container-sequence)
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
           (let* ([type-ctl (car v)]
		  [name-ctl (cadr v)]
		  [type (string-trim-both (send type-ctl get-value))]
		  [name (string-trim-both (send name-ctl get-value))])
             (cond
               [(and (java-id? type) (java-id? name))
                (cons (list type name) r)]
               [(java-id? type) 
                (error-message name-ctl (format CHECK-FIELD-NAME-F type))]
               [(java-id? name)
                (error-message type-ctl (format CHECK-TYPE-F name))]
               [else r])))
         '()
         (map (lambda (th) (th)) (send fields list))))))
  
  ;; ---------------------------------------------------------------------------
  ;; -> UnionInfo
  ;; get information about a datatype union 
  (define union-info%
    (class class-union-wizard%
      (super-new)
      (inherit-field info-pane switches?)
      (inherit tostring? template? error-message produce-name-from-text)
      
      ;; -----------------------------------------------------------------------
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
      
      ;; --- the variants of the union 
      (define meth-pane (new vertical-panel% (parent info-pane) (style '(border))))
      (add-button meth-pane ADD-INTERF (lambda (x y) (send methods add)))
      (define methods (new methods-pane% (window meth-pane) (error-message (lambda (ctl x) (error-message ctl x)))))
      (send methods add)
      (unless switches? 
        (send info-pane change-children (lambda (x) (remq meth-pane x))))
      
      ;; --- the variants of the union 
      (define vart-pane (new vertical-panel% (parent info-pane) (style '(border))))
      (add-button vart-pane ADD-VARIANT (lambda (x y) (send vart-panel add)))
      (define vart-panel 
        (new variant-panel%
             (parent vart-pane)
             (get-type (lambda () (get-type)))
             (error-message (lambda (ctl x) (error-message ctl x)))))
      

      ;; -> Union 
      (define/override (produce)
        (with-handlers ([an-error? (lambda _ #f)])
          (define m (send methods produce))
          (list 
           (make-dt (get-type)
                    m
                    (send vart-panel produce)
                    (send purpose get-value))
           (tostring?)
           (template?))))
      
      (define/override (make-class-cb x e)
        (and (produce) (send this show #f)))
      
      ;; make two variants to boot
      ;; allow people to add and delete a variant 
      (send vart-panel add)
      (send vart-panel add)))
  
  ;; get information about all panels 
  (define methods-pane% 
    (class vertical-panel% 
      (init-field window error-message)
      (super-new (parent window) (min-height 10) (stretchable-height #f))
      
      (define methods (new assoc%))
      
      ;; add a pane to the window for specifying one method signature 
      (define/public (add)
        (send window begin-container-sequence)
        (new method-panel% 
             (parent window) (style '(border))
             (window window) (error-message error-message) (methods methods))
        (send window end-container-sequence))
      
      (define/public (produce) (send methods list))))
  
  ;; get information about a single method signature  
  (define method-panel%
    (class horizontal-panel%
      (init-field window error-message methods)
      (super-new)

      ;; -----------------------------------------------------------------------
      ;; the callbacks 
      ;; remove this pane from the window and its information from the table 
      (define (remove _1 _2)
        (send methods remove this)
        (send this begin-container-sequence)
        (send window change-children (lambda (x) (remq this x)))
        (send window container-flow-modified)
        (send this end-container-sequence))
      
      ;; [Listof TextField%]       
      (define pa* '())
      ;; (union false '_) '_ -> Void
      ;; add this parameter TextField% to pane 
      (define (add-parameter-field button-or-false _2)
        (define _ (send this begin-container-sequence))
        (define x (make-text-field this (if button-or-false "," "") void pt))
        (set! pa* (append pa* (list x)))
        (send this change-children 
              (lambda (y)
                (let loop ([y y])
                  (if (eq? (cadr y) pa+)
                      (set-cdr! y (cons x end)) 
                      (loop (cdr y))))
                y))
        (send this end-container-sequence))
      
      ;; re-establish this pane so that programmers can edit the method info
      (define (edit _1 _2)
        (send this begin-container-sequence)
        (send this change-children 
              (lambda (y) (append (list (car y)) (list ret nam opn) pa* end)))
        (send this end-container-sequence))
      
      ;; retrieve, check, add method signature to table 
      (define (convert-info-to-signature button event)
        (with-handlers ([an-error? (lambda (x) #f)])
          (define sig 
	    (let ([ctls (cons nam (cons ret pa*))])
	      (check-sig 
	       (map (lambda (x) (send x get-value)) ctls)
	       ctls)))
          (define _ (send this begin-container-sequence))
          (define t (new message% (parent this) (label (method sig))))
          (define e (new button% (parent this) (label EDIT) (callback edit)))
          (send this change-children (lambda (y) (cons (car y) (list t e))))
          (send e focus)
          (send methods add this sig)
          (send this end-container-sequence)))
      ;; (cons String (cons String (Listof String))) -> Method 
      ;; check signature
      (define (check-sig sig ctls)
        (define name (string-trim-both (car sig)))
        (define typ* (map string-trim-both (cdr sig)))
        (unless (java-id? name)
          (error-message (car ctls) (format "not a java id: ~s" name)))
        (let ([typ*
               (let loop ([types* typ*][ctls (cdr ctls)])
                 (cond
                   [(null? types*) '()]
                   [(string=? (car types*) "")
                    (if (null? (cdr types*))
                        '()
                        (error-message (car ctls) bad-para))]
                   [else 
                    (if (java-id? (car types*))
                        (cons (car types*) (loop (cdr types*) (cdr ctls)))
                        (error-message (car ctls) (format no-type (car types*))))]))])
          (cons (car typ*) (cons name (cdr typ*)))))
      (define bad-para 
        "\"\" parameter type found, but not at the end of the parameter list")
      (define no-type "not a java type: ~s")
      
      (define pt  "<parameter type>")
      ;; ---------------------------------------------------------------------
      ;; now set up the one-line pane for specifying a method signature 
      (send window begin-container-sequence)      
      (define sub (new button% (parent this) (label "-") (callback remove)))
      ;; (make-delete-button ... when purpose statement is added/? 
      (define ret (make-text-field this "" void "<return type>"))
      (define nam (make-text-field this "" void "<method name>"))
      (define opn (new message% (parent this) (label "(")))
      (define pa+ (new button% (parent this) (label ", ...") (callback add-parameter-field)))
      (define cls (new message% (parent this) (label ")")))
      (define add (new button% (parent this) (label "+") (callback convert-info-to-signature)))
      (define end (list pa+ cls add))
      ;; ---------------------------------------------------------------------
      (add-parameter-field #f '__)
      (send window end-container-sequence)))
  
  ;; (-> String) (String -> Void) (Any -> Boolean) -> VariantPanel
  ;; manage the variant panels and their content for union
  (define variant-panel%
    (class horizontal-panel% 
      (super-new (alignment '(center center)) (min-height 150) (stretchable-height #f))
      (init get-type error-message)
      
      ;; -> Void
      (define/public (add)
        (send this begin-container-sequence)
        (let* ([vp (new vertical-panel% (parent this)(style '(border)))]
               [ms (new message% (parent vp) (label VARIANT))] 
               [bt (new button% (parent vp) (label EDIT)   
                        (callback (create-variant ms)))])
          (send variants add bt #f)
          (make-delete-button this vp (lambda () (send variants remove bt)))
          (send this end-container-sequence)))
      
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
                                         (switches? #f) (no-diagram #t)
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
    (opt-lambda (p l (c void) (init ""))
      (new text-field% 
           (parent p) (label l) (callback c) (init-value init)
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
  
  (define an-error (cons 1 2))
  ;; Any -> Boolean 
  (define (an-error? x) (eq? an-error x))
  
  ;; ------------------------------------------------------------------------
  #| Run, program, run: 

  (require (file "draw-txt.ss"))

  #| |#
  (define-values (b x) (get-class-info BEGINNER))
  (if (and x b) (printf "/*~n~a~n*/~n" (class-draw (car x))))
  (if x (printf "~a~n" (apply make-class x)))
   
  #||#  
  (define-values (c y) (get-union-info #;BEGINNER INTERMEDIATE))
  (if (and y c) (printf "/*~n~a~n*/~n" (dt-draw (car y))))
  (if y (printf "~a~n" (apply make-union (append y (list INTERMEDIATE)))))
  |#
  )
