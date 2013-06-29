(module marks scheme/base

  (require mzlib/list
	   mzlib/contract
           (prefix-in mz: mzscheme))

  (define-struct full-mark-struct (module-name source label bindings values))

  ; CONTRACTS
  (define mark? (-> ; no args  
                 full-mark-struct?))
  (define mark-list? (listof procedure?))
  
  (provide/contract 
   ;[make-debug-info (-> any? binding-set? varref-set? any? boolean? syntax?)] ; (location tail-bound free label lifting? -> mark-stx)
   [expose-mark (-> mark? (list/c any/c symbol? (listof (list/c identifier? any/c))))]
   ;[make-top-level-mark (syntax? . -> . syntax?)]
   [lookup-all-bindings ((identifier? . -> . boolean?) mark-list? . -> . (listof any/c))]
   [lookup-first-binding ((identifier? . -> . boolean?) mark-list? ( -> any) . -> . any)]
   [lookup-binding (mark-list? identifier? . -> . any)])
  
  (mz:provide
   make-debug-info
   assemble-debug-info
   wcm-wrap
   skipto-mark?
   skipto-mark
   strip-skiptos
   mark-list?
   mark-module-name
   mark-source
   mark-bindings
   mark-label
   mark-binding-value
   mark-binding-binding
   mark-binding-set!
   display-mark
   all-bindings
   #;lookup-binding-list
   debug-key
   extract-mark-list
   make-normal-breakpoint-info normal-breakpoint-info-mark-list normal-breakpoint-info-kind
   make-error-breakpoint-info error-breakpoint-info-message
   (struct breakpoint-halt ())
   make-expression-finished expression-finished-returned-value-list)
  
  ; BREAKPOINT STRUCTURES
  
  (define-struct normal-breakpoint-info (mark-list kind))
  (define-struct error-breakpoint-info (message))
  (define-struct breakpoint-halt ())
  (define-struct expression-finished (returned-value-list))
  
  (define-struct skipto-mark-struct ())
  (define skipto-mark? skipto-mark-struct?)
  (define skipto-mark (make-skipto-mark-struct))
  (define (strip-skiptos mark-list)
    (filter (lambda (x) (#%plain-app not (skipto-mark? x))) mark-list))
  
    
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key-struct ())
  (define debug-key (make-debug-key-struct))
  
  (define (extract-mark-list mark-set)
    (strip-skiptos (continuation-mark-set->list mark-set debug-key)))
   
  
  ; the 'varargs' creator is used to avoid an extra cons cell in every mark:
  (define (make-make-full-mark-varargs module-name source label bindings)
    (lambda (values)
      (make-full-mark-struct module-name source label bindings values)))
  
  ; see module top for type
  (define (make-full-mark module-name source label bindings assembled-info-stx)
    (mz:datum->syntax-object #'here `(#%plain-lambda () 
                                       (#%plain-app 
                                        ,(make-make-full-mark-varargs module-name source label bindings)
                                        ,assembled-info-stx))))
  
  (define (mark-module-name mark)
    (full-mark-struct-module-name (mark)))
  
  (define (mark-source mark)
    (full-mark-struct-source (mark)))
    
  ; : identifier -> identifier
  (define (make-mark-binding-stx id)
    #`(case-lambda
        [() #,id]
        [(v) (set! #,id v)]))
  
  (define (mark-bindings mark)
    (map list 
         (full-mark-struct-bindings (mark)) 
         (full-mark-struct-values (mark))))
  
  (define (mark-label mark)
    (full-mark-struct-label (mark)))
  
  (define (mark-binding-value mark-binding)
    ((cadr mark-binding)))
  
  (define (mark-binding-binding mark-binding)
    (car mark-binding))

  (define (mark-binding-set! mark-binding v)
    ((cadr mark-binding) v))
  
  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (mark-binding-binding binding)
                         (mark-binding-value binding)))
                 bindings))))
  
  (define (display-mark mark)
    (apply
     string-append
     (format "source: ~a\n" (mz:syntax-object->datum (mark-source mark)))
     (format "label: ~a\n" (mark-label mark))
     (format "bindings:\n")
     (map (lambda (binding)
                 (format " ~a : ~a\n" (syntax-e (mark-binding-binding binding))
                         (mark-binding-value binding)))
               (mark-bindings mark))))
  
  
  ; possible optimization: rig the mark-maker to guarantee statically that a
  ; variable can occur at most once in a mark.  
  
  (define (binding-matches matcher mark)
    (filter (lambda (binding-pair) (matcher (mark-binding-binding binding-pair))) (mark-bindings mark)))
  
  (define (lookup-all-bindings matcher mark-list)
    (apply append (map (lambda (m) (binding-matches matcher m)) mark-list)))
  
  (define (lookup-first-binding matcher mark-list fail-thunk)
    (let ([all-bindings (lookup-all-bindings matcher mark-list)])
      (if (null? all-bindings)
          (fail-thunk)
          (car all-bindings))))
  
  (define (lookup-binding mark-list id)
    (mark-binding-value
     (lookup-first-binding (lambda (id2) (mz:module-identifier=? id id2)) 
                           mark-list 
                           (lambda ()
                             (error 'lookup-binding "variable not found in environment: ~a\n" (if (syntax? id) 
                                                                                                  (mz:syntax-object->datum id)
                                                                                                  id))))))
  
  (define (all-bindings mark)
    (map mark-binding-binding (mark-bindings mark)))
  
  (define (wcm-wrap debug-info expr)
    (quasisyntax/loc expr (with-continuation-mark #,debug-key #,debug-info #,expr)))

  
  ; DEBUG-INFO STRUCTURES
  
  ;;;;;;;;;;
  ;;
  ;; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ;; a source expression and a set of binding/value pairs.
  ;; (syntax-object BINDING-SET VARREF-SET any boolean) -> debug-info)
  ;;
  ;;;;;;;;;;
     
  (define (make-debug-info module-name source tail-bound free-vars label lifting? assembled-info-stx)
    (make-full-mark module-name source label free-vars assembled-info-stx))
  
  (define (assemble-debug-info tail-bound free-vars label lifting?)
    (map make-mark-binding-stx free-vars))
  
  #;
  (define (make-top-level-mark source-expr)
    (make-full-mark source-expr 'top-level null)))
