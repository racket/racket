
(module sig mzscheme

  (require (lib "unitsig.ss"))

  (provide compiler:option^
	   compiler^
	   compiler:inner^
	   compiler:linker^)

  ;; Compiler options
  (define-signature compiler:option^
     (verbose ; default = #f
   
      setup-prefix ; string to embed in public names;
                   ; used mainly for compiling extensions
                   ;  with the collection name so that 
                   ;  cross-extension conflicts are less
                   ;  likely in architectures that expose
                   ;  the public names of loaded extensions
                   ; default = ""
   
      clean-intermediate-files ; #t => keep intermediate .c/.o files
                               ; default = #f
   
      compile-subcollections   ; #t => use 'compile-subcollections
                               ;  from infor for collection compiling
                               ; default = #t
   
      compile-for-embedded  ; #f => make objects to be linked
                            ; directly with MzScheme, not dynamically
                            ; loaded; default = #f
   
      max-inline-size      ; max size of inlined procedures
   
      disable-interrupts   ; #t => UNSAFE: turn off breaking, stack
                           ; overflow, and thread switching;
                           ; default = #f
      unsafe               ; #t => UNSAFE: omit some type checks
                           ; default = #f
      fixnum-arithmetic    ; #t => UNSAFE: don't check for overflow or
                           ; underflow for fixnum arithmetic;
                           ; default = #f
   
      propagate-constants  ; default = #t
      assume-primitives    ; #t => car = #%car; default = #f
      stupid               ; allow obvious non-syntactic errors;
                           ;  e.g.: ((lambda () 0) 1 2 3)
   
      vehicles             ; Controls how closures are compiled:
   			;  'vehicles:automatic,
                           ;  'vehicles:functions,
                           ;  'vechicles:units, or
                           ;  'vehicles:monolithic.
                           ; default = 'vehicles:automatic
      vehicles:monoliths   ; Size for 'vehicles:monolithic
      seed                 ; Randomizer seed for 'vehicles:monolithic
   
      max-exprs-per-top-level-set ; Number of top-level Scheme expressions
   			       ; crammed into one C function; default = 25
   
      unpack-environments  ; default = #t
   		        ; Maybe #f helps for register-poor architectures?
   
      debug ; #t => creates debug.txt debugging file
      test  ; #t => ignores top-level expressions with syntax errors
      ))

  ;; Compiler procedures
  (define-signature compiler^
    (compile-extensions
     compile-extensions-to-c
     compile-c-extensions

     compile-extension-parts
     compile-extension-parts-to-c
     compile-c-extension-parts

     link-extension-parts
     glue-extension-parts

     compile-zos

     compile-collection-extension
     compile-collection-zos

     current-compiler-dynamic-require-namespace))

  ;; Low-level extension compiler interface
  (define-signature compiler:inner^
    (compile-extension
     compile-extension-to-c
     compile-c-extension
     compile-extension-part
     compile-extension-part-to-c
     compile-c-extension-part
     eval-compile-prefix))
  
  ;; Low-level multi-file extension linker interface
  (define-signature compiler:linker^
    (link-extension
     glue-extension)))
