(module class mzscheme
  
  ;; All of the implementation is actually in private/class-internal.ss,
  ;;  which provides extra (private) functionality to contract.ss.
  (require "private/class-internal.ss")
  
  (provide class class* class/derived
	   define-serializable-class define-serializable-class*
           class?
	   mixin
           interface interface?
	   object% object? externalizable<%>
           object=?
	   new make-object instantiate
	   send send/apply send* class-field-accessor class-field-mutator with-method
           get-field field-bound? field-names
	   private* public*  pubment*
	   override* overment*
	   augride* augment*
	   public-final* override-final* augment-final*
	   define/private define/public define/pubment
	   define/override define/overment
	   define/augride define/augment
	   define/public-final define/override-final define/augment-final
	   define-local-member-name define-member-name 
           member-name-key generate-member-key 
           member-name-key? member-name-key=? member-name-key-hash-code
	   generic make-generic send-generic
	   is-a? subclass? implementation? interface-extension?
	   object-interface object-info object->vector
	   object-method-arity-includes?
	   method-in-interface? interface->method-names class->interface class-info
	   (struct exn:fail:object ())
	   make-primitive-class

	   ;; "keywords":
	   private public override augment
	   pubment overment augride
           public-final override-final augment-final
	   field init init-field init-rest
	   rename-super rename-inner inherit inherit/super inherit/inner inherit-field
	   this super inner
	   super-make-object super-instantiate super-new
	   inspect))
