(module class-traced mzscheme
  
  ;; All of the implementation is actually in private/class-internal.rkt,
  ;;  which provides extra (private) functionality to contract.rkt.
  (require racket/private/class-internal)
  
  (provide (rename class-traced class)
           (rename class*-traced class*)
           (rename class/derived-traced class/derived)
	   (rename define-serializable-class-traced define-serializable-class)
           (rename define-serializable-class*-traced define-serializable-class*)
           class?
	   (rename mixin-traced mixin)
           interface interface?
	   object% object? externalizable<%>
           object=?
	   (rename new-traced new)
           (rename make-object-traced make-object)
           (rename instantiate-traced instantiate)
	   (rename send-traced send)
           (rename send/apply-traced send/apply)
           (rename send*-traced send*)
           (rename class-field-accessor-traced class-field-accessor)
           (rename class-field-mutator-traced class-field-mutator)
           (rename with-method-traced with-method)
           (rename get-field-traced get-field)
           (rename set-field!-traced set-field!)
           (rename field-bound?-traced field-bound?)
           (rename field-names-traced field-names)
	   private* public*  pubment*
	   override* overment*
	   augride* augment*
	   public-final* override-final* augment-final*
	   define/private define/public define/pubment
	   define/override define/overment
	   define/augride define/augment
	   define/public-final define/override-final define/augment-final
	   define-local-member-name define-member-name member-name-key generate-member-key
	   (rename generic-traced generic)
           (rename make-generic-traced make-generic)
           (rename send-generic-traced send-generic)
	   (rename is-a?-traced is-a?)
           subclass? implementation? interface-extension?
	   (rename object-interface-traced object-interface)
           (rename object-info-traced object-info)
           (rename object->vector-traced object->vector)
	   (rename object-method-arity-includes?-traced object-method-arity-includes?)
	   method-in-interface? interface->method-names class->interface class-info
	   (struct exn:fail:object ())
	   make-primitive-class

	   ;; "keywords":
	   private public override augment
	   pubment overment augride
           public-final override-final augment-final
	   field init init-field
	   rename-super rename-inner inherit inherit-field
	   this super inner
	   super-make-object super-instantiate super-new
	   inspect))
