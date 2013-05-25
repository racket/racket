;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> This module contains only syntax definitions, which makes Swindle closer
;;> to CLOS -- making the object system much more convenient to use.

#lang s-exp swindle/turbo

(require swindle/tiny-clos)
(provide (all-from swindle/tiny-clos))

;;; ---------------------------------------------------------------------------
;;; General helpers

(defsyntax (args-arity stx)
  (syntax-case stx ()
    [(_ args)
     (let loop ([args #'args] [n 0])
       (syntax-case args ()
         [(a . more)
          (or (not (identifier? #'a))
              ;; stop at &-keyword
              (let ([sym (syntax-e #'a)])
                (or (eq? sym '||)
                    (not (eq? #\& (string-ref (symbol->string sym) 0))))))
          (loop #'more (add1 n))]
         [() (datum->syntax-object stx n stx)]
         [_  (quasisyntax/loc stx (make-arity-at-least #,n))]))]))

;;; ---------------------------------------------------------------------------
;;; Generic macros

;;>>... Generic macros

;;>> (generic)
;;> | (generic name initargs ...)
;;> | (generic name (arg ...) initargs ...)
;;>   Create a generic function object (an instance of the
;;>   `*default-generic-class*' parameter).  The first form uses the default
;;>   name given by the syntactical context, the second one gets an explicit
;;>   name and the third also gets a list of arguments which is used to
;;>   count the required number of arguments.  If there is no argument list
;;>   to count, the first method that gets added will set this number.  The
;;>   two last forms allow initargs to be passed to the <generic> instance
;;>   creation, for example, to specify a `:combination' argument.  (The
;;>   first form does not allow keywords, since a keyword would be taken as
;;>   the name.)
(defsyntax* (generic stx)
  (syntax-case stx ()
    [(_)
     #`(make (*default-generic-class*) :name '#,(syntax-local-name))]
    [(_ name) (identifier? #'name)
     #'(make (*default-generic-class*) :name 'name)]
    [(_ name initarg initargs ...)
     (and (identifier? #'name) (syntax-keyword? #'initarg))
     #'(make (*default-generic-class*) initarg initargs ... :name 'name)]
    [(_ name args) (identifier? #'name)
     #`(make (*default-generic-class*)
             :name 'name :arity (args-arity args))]
    [(_ name args initarg initargs ...)
     (and (identifier? #'name) (syntax-keyword? #'initarg))
     #`(make (*default-generic-class*)
             initarg initargs ... :name 'name :arity (args-arity args))]))

;;>> (defgeneric name (arg ...) initargs ...)
;;> | (defgeneric (name arg ...) initargs ...)
;;> | (defgeneric name initargs ...)
;;>   This form defines a generic function using the `generic' syntax given
;;>   above.  The last form doesn't specify a number of arguments.  Some
;;>   extra `initargs' can be specified too but they are needed mainly for a
;;>   `:combination' argument.
(defsyntax* (defgeneric stx)
  (let* ([ctx (syntax-local-context)]
         [ctx (cond [(pair? ctx) (car ctx)]
                    [(eq? ctx 'top-level) ctx]
                    [else #f])]
         [mark (lambda (name)
                 ((syntax-local-value #'generic-contexts-defined?) name ctx))])
    (syntax-case stx ()
      [(_ name args initargs ...) (identifier? #'name)
       (begin (mark #'name) #'(define name (generic name args initargs ...)))]
      [(_ (name . args) initargs ...) (identifier? #'name)
       (begin (mark #'name) #'(define name (generic name args initargs ...)))]
      [(_ name initargs ...) (identifier? #'name)
       (begin (mark #'name) #'(define name (generic name initargs ...)))])))

;; returns #t if an identifier id in context ctx is already defined as a genric
;; (used by defmethod to detect when it should expand to an add-method)
(define-syntax generic-contexts-defined?
  (let ([table (make-hash-table 'weak)])
    (lambda (id ctx)
      ;; ctx is either the first element of (syntax-local-context) or
      ;; 'top-level.  Note that top-level identifiers in different modules
      ;; should not be `module-identifier=?' (eg, `eval' takes care of this).
      (let ([cs (hash-table-get table ctx (lambda () '()))])
        (or (ormap (lambda (c) (module-identifier=? id c)) cs) ; defined
            (begin (hash-table-put! table ctx (cons id cs)) ; undefined
                   #f))))))

;;; ---------------------------------------------------------------------------
;;; Method macros

;;>>... Method macros

;;>> (call-next-method [args ...]) [*local*]
;;>> (next-method?) [*local*]
;;>   These are bindings which are available only in method bodies.
;;>   `call-next-method' will invoke the next method in a generic invocation
;;>   sequence if any.  If arguments are given to `call-next-method', it
;;>   will change the arguments for the next method -- but this is done when
;;>   the methods are already filtered and sorted, so the new arguments
;;>   should always be consistent with the old types.  If there are no
;;>   methods left, or when calling a method directly, or when a before or
;;>   after method is used, the `no-next-method' generic will be used --
;;>   normally resulting in an error.  `next-method?' returns `#t' if there
;;>   is another method ready to be called.

(defsyntax (make-method-specs/initargs stx)
  (syntax-case stx ()
    [(_ name args0 body . more)
     (let loop ([args #'args0] [specializers '()] [arguments '()])
       (syntax-case args (=)
         [([arg = val] . rest)
          (loop #'rest
                (cons #'(singleton val) specializers) (cons #'arg arguments))]
         [([arg type] . rest)
          (loop #'rest (cons #'type specializers) (cons #'arg arguments))]
         [([arg] . rest)
          (loop #'rest (cons #'<top> specializers) (cons #'arg arguments))]
         [(arg . rest)
          (and (identifier? #'arg)
               ;; stop at &-keyword
               (let ([sym (syntax-e #'arg)])
                 (or (eq? sym '||)
                     (not (eq? #\& (string-ref (symbol->string sym) 0))))))
          (loop #'rest (cons #'<top> specializers) (cons #'arg arguments))]
         [_ ; both null and rest argument
          (let* ([specializers (reverse specializers)]
                 [arguments    (reverse arguments)]
                 [name-e       (syntax-e #'name)]
                 [cnm (datum->syntax-object
                       #'args0 'call-next-method #'args0)])
            (unless (null? (syntax-e args))
              (set! arguments
                    (if (null? arguments) args (append arguments args))))
            (let ([makeit
                   (quasisyntax/loc stx
                     (make (*default-method-class*)
                           :specializers (list #,@specializers)
                           :name '#,(if name-e #'name (syntax-local-name))
                           :procedure
                           (lambda (#,cnm . #,arguments)
                             ;; See "Trick" in tiny-clos.rkt
                             ;; -- use a syntax to not do this unless needed
                             (letsyntax
                                 ([#,(datum->syntax-object
                                      #'args0 'next-method? #'args0)
                                   (lambda (stx)
                                     (syntax-case stx ()
                                       [(__) #'(not (eq? '*no-next-method*
                                                         (object-name #,cnm)))]
                                       [(__ . xs)
                                        #'((named-lambda next-method? () 1)
                                           . xs)]
                                       [__
                                        #'(named-lambda next-method? ()
                                            (not
                                             (eq? '*no-next-method*
                                                  (object-name #,cnm))))]))])
                               . body))
                           . more))])
              (if name-e
                (quasisyntax/loc stx (letrec ([name #,makeit]) name))
                makeit)))]))]))

;;>> (method (arg ...) body ...)
;;>> (named-method name (arg ...) body ...)
;;>> (qualified-method qualifier (arg ...) body ...)
;;>   These forms are all similar variants to create a method object (and
;;>   instance of the `*default-method-class*' parameter).  A method looks
;;>   very similar to a lambda expression, except that the an argument can
;;>   be a of the form `[arg spec]' where `spec' is a specializer -- either
;;>   a class or a singleton specifier (the square brackets are equivalent
;;>   to round parens, just make the code more readable).  Also, an argument
;;>   can have the form of `[arg = val]' which is shorthand for specifying
;;>   `[arg (singleton val)]'.  In case of a simple argument, <top> is
;;>   always used as a specializer, but this processing stops as soon as a
;;>   &-keyword is encountered.  The `named-method' form is used to provide
;;>   an explicit name (which can be used to call itself recursively) , and
;;>   `qualified-method' is used to provide an explicit qualifier (which
;;>   should be one of the standard qualifiers (:primary, :around, :before,
;;>   or :after) when using the standard <method> and <generic> classes).
;;>
;;>   The resulting method can be added to a generic and these specializers
;;>   will be used when filtering applicable methods, or it can be used by
;;>   itself and the specializers will be used to check the arguments.  This
;;>   makes it easy to use `method' instead of `lambda' to get some type
;;>   information, but note that the result is going to run slower since the
;;>   type check only takes time but cannot be used by Racket to optimize
;;>   the code.
;;>
;;>   Note that the specializer argument are evaluated normally, which means
;;>   that anything can be used, even something like:
;;>     (let ([x (list <string> <integer>)])
;;>       (method ([x (2nd x)] [y = (+ 2 3)]) (+ x y)))
(defsubst* (method args body0 body ...)
  (make-method-specs/initargs #f args (body0 body ...)))
(defsubst* (named-method name args body0 body ...)
  (make-method-specs/initargs name args (body0 body ...)))
(defsubst* (qualified-method qualifier args body0 body ...)
  (make-method-specs/initargs #f args (body0 body ...) :qualifier qualifier))

;;>> (-defmethod-create-generics- [#t/#f])
;;>   This is a syntax parameter (see above) holding a boolean.  When this
;;>   is set to `#t' (the default), then the `defmethod' form below will try
;;>   to detect when the first definition happens and automatic add a
;;>   `defgeneric' form to define the object as a generic.  A safer but less
;;>   convenient approach would be to set this to `#f' and always do an
;;>   explicit `defgeneric'.
(define-syntax-parameter* -defmethod-create-generics- #t)

(defsyntax (method-def-adder stx)
  (syntax-case stx ()
    [(_ qualifier name args body ...) (identifier? #'name)
     ;; always make it with no name so add-method will add it
     (with-syntax ([method-make (syntax/loc stx
                                  (qualified-method qualifier args body ...))])
       (let ([ctx (syntax-local-context)])
         (cond
          [(or ; if:
            ;; not enabled
            (not (syntax-e ((syntax-local-value
                             #'-defmethod-create-generics-))))
            ;; expression position -- same as using add-method
            (eq? 'expression ctx)
            ;; defined symbol or second module binding
            (identifier-binding #'name)
            ;; already defined in this local context or top-level
            (let ([ctx (cond [(pair? ctx) (car ctx)]
                             [(eq? ctx 'top-level) ctx]
                             [else #f])])
              (and ctx ((syntax-local-value #'generic-contexts-defined?)
                        #'name ctx))))
           ;; then use add-method
           ;; (printf ">>> ~s: add\n" (syntax-e #'name))
           (syntax/loc stx (add-method name method-make))]
          ;; this might still be useful sometimes...
          ;; [(eq? 'top-level ctx)
          ;;  ;; if top-level then use a trick: try to use an
          ;;  (syntax/loc stx
          ;;    (define name ; trick: try using exising generic
          ;;      (let ([g (or (no-errors name) (generic name))])
          ;;        (add-method g method-make)
          ;;        g)))]
          [else
           ;; first module or function binding
           ;; (printf ">>> ~s: def\n" (syntax-e #'name))
           (syntax/loc stx (define name
                             (let ([g (generic name)])
                               (add-method g method-make)
                               g)))])))]))

;;>> (defmethod name [qualifier] (arg ...) body ...)
;;> | (defmethod [qualifier] (name arg ...) body ...)
;;>   This form is used to define a method object using `method' and its
;;>   variants above.  A qualifier (a :keyword) can be specified anywhere
;;>   before the argument list, and the name can be either specified before
;;>   the arguments (Lisp style) or with the arguments (Scheme style).
;;>   Depending on `-defmethod-create-generics-' (see above), this form
;;>   might add a `defgeneric' form to define the given `name' as a generic
;;>   object, and then add the created method.  The created method is
;;>   attached to the generic in any case, which makes the name of this form
;;>   a little misleading since it is not always defining a variable value.
;;>   In a local definition context, this should do the right thing as long
;;>   as `defmethod' or `defgeneric' is used to define the method (but note
;;>   that using a local generic function, is very inefficient) -- for
;;>   example, both of these work (defining a local generic):
;;>     (define (f)
;;>       (defgeneric foo)
;;>       (defmethod (foo [x <c1>]) 1)
;;>       (defmethod (foo [x <c2>]) 2)
;;>       3)
;;>     (define (f)
;;>       (defmethod (foo [x <c1>]) 1)
;;>       (defmethod (foo [x <c2>]) 2)
;;>       3)
;;>   but this fails because the first `defmethod' doesn't know that it is
;;>   already defined:
;;>     (define (f)
;;>       (define foo (generic foo))
;;>       (defmethod (foo [x c1]) 1)
;;>       (defmethod (foo [x c1]) 2)
;;>       3)
;;>   second "but" -- this:
;;>     (define (f)
;;>       (define foo (generic foo))
;;>       blah
;;>       (defmethod (foo [x <c1>]) 1)
;;>       (defmethod (foo [x <c2>]) 2)
;;>       3)
;;>   works because a `defmethod' in an expression context is always the
;;>   same as `add-method'.
(defsyntax* (defmethod stx)
  (define (n+a? stx)
    (let ([na (syntax-e stx)]) (and (pair? na) (identifier? (car na)))))
  (syntax-case stx ()
    [(_ name qualifier args body0 body ...)
     (and (identifier? #'name) (syntax-keyword? #'qualifier))
     (syntax/loc stx
       (method-def-adder qualifier name args body0 body ...))]
    [(_ qualifier name args body0 body ...)
     (and (identifier? #'name) (syntax-keyword? #'qualifier))
     (syntax/loc stx
       (method-def-adder qualifier name args body0 body ...))]
    [(_ qualifier name+args body0 body ...)
     (and (n+a? #'name+args) (syntax-keyword? #'qualifier))
     ;; simple pattern matching with (name . args) and using args won't work
     ;; since the destructing loses the arguments context and call-next-method
     ;; won't be accessible in the body.
     (with-syntax ([name (car (syntax-e #'name+args))]
                   [args (datum->syntax-object ; hack binding context!
                          #'name+args
                          (cdr (syntax-e #'name+args))
                          #'name+args)])
       (syntax/loc stx
         (method-def-adder qualifier name args body0 body ...)))]
    [(_ name+args body0 body ...) (n+a? #'name+args)
     ;; same as above
     (with-syntax ([name (car (syntax-e #'name+args))]
                   [args (datum->syntax-object ; hack binding context!
                          #'name+args
                          (cdr (syntax-e #'name+args))
                          #'name+args)])
       (syntax/loc stx
         (method-def-adder #f name args body0 body ...)))]
    [(_ name args body0 body ...) (identifier? #'name)
     (syntax/loc stx (method-def-adder #f name args body0 body ...))]))

;;>> (beforemethod ...)
;;>> (aftermethod ...)
;;>> (aroundmethod ...)
;;>> (defbeforemethod ...)
;;>> (defaftermethod ...)
;;>> (defaroundmethod ...)
;;>   These forms are shorthands that will generate a qualified method using
;;>   one of the standard qualifiers.
(defsubst* (beforemethod . more) (qualified-method :before . more))
(defsubst* (aftermethod  . more) (qualified-method :after  . more))
(defsubst* (aroundmethod . more) (qualified-method :around . more))
(defsubst* (defbeforemethod . more) (defmethod :before . more))
(defsubst* (defaftermethod  . more) (defmethod :after  . more))
(defsubst* (defaroundmethod . more) (defmethod :around . more))

;;; ---------------------------------------------------------------------------
;;; Class macros

;;>>... Class macros

(defsyntax (make-class-form stx)
  (define (slots/initargs s/a)
    (let loop ([xs s/a] [r '()])
      (syntax-case xs ()
        [() (values (datum->syntax-object #'s/a (reverse r) #'s/a)
                    #'())]
        [((name . args) . more) (identifier? #'name)
         (loop #'more (cons #'(list 'name . args) r))]
        [(key val . more) (syntax-keyword? #'key)
         (values (datum->syntax-object #'s/a (reverse r) #'s/a)
                 #'(key val . more))]
        [(name . more) (identifier? #'name)
         (loop #'more (cons #'(list 'name) r))])))
  (syntax-case stx ()
    [(_ metaclass cname supers . s/a)
     (let*-values ([(slots initargs) (slots/initargs #'s/a)]
                   [(meta) (syntax-getarg initargs :metaclass #'metaclass)])
       (with-syntax ([(arg ...) #`(#,@initargs
                                   :direct-supers (list . supers)
                                   :direct-slots (list #,@slots)
                                   :name '#,(if (syntax-e #'cname)
                                              #'cname (syntax-local-name)))])
         (if (identifier? #'cname)
           #`(rec-make (cname #,meta arg ...))
           #`(make #,meta arg ...))))]))

;;>> (class [name] (super ...) slot ... class-initarg ...)
;;>   Create a class object (an instance of the `*default-class-class*'
;;>   parameter).  An explicit name can optionally be specified explicitly.
;;>   The list of superclasses are evaluated normally, so they can be any
;;>   expression (as with the `method' forms).  Each slot can be either a
;;>   symbol, which will be used as the slot name, or a list that begins
;;>   with a symbol and continues with a keyword-argument option list.
;;>   Finally, more initargs for the class generation can be provided.  See
;;>   the `defclass' forms below for an explanation on the available slot
;;>   option and class initargs.  If a name is given, then `rec-make' is
;;>   used, see that for a description.
(defsyntax* (class stx)
  (syntax-case stx ()
    [(_ name supers slot ...) (identifier? #'name)
     #'(make-class-form (*default-class-class*) name supers slot ...)]
    [(_ supers slot ...)
     #'(make-class-form (*default-class-class*) #f supers slot ...)]))

;;>> (entityclass [name] (super) slot ... class-initarg ...)
;;>   Same as the `class' form, but creates an entity class object (an
;;>   instance of the `*default-entityclass-class*' parameter).
(defsyntax* (entityclass stx)
  (syntax-case stx ()
    [(_ name supers slot ...) (identifier? #'name)
     #'(make-class-form (*default-entityclass-class*) name supers slot ...)]
    [(_ supers slot ...)
     #'(make-class-form (*default-entityclass-class*) #f supers slot ...)]))

;;>> (-defclass-auto-initargs- [#f/initargs])
;;>   This is a syntax parameter (see above) holding either `#f' or an
;;>   initargs list .  If it is not `#f', `defclass' below will add its
;;>   contents to the end of the given initargs (so user supplied arguments
;;>   can override them).  The default is `#f'.
(define-syntax-parameter* -defclass-auto-initargs- #f)

;;>> (-defclass-autoaccessors-naming- [naming-keyword])
;;>   This syntax parameter holds a keyword symbol that is used in the
;;>   `defclass' for the `:autoaccessors' if it is specified as `#t' or if
;;>   it used due to `:auto'.  See the description of the `:autoaccessors'
;;>   option below for possible values.  The default is `:class-slot'.
(define-syntax-parameter* -defclass-autoaccessors-naming- :class-slot)

;;>> (-defclass-accessor-mode- [mode-keyword])
;;>   This syntax parameter holds a keyword symbol that is used in the
;;>   `defclass' for the way accessors, readers, and writers are generated.
;;>   It can be `:defmethod' for using `defmethod', `:defgeneric' for using
;;>   `defgeneric' and then `add-method', `:add-method' for using
;;>   `add-method', `:method' for defining an independent method, or
;;>   `:procedure' for defining a simple Scheme procedure.  The default is
;;>   `:defmethod.  This default is usually fine, but a situation where this
;;>   is important is if the syntax parameter `-defmethod-create-generics-'
;;>   is set to `#f' so a `defmethod' requires a prior `defgeneric' so a
;;>   defclass will not work unless the generic functions are defined in
;;>   advance.
(define-syntax-parameter* -defclass-accessor-mode- :defmethod)

;;>> (defclass name (super ...) slot ... class-initarg ...)
;;>   This form uses the `class' form above to define a new class.  See the
;;>   `class' form for the syntax.  Note that slot-options that are not
;;>   compile-time ones (method names) are accumulated according to the
;;>   class precedence list.
;;>
;;>   Available slot options are:
;;>   * :initarg keyword
;;>     Use `keyword' in `make' to provide a value for this slot.
;;>   * :initializer func
;;>     Use the given function to initialize the slot -- either a thunk or a
;;>     function that will be applied on the initargs given to `make'.
;;>   * :initvalue value
;;>     Use `value' as the default for this slot.
;;>   * :reader name
;;>     Define `name' (an unquoted symbol) as a reader method for this slot.
;;>   * :writer name
;;>     Define `name' (an unquoted symbol) as a writer method for this slot.
;;>   * :accessor name
;;>     Define `name' (an unquoted symbol) as an accessor method for this
;;>     slot -- this means that two methods are defined: `name' and
;;>     `set-name!'.
;;>   * :type type
;;>     Restrict this slot value to objects of the given `type'.
;;>   * :lock { #t | #f | value }
;;>     If specified and non-`#f', then this slot is locked.  `#t' locks it
;;>     permanently, but a different value works as a key: they allow setting
;;>     the slot by using cons of the key and the value to set.
;;>   * :allocation { :class | :instance }
;;>     Specify that this slot is a normal one (`:instance', the default),
;;>     or allocated per class (`:class').
;;>   The specific way of creating helper methods (for readers, writers, and
;;>   accessors) is determined by `-defclass-accessor-mode-' (see above).
;;>
;;>   Available class options (in addition to normal ones that initialize
;;>   the class slots like `:name', `:direct-slots', `:direct-supers') are:
;;>   * :metaclass class
;;>     create a class object which is an instance of the `class'
;;>     meta-class (this means that an instance of the given meta-class
;;>     should be used for creating the new class).
;;>   * :autoinitargs { #t | #f }
;;>     if set to `#t', make the class definition automatically generate
;;>     initarg keywords from the slot names.  (The keywords have the same
;;>     name as the slots, eg `:foo'.)
;;>   * :autoaccessors { #f | #t | :class-slot | :slot }
;;>     if set to non-`#f', generate accessor methods automatically --
;;>     either using the classname "-" slotname convention (`:class-slot')
;;>     or just the slotname (`:slot').  If it is `#t' (or turned on by
;;>     `:auto') then the default naming style is taken from the
;;>     `-defclass-autoaccessors-naming-' syntax parameter.  Note that for
;;>     this, and other external object definitions (`:automaker' and
;;>     `:autopred'), the class name is stripped of a surrounding "<>"s if
;;>     any.
;;>   * :automaker { #f | #t }
;;>     automatically creates a `maker' function using the "make-" classname
;;>     naming convention.  The maker function is applied on arguments and
;;>     keyword-values -- if there are n slots, then arguments after the
;;>     first n are passed to `make' to create the instance, then the first
;;>     n are `slot-set!'ed into the n slots.  This means that it can get
;;>     any number of arguments, and usually there is no point in additional
;;>     keyword values (since if they initialize slots, their values will
;;>     get overridden anyway).  It also means that the order of the
;;>     arguments depend on the *complete* list of the class's slots (as
;;>     given by `class-slots'), so use caution when doing multiple
;;>     inheritance (actually, in that case it is probably better to avoid
;;>     these makers anyway).
;;>   * :autopred { #f | #t }
;;>     automatically create a predicate function using the `classname "?"'
;;>     naming convention.
;;>   * :default-slot-options { #f | '(keyword ...) }
;;>     if specified as a quoted list, then slot descriptions are modified
;;>     so the first arguments are taken as values to the specified
;;>     keywords.  For example, if it is `'(:type :initvalue)' then a slot
;;>     description can have a single argument for `:type' after the slot
;;>     name, a second argument for `:initvalue', and the rest can be more
;;>     standard keyword-values.  This is best set with
;;>     `-defclass-auto-initargs-'
;;>   * :auto { #f | #t }
;;>     if specified as `#t', then all automatic behavior available above is
;;>     turned on.
;; The following option is added in extra.rkt
;;>   * :printer { #f | #t | procedure }
;;>     if given, install a printer function.  `#t' means install the
;;>     `print-object-with-slots' function from "clos.rkt", otherwise, it is
;;>     expected to be a function that gets an object, an escape boolean
;;>     flag an an optional port (i.e, 2 or more arguments), and prints the
;;>     object on the class using the escape flag to select `display'-style
;;>     (`#f') or `write'-style (#t).
;;>
;;>   Note that the class object is made by `class' with a name, so it is
;;>   possible to use the class itself as the value of `:type' properties
;;>   for a recursive class.
;;>
;;>   Whenever the classname is used, it is taken from the defined name,
;;>   without a surrounding "<>"s if any.  Note that some of these options
;;>   are processed at compile time (all method names and auto-generation of
;;>   methods).
(defsyntax (make-defclass-form stx)
  (syntax-case stx ()
    [(_ class-maker name supers . slots0)
     (identifier? #'name)
     (let loop ([slots1 #'slots0] [slots2 '()])
       (syntax-case slots1 ()
         [(slot more ...) (not (syntax-keyword? #'slot))
          (loop #'(more ...) (cons #'slot slots2))]
         [(initarg ...) ; if slots1 is not null then it contains class keywords
          (let* ([autoargs (let ([as ((syntax-local-value
                                       #'-defclass-auto-initargs-))])
                             (and (syntax? as) (syntax-e as) as))]
                 [initargs (if autoargs
                             #`(initarg ... #,@autoargs) #'(initarg ...))]
                 [defmethods '()]
                 [sgetarg (lambda (arg . def)
                            (let ([a (apply syntax-getarg initargs arg def)])
                              (if (syntax? a) (syntax-object->datum a) a)))]
                 [all-auto (sgetarg :auto)]
                 [autoaccessors (sgetarg :autoaccessors (and all-auto #t))]
                 [automaker (or (sgetarg :automaker) all-auto)]
                 [autopred (or (sgetarg :autopred) all-auto)]
                 [accessor-mode (syntax-e ((syntax-local-value
                                            #'-defclass-accessor-mode-)))]
                 [default-slot-options (sgetarg :default-slot-options)]
                 [string-name
                  (regexp-replace
                   #rx"^<(.*)>$" (symbol->string (syntax-e #'name)) "\\1")])
            (define (get-defaccessor-form a-name typed-args untyped-args body)
              (case accessor-mode
                [(:defmethod)
                 #`(defmethod (#,a-name #,@typed-args) #,body)]
                [(:defgeneric)
                 #`(begin (defgeneric (#,a-name #,@untyped-args))
                          (add-method #,a-name (method #,typed-args #,body)))]
                [(:add-method)
                 #`(add-method #,a-name (method #,typed-args #,body))]
                [(:method) #`(define #,a-name (method #,typed-args #,body))]
                [(:procedure) #`(define (#,a-name #,@untyped-args) #,body)]
                [else (error
                       'defclass
                       "bad value in -defclass-accessor-mode-: ~e"
                       accessor-mode)]))
            (define (addreader reader sname)
              (push! (get-defaccessor-form
                      reader #'((x name)) #'(x) #`(slot-ref x '#,sname))
                     defmethods))
            (define (addwriter writer sname type)
              (push! (get-defaccessor-form
                      writer #`((x name) #,(if type #`(n #,type) #'n)) #'(x n)
                      #`(slot-set! x '#,sname n))
                     defmethods))
            (define (do-slot slot)
              (define-values (sname args)
                (syntax-case slot ()
                  [(sname args ...)
                   (values
                    #'sname
                    (cond
                     [(not default-slot-options) #'(args ...)]
                     [(and (list? default-slot-options)
                           (= 2 (length default-slot-options))
                           (memq (car default-slot-options)
                                 '(quote quasiquote)))
                      (let loop ([d  (cadr default-slot-options)]
                                 [as #'(args ...)]
                                 [r  '()])
                        (syntax-case as ()
                          [(v rest ...) (pair? d)
                           (loop (cdr d)
                                 #'(rest ...)
                                 (list* #'v (car d) r))]
                          [_ (datum->syntax-object #'(args ...)
                                                   (append (reverse r) as)
                                                   #'(args ...))]))]
                     [else (raise-syntax-error
                            #f "bad form for :default-slot-options"
                            stx initargs)]))]
                  [sname (values #'sname #'())]))
              (let ([reader (syntax-getarg args :reader)]
                    [writer (syntax-getarg args :writer)]
                    [accessor
                     (syntax-getarg
                      args :accessor
                      (and autoaccessors
                           (thunk
                             (if (eq? autoaccessors :slot)
                               sname
                               (datum->syntax-object
                                sname
                                (string->symbol
                                 (concat string-name "-"
                                         (symbol->string (syntax-e sname))))
                                sname)))))]
                    [type (syntax-getarg args :type)])
                (when reader (addreader reader sname))
                (when writer (addwriter writer sname type))
                (when accessor
                  (addreader accessor sname)
                  (addwriter
                   (datum->syntax-object
                    accessor
                    (string->symbol
                     (concat "set-" (symbol->string (syntax-e accessor)) "!"))
                    accessor)
                   sname type))
                (let loop ([as args] [res (list sname)])
                  (syntax-case as ()
                    [(keyword value more ...)
                     (loop #'(more ...)
                           (list* (if (memq (syntax-e #'keyword)
                                            '(:reader :writer :accessor))
                                    #''value #'value)
                                  #'keyword res))]
                    [() (datum->syntax-object as (reverse res) as)]))))
            (when (eq? autoaccessors #t)
              (set! autoaccessors
                    (syntax-e ((syntax-local-value
                                #'-defclass-autoaccessors-naming-)))))
            (unless (memq autoaccessors '(#t #f :slot :class-slot))
              (raise-syntax-error
               #f (concat "`:autoaccessors' expecting either a "
                          "`:slot' or `:class-slot' as value.")
               stx initargs))
            (let ([slots (map do-slot (reverse slots2))])
              #`(begin
                  (define name
                    (class-maker name supers
                                 . #,(datum->syntax-object
                                      #'slots0
                                      ;; note: append with a non-list 2nd arg
                                      (append
                                       slots (if all-auto
                                               #`(:autoinitargs #t #,@initargs)
                                               initargs))
                                      #'slots0)))
                  #,@(datum->syntax-object
                      #'stx (reverse defmethods) #'stx)
                  #,@(if automaker
                       (with-syntax
                           ([maker (datum->syntax-object
                                    #'name
                                    (string->symbol
                                     (concat "make-" string-name))
                                    #'name)])
                         #'((define maker
                              (let ([slots (class-slots name)])
                                (lambda args
                                  (let loop ([as args] [ss slots] [r '()])
                                    (if (or (null? as) (null? ss))
                                      (let ([new (make name . as)])
                                        (for-each (lambda (x)
                                                    (slot-set! new . x))
                                                  r)
                                        new)
                                      (loop (cdr as) (cdr ss)
                                            (cons (list (caar ss) (car as))
                                                  r)))))))))
                       '())
                  #,@(if autopred
                       (with-syntax
                           ([pred? (datum->syntax-object
                                    #'name
                                    (string->symbol (concat string-name "?"))
                                    #'name)])
                         #'((define (pred? x) (instance-of? x name))))
                       '()))))]))]))

(defsubst* (defclass name supers slot ...)
  (make-defclass-form class name supers slot ...))

;;>> (defentityclass name (super ...) slot ... class-initarg ...)
;;>   The same as `defclass', but for entity classes.
(defsubst* (defentityclass name supers slot ...)
  (make-defclass-form entityclass name supers slot ...))

;;; ---------------------------------------------------------------------------
;;; Forms with a provide version

;;>>...
;;> *** Auto provide forms

;;>> (defgeneric* ...)
;;>> (defclass* ...)
;;>> (defentityclass* ...)
;;>   These forms are defined as the original version, except that the
;;>   defined variable is automatically provided (made using
;;>   `make-provide-syntax' above).  Note that there is no version for
;;>   `defmethod' since it should not be used where a single definition
;;>   place is needed -- and it wouldn't make sense to have multiple
;;>   `provide' forms for every `defmethod*' occurrence.  Note that
;;>   `defclass*' provides only the class identifier and not any
;;>   automatically generated ones (accessors etc).
(provide defgeneric*)     (make-provide-syntax defgeneric     defgeneric*)
(provide defclass*)       (make-provide-syntax defclass       defclass*)
(provide defentityclass*) (make-provide-syntax defentityclass defentityclass*)
