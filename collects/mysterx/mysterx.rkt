;; mysterx.rkt

(module mysterx mzscheme

  ; private mysterx modules

  (require (prefix mxprims: "private/mxmain.rkt")
           (prefix style: "private/style.rkt")
           "private/filter.rkt"
           "private/properties.rkt"
           "private/util.rkt")

  ; mzlib

  (require (prefix mzlib: mzlib/list))
  (require mzlib/string)
  (require mzlib/class)
  (require net/url)
  (require mzlib/etc)
  (require mzlib/thread)

  ; exports

  (provide
    mx-browser%
    mx-element%
    mx-document<%>
    mx-event<%>
    mx-version
    block-while-browsers
    com-invoke
    com-get-property
    com-set-property!
    com-methods
    com-get-properties
    com-set-properties
    com-events
    com-method-type
    com-get-property-type
    com-set-property-type
    com-event-type
    com-object-type
    com-add-ref
    com-ref-count
    com-is-a?
    com-help
    com-register-event-handler
    com-unregister-event-handler
    com-all-coclasses
    com-all-controls
    coclass->html
    progid->html
    cocreate-instance-from-coclass
    cci/coclass
    cocreate-instance-from-progid
    cci/progid
    com-get-active-object-from-coclass
    gao/coclass
    coclass
    progid
    set-coclass!
    set-coclass-from-progid!
    com-object-eq?
    com-omit
    make-css-percentage
    css-percentage?
    css-percentage-num
    make-css-length
    css-length?
    css-length-num
    css-length-units
    com-date->date
    date->com-date
    com-date?
    com-currency?
    com-currency->number
    number->com-currency
    com-scode?
    com-scode->number
    number->com-scode
    com-object?
    com-iunknown?)

  (define mx-version mxprims:mx-version)
  (define block-while-browsers mxprims:block-while-browsers)
  (define com-invoke mxprims:com-invoke)
  (define com-method-type mxprims:com-method-type)
  (define com-get-property-type mxprims:com-get-property-type)
  (define com-set-property-type mxprims:com-set-property-type)
  (define com-event-type mxprims:com-event-type)
  (define com-object-type mxprims:com-object-type)
  (define com-add-ref mxprims:com-add-ref)
  (define com-ref-count mxprims:com-ref-count)
  (define com-is-a? mxprims:com-is-a?)
  (define com-currency? mxprims:com-currency?)
  (define number->com-currency mxprims:number->com-currency)
  (define com-currency->number mxprims:com-currency->number)
  (define com-date? mxprims:com-date?)
  (define com-date->date mxprims:com-date->date)
  (define date->com-date mxprims:date->com-date)
  (define com-scode? mxprims:com-scode?)
  (define number->com-scode mxprims:number->com-scode)
  (define com-scode->number mxprims:com-scode->number)
  (define com-object? mxprims:com-object?)
  (define com-iunknown? mxprims:com-iunknown?)
  (define com-help mxprims:com-help)
  (define com-register-event-handler mxprims:com-register-event-handler)
  (define com-unregister-event-handler mxprims:com-unregister-event-handler)
  (define coclass->html mxprims:coclass->html)
  (define progid->html mxprims:progid->html)
  (define cocreate-instance-from-coclass mxprims:cocreate-instance-from-coclass)
  (define cci/coclass cocreate-instance-from-coclass)
  (define cocreate-instance-from-progid mxprims:cocreate-instance-from-progid)
  (define cci/progid cocreate-instance-from-progid)
  (define com-get-active-object-from-coclass mxprims:com-get-active-object-from-coclass)
  (define gao/coclass com-get-active-object-from-coclass)
  (define coclass mxprims:coclass)
  (define progid mxprims:progid)
  (define set-coclass! mxprims:set-coclass!)
  (define set-coclass-from-progid! mxprims:set-coclass-from-progid!)
  (define com-object-eq? mxprims:com-object-eq?)
  (define com-omit mxprims:com-omit)

  ;; sort results of "reflection" results

  (define (alphabetize lst)
    (mzlib:sort lst string-ci<?))

  (define (make-sorted-fun f)
    (lambda (obj)
      (alphabetize (f obj))))

  (define com-methods (make-sorted-fun mxprims:com-methods))
  (define com-get-properties (make-sorted-fun mxprims:com-get-properties))
  (define com-set-properties (make-sorted-fun mxprims:com-set-properties))
  (define com-events (make-sorted-fun mxprims:com-events))

  (define (make-sorted-thunk f)
    (lambda ()
      (alphabetize (f))))

  (define com-all-coclasses
    (make-sorted-thunk mxprims:com-all-coclasses))
  (define com-all-controls
    (make-sorted-thunk mxprims:com-all-controls))

  ;; property getter/setter

  (define (get-item-property obj item)
    (cond
     [(and (pair? item)
	   (string? (car item)))
      (apply mxprims:com-get-property obj item)]
     [(string? item)
      (mxprims:com-get-property obj item)]
     [else
      (error "For COM property, expected a string or a list with a string a the first element")]))

  (define (com-get-property obj . path)
    (cond
     [(null? path)
      (error 'com-get-property
	     "Expected one or more properties (strings or lists with a string as the first element)")]
     [(null? (cdr path))
      (get-item-property obj (car path))]
     [else (apply com-get-property
		  (get-item-property obj (car path))
		  (cdr path))]))

  (define (com-set-property! obj . path-and-value)
    (cond
     [(or (null? path-and-value)
	  (null? (cdr path-and-value)))
      (error 'com-set-property!
	     "Expected one or more properties (strings or lists with a string as the first element) and a value")]
     [(null? (cddr path-and-value)) ; (property value)
      (let ([ppty (car path-and-value)]
	    [val (cadr path-and-value)])
	(if (pair? ppty)
	    (if (string? (car ppty))
		(apply mxprims:com-set-property! obj
		       (append ppty (list val)))
		(error 'com-set-property!
		       "Indexed property must be a list with a string (property name) as the first element"))
	    (mxprims:com-set-property! obj ppty val)))]
     [else (apply com-set-property!
		  (get-item-property obj (car path-and-value))
		  (cdr path-and-value))]))

  ;; style-related procedures

  (define make-css-percentage style:make-css-percentage)
  (define css-percentage? style:css-percentage?)
  (define css-percentage-num style:css-percentage-num)
  (define make-css-length style:make-css-length)
  (define css-length? style:css-length?)
  (define css-length-num style:css-length-num)
  (define css-length-units style:css-length-units)

  (define html-sem (make-semaphore 1))   ; protects HTML insertions
  (define html-wait (lambda () (semaphore-wait html-sem)))
  (define html-post (lambda () (semaphore-post html-sem)))

  (define mx-element%
    (class object% (init document dhtml-element)
       (public
	insert-html
	append-html
	replace-html
	get-html
	get-text
	insert-text
	append-text
	insert-object-from-coclass
	append-object-from-coclass
	insert-object-from-progid
	append-object-from-progid
	focus
	selection
	set-selection!
	attribute
	set-attribute!
	click
	tag
	font-family
	font-family-native
	set-font-family!
	set-font-family-native!
	font-style
	font-style-native
	set-font-style!
	set-font-style-native!
	font-variant
	font-variant-native
	set-font-variant!
	set-font-variant-native!
	font-weight
	font-weight-native
	set-font-weight!
	set-font-weight-native!
	font-native
	set-font-native!
	background-native
	set-background-native!
	background-attachment
	background-attachment-native
	set-background-attachment!
	set-background-attachment-native!
	background-image
	background-image-native
	set-background-image!
	set-background-image-native!
	background-repeat
	background-repeat-native
	set-background-repeat!
	set-background-repeat-native!
	background-position
	background-position-native
	set-background-position!
	set-background-position-native!
	text-decoration
	text-decoration-native
	set-text-decoration!
	set-text-decoration-native!
	text-transform
	text-transform-native
	set-text-transform!
	set-text-transform-native!
	text-align
	text-align-native
	set-text-align!
	set-text-align-native!
	margin
	margin-native
	set-margin!
	set-margin-native!
	padding
	padding-native
	set-padding!
	set-padding-native!
	border
	border-native
	set-border!
	set-border-native!
	border-top-native
	set-border-top!
	set-border-top-native!
	border-bottom-native
	set-border-bottom!
	set-border-bottom-native!
	border-left-native
	set-border-left!
	set-border-left-native!
	border-right-native
	border-top
	border-bottom
	border-left
	border-right
	set-border-right!
	set-border-right-native!
	border-color
	border-color-native
	set-border-color!
	set-border-color-native!
	border-width
	border-width-native
	set-border-width!
	set-border-width-native!
	border-style
	border-style-native
	set-border-style!
	set-border-style-native!
	border-top-style
	border-top-style-native
	set-border-top-style!
	set-border-top-style-native!
	border-bottom-style
	border-bottom-style-native
	set-border-bottom-style!
	set-border-bottom-style-native!
	border-left-style
	border-left-style-native
	set-border-left-style!
	set-border-left-style-native!
	border-right-style
	border-right-style-native
	set-border-right-style!
	set-border-right-style-native!
	border-top-color
	border-top-color-native!
	set-border-top-color!
	set-border-top-color-native!
	border-bottom-color
	border-bottom-color-native
	set-border-bottom-color!
	set-border-bottom-color-native!
	border-left-color
	border-left-color-native
	set-border-left-color!
	set-border-left-color-native!
	border-right-color
	border-right-color-native
	set-border-right-color!
	set-border-right-color-native!
	border-top-width
	border-top-width-native
	set-border-top-width!
	set-border-top-width-native!
	border-bottom-width
	border-bottom-width-native
	set-border-bottom-width!
	set-border-bottom-width-native!
	border-left-width
	border-left-width-native
	set-border-left-width!
	set-border-left-width-native!
	border-right-width
	border-right-width-native
	set-border-right-width!
	set-border-right-width-native!
	style-float
	style-float-native
	set-style-float!
	set-style-float-native!
	clear
	clear-native
	set-clear!
	set-clear-native!
	display
	display-native
	set-display!
	set-display-native!
	visibility
	visibility-native
	set-visibility!
	set-visibility-native!
	list-style-type
	list-style-type-native
	set-list-style-type!
	set-list-style-type-native!
	list-style-position
	list-style-position-native
	set-list-style-position!
	set-list-style-position-native!
	list-style-image
	list-style-image-native
	set-list-style-image!
	set-list-style-image-native!
	list-style
	list-style-native
	set-list-style!
	set-list-style-native!
	position
	position-native
	overflow
	overflow-native
	set-overflow!
	set-overflow-native!
	pagebreak-before
	pagebreak-before-native
	set-pagebreak-before!
	set-pagebreak-before-native!
	pagebreak-after
	pagebreak-after-native
	set-pagebreak-after!
	css-text-native
	set-css-text-native!
	cursor
	cursor-native
	set-cursor!
	set-cursor-native!
	clip
	clip-native
	set-clip!
	set-clip-native!
	filter
	filter-native
	set-filter!
	set-filter-native!
	style-string
	text-decoration-none
	set-text-decoration-none!
	text-decoration-underline
	set-text-decoration-underline!
	text-decoration-overline
	set-text-decoration-overline!
	text-decoration-linethrough
	set-text-decoration-linethrough!
	text-decoration-blink
	set-text-decoration-blink!
	pixel-top
	set-pixel-top!
	pixel-left
	set-pixel-left!
	pixel-width
	set-pixel-width!
	pixel-height
	set-pixel-height!
	pos-top
	set-pos-top!
	pos-left
	set-pos-left!
	pos-width
	set-pos-width!
	pos-height
	set-pos-height!
	font-size
	font-size-native
	set-font-size!
	set-font-size-native!
	color
	color-native
	set-color!
	set-color-native!
	background-color
	background-color-native
	set-background-color!
	set-background-color-native!
	background-position-x
	background-position-x-native
	set-background-position-x!
	set-background-position-x-native!
	background-position-y
	background-position-y-native
	set-background-position-y!
	set-background-position-y-native!
	letter-spacing
	letter-spacing-native
	set-letter-spacing!
	set-letter-spacing-native!
	vertical-align
	vertical-align-native
	set-vertical-align!
	set-vertical-align-native!
	text-indent
	text-indent-native
	set-text-indent!
	set-text-indent-native!
	line-height
	line-height-native
	set-line-height!
	set-line-height-native!
	margin-top
	margin-top-native
	set-margin-top!
	set-margin-top-native!
	margin-bottom
	margin-bottom-native
	set-margin-bottom!
	set-margin-bottom-native!
	margin-left
	margin-left-native
	set-margin-left!
	set-margin-left-native!
	margin-right
	margin-right-native
	set-margin-right!
	set-margin-right-native!
	padding-top
	padding-top-native
	set-padding-top!
	set-padding-top-native!
	padding-bottom
	padding-bottom-native
	set-padding-bottom!
	set-padding-bottom-native!
	padding-left
	padding-left-native
	set-padding-left!
	set-padding-left-native!
	padding-right
	padding-right-native
	set-padding-right!
	set-padding-right-native!
	width
	width-native
	set-width!
	set-width-native!
	height
	height-native
	set-height!
	set-height-native!
	top
	top-native
	set-top!
	set-top-native!
	left
	left-native
	set-left!
	set-left-native!
	z-index
	z-index-native
	set-z-index!
	set-z-index-native!)

       ; private fields
       (define elt dhtml-element)

       (define doc document)

       (define get-string-as-symbol
	(lambda (f name)
	  (let ([s (f elt)])
	    (if (empty-string? s)
		(empty-property-error name)
		(string->symbol s)))))

       (define set-symbol-as-string
	 (lambda (sym vals f name)
	   (unless (member sym vals)
		   (error
		    (format "~a: Expected value in '~a, got ~a"
			    name vals sym)))
	   (f elt (symbol->string sym))))

       (define html-insertion-maker
	 (lambda (f)
	   (lambda (s)
	     (dynamic-wind
		 html-wait
		 (lambda () (f elt s))
		 html-post))))

       (define insert-object-maker
	 (lambda (name->html)
	   (opt-lambda
	    (object width height [size 'pixels])
	    (dynamic-wind
		html-wait
		(lambda ()
		  (let ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-insert-html
		     elt
		     (name->html object width height size))
		    (let* ([new-objects (mxprims:document-objects doc)]
			   [obj (car (mzlib:remove* old-objects new-objects
						    com-object-eq?))])
		      (mxprims:com-register-object obj)
		      obj)))
		html-post))))

       (define append-object-maker
	 (lambda (name->html)
	   (opt-lambda
	    (object width height [size 'pixels])
	    (dynamic-wind
		html-wait
		(lambda ()
		  (let* ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-append-html
		     elt
		     (name->html object width height size))
		    (let* ([new-objects (mxprims:document-objects doc)]
			   [obj (car (mzlib:remove* old-objects
						    new-objects
						    com-object-eq?))])
		      (mxprims:com-register-object obj)
		      obj)))
		html-post))))

       (define insert-object-from-coclass-raw
	(insert-object-maker coclass->html))
       (define append-object-from-coclass-raw
	(append-object-maker coclass->html))
       (define insert-object-from-progid-raw
	(insert-object-maker progid->html))
       (define append-object-from-progid-raw
	(append-object-maker progid->html))
       (define insert-html
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (mxprims:element-insert-html elt s))
	       html-post)))
       (define get-html
	 (lambda () (mxprims:element-get-html elt)))
       (define get-text
	 (lambda () (mxprims:element-get-text elt)))
       (define insert-text
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (mxprims:element-insert-text elt s))
	       html-post)))
       (define append-text
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (mxprims:element-append-text elt s))
	       html-post)))
       (define append-html
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (mxprims:element-append-html elt s))
	       html-post)))
       (define replace-html
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (mxprims:element-replace-html elt s))
	       html-post)))
       (define insert-object-from-coclass
	 (lambda args
	   (apply insert-object-from-coclass-raw args)))
       (define append-object-from-coclass
	 (lambda args
	   (apply append-object-from-coclass-raw args)))
       (define insert-object-from-progid
	 (lambda args
	   (apply insert-object-from-progid-raw args)))
       (define append-object-from-progid
	 (lambda args
	   (apply append-object-from-progid-raw args)))
       (define focus
	 (lambda ()
	   (mxprims:element-focus elt)))
       (define selection
	 (lambda ()
	   (mxprims:element-selection elt)))
       (define set-selection!
	 (lambda (val)
	   (mxprims:element-set-selection! elt val)))
       (define attribute
	 (lambda (s)
	   (mxprims:element-attribute elt s)))
       (define set-attribute!
	 (lambda (a v)
	   (mxprims:element-set-attribute! elt a v)))
       (define click
	 (lambda ()
	   (mxprims:element-click elt)))
       (define tag
	 (lambda ()
	   (mxprims:element-tag elt)))
       (define font-family
	 (lambda ()
	   (let ([s (mxprims:element-font-family elt)])
	     (if (empty-string? s)
		 (empty-property-error "font-family")
		 (style:string->font-families s)))))
       (define font-family-native
	 (lambda ()
	   (mxprims:element-font-family elt)))
       (define set-font-family!
	 (lambda (ff)
	   (unless (and (pair? ff)
			(andmap string? ff))
		   (error "set-font-family!: Expected list of strings, got"
			  ff))
	   (mxprims:element-set-font-family!
	    elt
	    (style:font-families->string ff))))
       (define set-font-family-native!
	 (lambda (s)
	   (mxprims:element-set-font-family! elt s)))
       (define font-style
	 (lambda ()
	   (get-string-as-symbol
	    mxprims:element-font-style "font-style")))
	(define font-style-native
	 (lambda ()
	   (mxprims:element-font-style elt)))
	(define set-font-style!
	 (lambda (sym)
	   (set-symbol-as-string sym *font-styles*
				 mxprims:element-set-font-style!
				 "set-font-style!")))
	(define set-font-style-native!
	 (lambda (s)
	   (mxprims:element-set-font-style! elt s)))
	(define font-variant
	 (lambda ()
	   (get-string-as-symbol mxprims:element-font-variant
				 "font-variant")))
	(define font-variant-native
	 (lambda ()
	   (mxprims:element-font-variant elt)))
	(define set-font-variant!
	 (lambda (sym)
	   (set-symbol-as-string
	    sym *font-variants* mxprims:element-set-font-variant!
	    "set-font-variant!")))
	(define set-font-variant-native!
	 (lambda (s)
	   (mxprims:element-set-font-variant! elt s)))
	(define font-weight
	 (lambda ()
	   (let ([s (mxprims:element-font-weight elt)])
	     (if (empty-string? s)
		 (empty-property-error "font-weight")
		 (let ((c (string-ref s 0)))
		   (if (char-numeric? c)
		       (string->number s)
		       (string->symbol s)))))))
	(define font-weight-native
	 (lambda ()
	   (mxprims:element-font-weight elt)))
	(define set-font-weight!
	 (lambda (w)
	   (unless (member w
			   '(bold bolder lighter normal
				  100 200 300 400 500 600 700 800 900))
		   (error
		    (string-append
		     "Expected value in "
		     "'(bold bolder lighter normal "
		     "100 200 300 400 500 600 700 800 900),"
		     "got ~a")
		    w))
	   (let ((s (if (number? w)
			(number->string w)
			(symbol->string w))))
	     (mxprims:element-set-font-weight! elt s))))


	(define set-font-weight-native!
	 (lambda (s)
	   (mxprims:element-set-font-weight! elt s)))
	(define font-native
	 (lambda ()
	   (mxprims:element-font elt)))
	(define set-font-native!
	 (lambda (s)
	   (mxprims:element-set-font! elt s)))
	(define background-native
	 (lambda ()
	   (mxprims:element-background elt)))
	(define set-background-native!
	 (lambda (s)
	   (mxprims:element-set-background! elt s)))
	(define background-attachment
	 (lambda ()
	   (get-string-as-symbol mxprims:element-background-attachment
				 "background-attachment")))
	(define background-attachment-native
	 (lambda ()
	   (mxprims:element-background-attachment elt)))
	(define set-background-attachment!
	 (lambda (sym)
	   (set-symbol-as-string
	    sym *background-attachments*
	    mxprims:element-set-background-attachment!
	    "set-background-attachment!")))
	(define set-background-attachment-native!
	 (lambda (s)
	   (mxprims:element-set-background-attachment! elt s)))
	(define background-image
	 (lambda ()
	   (let ([s (mxprims:element-background-image elt)])
	     (cond
	      [(empty-string? s)
	       (empty-property-error "background-image")]
	      [(string=? s "none") 'none]
	      [(string-ci=? (substring s 0 3) "url")
	       (list->string
		(mzlib:filter (lambda (c)
				(not (member c '(#\( #\)))))
			      (string->list
			       (substring s 3 (string-length s)))))]
	      [else (error "Unknown background-image value: ~a"
			   s)]))))
	(define background-image-native
	 (lambda ()
	   (mxprims:element-background-image elt)))
	(define set-background-image!
	 (lambda (image)
	   (cond
	    [(eq? image 'none)
	     (mxprims:element-set-background-image! elt "none")]
	    [(string? image)
	     (mxprims:element-set-background-image!
	      elt
	      (string-append "url(" image ")"))]
	    [else
	     (error "Expected 'none or string, got: ~a" image)])))
	(define set-background-image-native!
	 (lambda (s)
	   (mxprims:element-set-background-image! elt s)))
	(define background-repeat
	 (lambda ()
	   (get-string-as-symbol mxprims:element-background-repeat
				 "background-repeat")))
	(define background-repeat-native
	 (lambda ()
	   (mxprims:element-background-repeat elt)))
	(define set-background-repeat!
	 (lambda (sym)
	   (set-symbol-as-string
	    sym *background-repeats*
	    mxprims:element-set-background-repeat!
	    "set-background-repeat!")))
	(define set-background-repeat-native!
	 (lambda (s)
	   (mxprims:element-set-background-repeat! elt s)))
	(define background-position
	 (lambda ()
	   (let ([s (mxprims:element-background-position elt)])
	     (if (empty-string? s)
		 (empty-property-error "background-position")
		 (style:string->background-position s)))))
	(define background-position-native
	 (lambda ()
	   (mxprims:element-background-position elt)))
	(define set-background-position!
	 (lambda (pos)
	   (cond
	    [(and (pair? pos) (= (length pos) 2))
	     (if (andmap symbol? pos)
		 (let ([elt-1 (car pos)]
		       [elt-2 (cadr pos)])
		   (if (or (and (horizontal? elt-1)
				(vertical? elt-2))
			   (and (vertical? elt-1)
				(horizontal? elt-2)))
		       (mxprims:element-set-background-position!
			elt
			(string-append (symbol->string elt-1)
				       " "
				       (symbol->string elt-2)))
		       (error
			(format
			 (string-append
			  "One symbol must be from "
			  "'~a, other from "
			  "'~a, got: ~a")
			 *horizontals* *verticals* pos))))
		 (if (andmap style:percentage-or-length? pos)
		     (mxprims:element-set-background-position!
		      elt
		      (string-append
		       (style:percentage-or-length->string (car pos))
		       " "
		       (style:percentage-or-length->string (cadr pos))))
		     (error
		      (format
		       (string-append
			"Two elements of list "
			" must be either a percentage or "
			" CSS length, got: ~a") pos))))]
	    [(style:percentage-or-length? pos)
	     (mxprims:element-set-background-position!
	      elt
	      (style:percentage-or-length->string pos))]
	    [else
	     (error
	      (format
	       (string-append
		"Expected any of "
		"1) a list of two symbols, one "
		"from '~a, the other from '~a, or "
		"2) a two element list, where each element is a "
		"percentage or CSS length, or "
		"3) a percentage, or "
		"4) a CSS length.  Got: ~a")
	       *horizontals* *verticals* pos))])))
	(define set-background-position-native!
	 (lambda (s)
	   (mxprims:element-set-background-position! elt s)))
	(define text-decoration
	 (lambda ()
	   (style:validated-string->symbols
	    (mxprims:element-text-decoration elt)
	    "text-decoration" style:parse-decoration)))
	(define text-decoration-native
	 (lambda ()
	   (mxprims:element-text-decoration elt)))
	(define set-text-decoration!
	 (lambda (decs)
	   (unless
	    (andmap decoration? decs)
	    (error
	     (format "Expected text decorations from ~a, got: ~a"
		     *decorations* decs)))
	   (mxprims:element-set-text-decoration! elt
						 (symbols->string decs))))
	(define set-text-decoration-native!
	 (lambda (s)
	   (mxprims:element-set-text-decoration! elt s)))
	(define text-transform
	 (lambda ()
	   (get-string-as-symbol mxprims:element-text-transform
				 "text-transform")))
	(define text-transform-native
	 (lambda ()
	   (mxprims:element-text-transform elt)))
	(define set-text-transform!
	 (lambda (sym)
	   (set-symbol-as-string
	    sym *text-transforms* mxprims:element-set-text-transform!
	    "set-text-transforms!")))
	(define set-text-transform-native!
	 (lambda (s)
	   (mxprims:element-set-text-transform! elt s)))
	(define text-align
	 (lambda ()
	   (get-string-as-symbol
	    mxprims:element-text-align
	    "text-align")))
	(define text-align-native
	 (lambda ()
	   (mxprims:element-text-align elt)))
	(define set-text-align!
	 (lambda (sym)
	   (set-symbol-as-string
	    sym *text-aligns*
	    mxprims:element-set-text-align!
	    "set-text-align!")))
	(define set-text-align-native!
	 (lambda (s)
	   (mxprims:element-set-text-align! elt s)))
	(define margin
	 (lambda ()
	   (let ([s (mxprims:element-margin elt)])
	     (if (empty-string? s)
		 (empty-property-error "margin")
		 (style:string->margin s)))))
	(define margin-native
	 (lambda ()
	   (mxprims:element-margin elt)))
	(define set-margin!
	 (lambda (lst)
	   (let ([len (length lst)])
	     (when (or (< len 1) (> len 4))
		   (error
		    "Expected one to four margin values, got"
		    lst)))
	   (mxprims:element-set-margin! elt (style:margin->string lst))))
	(define set-margin-native!
	 (lambda (s)
	   (mxprims:element-set-margin! elt s)))
	(define padding
	 (lambda ()
	   (let ([s (mxprims:element-padding elt)])
	     (if (empty-string? s)
		 (empty-property-error "padding")
		 (style:string->padding s)))))
	(define padding-native
	 (lambda ()
	   (mxprims:element-padding elt)))
	(define set-padding!
	 (lambda (pads)
	   (unless (and (pair? pads)
			(let ([len (length pads)])
			  (and (>= len 1) (<= len 4)))
			(andmap style:percentage-or-length? pads))
		   (error (string-append
			   "set-padding: expected list of "
			   "1 to 4 css-percentages or "
			   "css-lengths, got") pads))
	   (mxprims:element-set-padding!
	    elt
	    (style:padding->string pads))))
	(define set-padding-native!
	 (lambda (s)
	   (mxprims:element-set-padding! elt s)))
	(define border-raw
	 (style:make-border-getter elt mxprims:element-border "border"))
	(define border
	  (lambda args
	    (apply border-raw args)))
	(define border-native
	 (lambda (s)
	   (mxprims:element-border elt s)))
	(define set-border!
	 (lambda (cs)
	   (style:set-border-with-fun
	    elt cs mxprims:element-set-border!)))
	(define set-border-native!
	 (lambda (s)
	   (mxprims:element-set-border! elt s)))
	(define border-top-raw
	 (style:make-border-getter
	  elt mxprims:element-border-top "border-top"))
	(define border-top
	 (lambda args
	   (apply border-top-raw args)))
	(define border-top-native
	 (lambda ()
	   (mxprims:element-border-top elt)))
	(define set-border-top!
	 (lambda (cs)
	   (style:set-border-with-fun
	    elt cs mxprims:element-set-border-top!)))
	(define set-border-top-native!
	 (lambda (s)
	   (mxprims:element-set-border-top! elt s)))
	(define border-bottom-raw
	 (style:make-border-getter
	  elt mxprims:element-border-bottom "border-bottom"))
	(define border-bottom
	  (lambda args
	    (apply border-bottom-raw args)))
	(define border-bottom-native
	 (lambda ()
	   (mxprims:element-border-bottom elt)))
	(define set-border-bottom!
	 (lambda (cs)
	   (style:set-border-with-fun
	    elt cs mxprims:element-set-border-bottom!)))
	(define set-border-bottom-native!
	 (lambda (s)
	   (mxprims:element-set-border-bottom! elt s)))
	(define border-left-raw
	 (style:make-border-getter
	  elt mxprims:element-border-left "border-left"))
	(define border-left
	  (lambda args
	  (apply border-left-raw args)))
	(define border-left-native
	 (lambda ()
	   (mxprims:element-border-left elt)))
	(define set-border-left!
	 (lambda (cs)
	   (style:set-border-with-fun
	    elt cs mxprims:element-set-border-left!)))
	(define set-border-left-native!
	 (lambda (s)
	   (mxprims:element-set-border-left! elt s)))
	(define border-right-raw
	 (style:make-border-getter
	  elt mxprims:element-border-right "border-right"))
	(define border-right
	  (lambda args
	    (apply border-right-raw args)))
	(define border-right-native
	 (lambda ()
	   (mxprims:element-border-right elt)))
	(define set-border-right!
	 (lambda (cs)
	   (style:set-border-with-fun
	    elt cs mxprims:element-set-border-right!)))
	(define set-border-right-native!
	 (lambda (s)
	   (mxprims:element-set-border-right! elt s)))
	(define border-color-raw
	 (style:make-color-getter
	  elt
	  mxprims:element-border-color
	  "border-color"))
	(define border-color
	  (lambda args
	    (apply border-color-raw args)))
	(define border-color-native
	 (lambda ()
	   (mxprims:element-border-color elt)))
	(define set-border-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-border-color!
	  "set-border-color!"))
	(define set-border-color!
	  (lambda args
	    (apply set-border-color-raw! args)))
	(define set-border-color-native!
	 (lambda (s)
	   (mxprims:element-set-border-color! elt s)))
	(define border-width-raw
	 (style:make-border-width-getter
	  elt mxprims:element-border-width "border-width"))
	(define border-width
	  (lambda args
	    (apply border-width-raw args)))
	(define border-width-native
	 (lambda ()
	   (mxprims:element-border-width elt)))
	(define set-border-width!
	 (lambda (s)
	   (unless (style:border-width? s)
		   (error
		    (format "border-width: Expected element of ~a or CSS length, got: ~a"
			    *border-widths* s)))
	   (mxprims:element-set-border-width!
	    elt
	    (style:border-width->string s))))
	(define set-border-width-native!
	 (lambda (s)
	   (mxprims:element-set-border-width! elt s)))
	(define border-style-raw
	 (style:make-border-style-getter
	  elt mxprims:element-border-style
	  "border-style"))
	(define border-style
	  (lambda args
	    (apply border-style-raw args)))
	(define border-style-native
	 (lambda ()
	   (mxprims:element-border-style elt)))
	(define set-border-style-raw!
	 (style:make-border-style-setter
	  elt mxprims:element-set-border-style!
	  "set-border-style!"))
	(define set-border-style!
	  (lambda args
	    (apply set-border-style-raw! args)))
	(define set-border-style-native!
	 (lambda (s)
	   (mxprims:element-set-border-style! elt s)))
	(define border-top-style-raw
	 (style:make-border-style-getter
	  elt mxprims:element-border-top-style
	  "border-top-style"))
	(define border-top-style
	  (lambda args
	    (apply border-top-style-raw args)))
	(define border-top-style-native
	 (lambda ()
	   (mxprims:element-border-top-style elt)))
	(define set-border-top-style-raw!
	 (style:make-border-style-setter
	  elt mxprims:element-set-border-top-style!
	  "set-border-top-style!"))
	(define set-border-top-style!
	  (lambda args
	    (apply set-border-top-style-raw! args)))
	(define set-border-top-style-native!
	 (lambda (s)
	   (mxprims:element-set-border-top-style! elt s)))
	(define border-bottom-style-raw
	 (style:make-border-style-getter
	  elt mxprims:element-border-bottom-style
	  "border-bottom-style"))
	(define border-bottom-style
	  (lambda args
	    (apply border-bottom-style-raw args)))
	(define border-bottom-style-native
	 (lambda ()
	   (mxprims:element-border-bottom-style elt)))
	(define set-border-bottom-style-raw!
	 (style:make-border-style-setter
	  elt mxprims:element-set-border-bottom-style!
	  "set-border-bottom-style!"))
	(define set-border-bottom-style!
	  (lambda args
	    (apply set-border-bottom-style-raw! args)))
	(define set-border-bottom-style-native!
	 (lambda (s)
	   (mxprims:element-set-border-bottom-style! elt s)))
	(define border-left-style-raw
	 (style:make-border-style-getter
	  elt mxprims:element-border-left-style
	  "border-left-style"))
	(define border-left-style
	  (lambda args
	    (apply border-left-style-raw args)))
	(define border-left-style-native
	 (lambda ()
	   (mxprims:element-border-left-style elt)))
	(define set-border-left-style-raw!
	 (style:make-border-style-setter
	  elt mxprims:element-set-border-left-style!
	  "set-border-left-style!"))
	(define set-border-left-style!
	  (lambda args
	    (apply set-border-left-style-raw! args)))
	(define set-border-left-style-native!
	 (lambda (s)
	   (mxprims:element-set-border-left-style! elt s)))
	(define border-right-style-raw
	 (style:make-border-style-getter
	  elt mxprims:element-border-right-style
	  "border-right-style"))
	(define border-right-style
	  (lambda args
	    (apply border-right-style-raw args)))
	(define border-right-style-native
	 (lambda ()
	   (mxprims:element-border-right-style elt)))
	(define set-border-right-style-raw!
	 (style:make-border-style-setter elt
					 mxprims:element-set-border-right-style!
					 "set-border-right-style!"))
	(define set-border-right-style!
	  (lambda args
	    (apply set-border-right-style-raw! args)))
	(define set-border-right-style-native!
	 (lambda (s)
	   (mxprims:element-set-border-right-style! elt s)))
	(define border-top-color-raw
	 (style:make-color-getter
	  elt mxprims:element-border-top-color
	  "border-top-color"))
	(define border-top-color
	  (lambda args
	    (apply border-top-color-raw args)))
	(define border-top-color-native!
	 (lambda ()
	   (mxprims:element-border-top-color elt)))
	(define set-border-top-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-border-top-color!
	  "set-border-top-color!"))
	(define set-border-top-color!
	  (lambda args
	    (apply set-border-top-color-raw! args)))
	(define set-border-top-color-native!
	 (lambda (s)
	   (mxprims:element-set-border-top-color! elt s)))
	(define border-bottom-color-raw
	 (style:make-color-getter
	  elt mxprims:element-border-bottom-color
	  "border-bottom-color"))
	(define border-bottom-color
	  (lambda args
	    (apply border-bottom-color-raw args)))
	(define border-bottom-color-native
	 (lambda ()
	   (mxprims:element-border-bottom-color elt)))
	(define set-border-bottom-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-border-bottom-color!
	  "set-border-bottom-color!"))
	(define set-border-bottom-color!
	  (lambda args
	    (apply set-border-bottom-color-raw! args)))
	(define set-border-bottom-color-native!
	 (lambda (s)
	   (mxprims:element-set-border-bottom-color! elt s)))
	(define border-left-color-raw
	 (style:make-color-getter
	  elt mxprims:element-border-left-color
	  "border-left-color"))
	(define border-left-color
	  (lambda args
	    (apply border-left-color-raw args)))
	(define border-left-color-native
	 (lambda ()
	   (mxprims:element-border-left-color elt)))
	(define set-border-left-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-border-left-color!
	  "set-border-left-color!"))
	(define set-border-left-color!
	  (lambda args
	    (apply set-border-left-color-raw! args)))
	(define set-border-left-color-native!
	 (lambda (s)
	   (mxprims:element-set-border-left-color! elt s)))
	(define border-right-color-raw
	 (style:make-color-getter
	  elt mxprims:element-border-right-color
	  "border-right-color"))
	(define border-right-color
	  (lambda args
	    (apply border-right-color-raw args)))
	(define border-right-color-native
	 (lambda ()
	   (mxprims:element-border-right-color elt)))
	(define set-border-right-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-border-right-color!
	  "set-border-right-color!"))
	(define set-border-right-color!
	  (lambda args
	    (apply set-border-right-color-raw! args)))
	(define set-border-right-color-native!
	 (lambda (s)
	   (mxprims:element-set-border-right-color! elt s)))
	(define border-top-width-raw
	 (style:make-border-width-getter
	  elt mxprims:element-border-top-width "border-top-width"))
	(define border-top-width
	  (lambda args
	    (apply border-top-width-raw args)))
	(define border-top-width-native
	 (lambda ()
	   (mxprims:element-border-top-width elt)))
	(define set-border-top-width-raw!
	 (style:make-border-width-setter
	  elt mxprims:element-set-border-top-width!
	  "set-border-top-width!"))
	(define set-border-top-width!
	  (lambda args
	    (apply set-border-top-width-raw! args)))
	(define set-border-top-width-native!
	 (lambda (s)
	   (mxprims:element-set-border-top-width! elt s)))
	(define border-bottom-width-raw
	 (style:make-border-width-getter
	  elt mxprims:element-border-bottom-width "border-bottom-width"))
	(define border-bottom-width
	  (lambda args
	    (apply border-bottom-width-raw args)))
	(define border-bottom-width-native
	 (lambda ()
	   (mxprims:element-border-bottom-width elt)))
	(define set-border-bottom-width-raw!
	 (style:make-border-width-setter
	  elt mxprims:element-set-border-bottom-width!
	  "set-border-bottom-width!"))
	(define set-border-bottom-width!
	  (lambda args
	    (apply set-border-bottom-width-raw! args)))
	(define set-border-bottom-width-native!
	 (lambda (s)
	   (mxprims:element-set-border-bottom-width! elt s)))
	(define border-left-width-raw
	 (style:make-border-width-getter
	  elt mxprims:element-border-left-width "border-left-width"))
	(define border-left-width
	  (lambda args
	    (apply border-left-width-raw args)))
	(define border-left-width-native
	 (lambda ()
	   (mxprims:element-border-left-width elt)))
	(define set-border-left-width-raw!
	 (style:make-border-width-setter
	  elt mxprims:element-set-border-left-width!
	  "set-border-left-width!"))
	(define set-border-left-width!
	  (lambda args
	    (apply set-border-left-width-raw! args)))
	(define set-border-left-width-native!
	 (lambda (s)
	   (mxprims:element-set-border-left-width! elt s)))
	(define border-right-width-raw
	 (style:make-border-width-getter
	  elt mxprims:element-border-right-width "border-right-width"))
	(define border-right-width
	  (lambda args
	    (apply border-right-width-raw args)))
	(define border-right-width-native
	 (lambda ()
	   (mxprims:element-border-right-width elt)))
	(define set-border-right-width-raw!
	 (style:make-border-width-setter
	  elt mxprims:element-set-border-right-width!
	  "set-border-right-width!"))
	(define set-border-right-width!
	  (lambda args
	    (apply set-border-right-width-raw! args)))
	(define set-border-right-width-native!
	 (lambda (s)
	   (mxprims:element-set-border-right-width! elt s)))
	(define style-float-raw
	 (style:make-element-getter
	  elt
	  mxprims:element-style-float "style-float" ))
	(define style-float
	  (lambda args
	    (apply style-float-raw args)))
	(define style-float-native
	 (lambda ()
	   (mxprims:element-style-float elt)))
	(define set-style-float-raw!
	 (style:make-element-setter elt
				    style-float?
				    *style-floats*
				    mxprims:element-set-style-float!))
	(define set-style-float!
	  (lambda args
	    (apply set-style-float-raw! args)))
	(define set-style-float-native!
	 (lambda (s)
	   (mxprims:element-set-style-float! elt s)))
	(define clear-raw
	 (style:make-element-getter
	  elt mxprims:element-clear "clear"))
	(define clear
	  (lambda args
	    (apply clear-raw args)))
	(define clear-native
	 (lambda ()
	   (mxprims:element-clear elt)))
	(define set-clear-raw!
	 (style:make-element-setter elt
				    clear?
				    *clears*
				    mxprims:element-set-clear!))
	(define set-clear!
	  (lambda args
	    (apply set-clear-raw! args)))
	(define set-clear-native!
	 (lambda (s)
	   (mxprims:element-set-clear! elt s)))
	(define display-raw
	 (style:make-element-getter
	  elt mxprims:element-display "display"))
	(define display
	  (lambda args
	    (apply display-raw args)))
	(define display-native
	 (lambda ()
	   (mxprims:element-display elt)))
	(define set-display-raw!
	 (style:make-element-setter elt
				    display?
				    *displays*
				    mxprims:element-set-display!))
	(define set-display!
	  (lambda args
	    (apply set-display-raw! args)))
	(define set-display-native!
	 (lambda (s)
	   (mxprims:element-set-display! elt s)))
	(define visibility-raw
	 (style:make-element-getter
	  elt mxprims:element-visibility
	  "visibility"))
	(define visibility
	  (lambda args
	    (apply visibility-raw args)))
	(define visibility-native
	 (lambda ()
	   (mxprims:element-visibility elt)))
	(define set-visibility-raw!
	 (style:make-element-setter elt
				    visibility?
				    *visibilities*
				    mxprims:element-set-visibility!))
	(define set-visibility!
	  (lambda args
	    (apply set-visibility-raw! args)))
	(define set-visibility-native!
	 (lambda (s)
	   (mxprims:element-set-visibility! elt s)))
	(define list-style-type-raw
	 (style:make-element-getter
	  elt mxprims:element-list-style-type
	  "list-style-type"))
	(define list-style-type
	  (lambda args
	    (apply list-style-type-raw args)))
	(define list-style-type-native
	 (lambda ()
	   (mxprims:element-list-style-type elt)))
	(define set-list-style-type-native!
	 (lambda (s)
	   (mxprims:element-set-list-style-type! elt s)))
	(define list-style-position-raw
	 (style:make-element-getter
	  elt mxprims:element-list-style-position
	  "list-style-position"))
	(define list-style-position
	  (lambda args
	    (apply list-style-position-raw args)))
	(define list-style-position-native
	 (lambda ()
	   (mxprims:element-list-style-position elt)))
	(define set-list-style-position-native!
	 (lambda (s)
	   (mxprims:element-set-list-style-position! elt s)))
	(define list-style-image
	 (lambda ()
	   (let ([s (mxprims:element-list-style-image  elt)])
	     (when (empty-string? s)
		   (empty-property-error "list-style-image"))
	     (cond
	      [(string-ci=? s "none") 'none]
	      [(string-ci=? (substring s 0 4) "url(")
	       (style:url->string s)]
	      [else
	       (error
		(format
		 "list-style-image: Expected 'none or URL, got: ~a" s))]))))
	(define list-style-image-native
	 (lambda ()
	   (mxprims:element-list-style-image elt)))
	(define set-list-style-image!
	 (lambda (s)
	   (let ([str (if (eq? s 'none)
			  "none"
			  (style:string->url s))])
	     (mxprims:element-set-list-style-image! elt str))))
	(define set-list-style-image-native!
	 (lambda (s)
	   (mxprims:element-set-list-style-image! elt s)))
	(define list-style
	 (lambda ()
	   (let* ([s (mxprims:element-list-style elt)]
		  [elts (style:parse-string s)])
	     (map style:string->list-style-item elts))))
	(define list-style-native
	 (lambda ()
	   (mxprims:element-list-style elt)))
	(define set-list-style!
	 (lambda (items)
	   (mxprims:element-set-list-style!
	    elt
	    (fold-strings-with-spaces
	     (map style:list-style-item->string items)))))
	(define set-list-style-native!
	 (lambda (s)
	   (mxprims:element-set-list-style! elt s)))
	(define position-raw
	 (style:make-element-getter
	  elt mxprims:element-position
	  "position"))
	(define position
	  (lambda args
	    (apply position-raw args)))
	(define position-native
	 (lambda ()
	   (mxprims:element-position elt)))
	(define overflow-raw
	 (style:make-element-getter
	  elt mxprims:element-overflow "overflow"))
	(define overflow
	  (lambda args
	    (apply overflow-raw args)))
	(define overflow-native
	 (lambda ()
	   (mxprims:element-overflow elt)))
	(define set-overflow-raw!
	 (style:make-element-setter elt
				    overflow?
				    *overflows*
				    mxprims:element-set-overflow!))
	(define set-overflow!
	  (lambda args
	    (apply set-overflow-raw! args)))
	(define set-overflow-native!
	 (lambda (s)
	   (mxprims:element-set-overflow! elt s)))
	(define pagebreak-before-raw
	 (style:make-pagebreak-getter
	  elt
	  mxprims:element-pagebreak-before))
	(define pagebreak-before
	  (lambda args
	    (apply pagebreak-before-raw args)))
	(define pagebreak-before-native
	 (lambda ()
	   (mxprims:element-pagebreak-before elt)))
	(define set-pagebreak-before-raw!
	 (style:make-pagebreak-setter elt
				      mxprims:element-set-pagebreak-before!
				      "set-pagebreak-before!"))
	(define set-pagebreak-before!
	  (lambda args
	    (apply set-pagebreak-before-raw! args)))
	(define set-pagebreak-before-native!
	 (lambda (s)
	   (mxprims:element-set-pagebreak-before! elt s)))
	(define pagebreak-after-raw
	 (style:make-pagebreak-getter
	  elt mxprims:element-pagebreak-after))
	(define pagebreak-after
	  (lambda args
	    (apply pagebreak-after-raw args)))
	(define pagebreak-after-native
	 (lambda ()
	   (mxprims:element-pagebreak-after elt)))
	(define set-pagebreak-after-raw!
	 (style:make-pagebreak-setter elt
				      mxprims:element-set-pagebreak-after!
				      "set-pagebreak-after!"))
	(define set-pagebreak-after!
	  (lambda args
	    (apply set-pagebreak-after-raw! args)))
	(define css-text-native
	 (lambda ()
	   (mxprims:element-css-text elt)))
	(define set-css-text-native!
	 (lambda (s)
	   (mxprims:element-set-css-text! elt s)))
	(define cursor-raw
	 (style:make-element-getter
	  elt mxprims:element-cursor "cursor"))
	(define cursor
	  (lambda args
	    (apply cursor-raw args)))
	(define cursor-native
	 (lambda ()
	   (mxprims:element-cursor elt)))
	(define set-cursor-raw!
	 (style:make-element-setter elt
				    cursor?
				    *cursors*
				    mxprims:element-set-cursor!))
	(define set-cursor!
	  (lambda args
	    (apply set-cursor-raw! args)))
	(define set-cursor-native!
	 (lambda (s)
	   (mxprims:element-set-cursor! elt s)))
	(define clip
	 (lambda ()
	   (let ([s (mxprims:element-clip elt)])
	     (cond
	      [(empty-string? s)
	       (empty-property-error "clip")]
	      [(string-ci=? s "auto")
	       'auto]
	      [(style:clip-rect? s)
	       (style:clip-rect->symbols s)]
	      [else
	       (error
		(format "clip: Expected clip string, got: ~a" s))]))))
	(define clip-native
	 (lambda ()
	   (mxprims:element-clip elt)))
	(define set-clip!
	 (lambda (s)
	   (let ([str (cond
		       [(eq? s 'auto) "auto"]
		       [(and (pair? s)
			     (= (length s) 4)
			     (andmap
			      (lambda (elt)
				(or (eq? elt 'auto)
				    (css-length? elt)))
			      s))
			(string-append
			 "rect("
			 (fold-strings-with-spaces
			  (map
			   (lambda (elt)
			     (if (eq? elt 'auto)
				 "auto"
				 (style:css-length->string elt)))
			   s))
			 ")")]
		       [else
			(error
			 (format
			  (string-append
			   "Expected 'auto or 4-element list of "
			   "CSS lengths, with elements "
			   "possibly replaced by 'auto. Got ~a")
			  s))])])
	     (mxprims:element-set-clip! elt str))))
	(define set-clip-native!
	 (lambda (s)
	   (mxprims:element-set-clip! elt s)))
	(define filter
	 (lambda ()
	   (let ([s (mxprims:element-filter elt)])
	     (if (empty-string? s)
		 (empty-property-error "filter")
		 (string->filter s)))))
	(define filter-native
	 (lambda ()
	   (mxprims:element-filter elt)))
	(define set-filter!
	 (lambda (flt . options)
	   (let ([s (filter->string flt options)])
	     (mxprims:element-set-filter! elt s))))
	(define set-filter-native!
	 (lambda (s)
	   (mxprims:element-set-filter! elt s)))
	(define style-string
	 (lambda ()
	   (mxprims:element-style-string elt)))
		; the text decoration, blink attributes are boolean
		; hence no conversion to/from strings
	(define text-decoration-none
	 (lambda ()
	   (mxprims:element-text-decoration-none elt)))
	(define set-text-decoration-none!
	 (lambda (s)
	   (mxprims:element-set-text-decoration-none! elt s)))
	(define text-decoration-underline
	 (lambda ()
	   (mxprims:element-text-decoration-underline elt)))
	(define set-text-decoration-underline!
	 (lambda (s)
	   (mxprims:element-set-text-decoration-underline! elt s)))
	(define text-decoration-overline
	 (lambda ()
	   (mxprims:element-text-decoration-overline elt)))
	(define set-text-decoration-overline!
	 (lambda (s)
	   (mxprims:element-set-text-decoration-overline! elt s)))
	(define text-decoration-linethrough
	 (lambda ()
	   (mxprims:element-text-decoration-linethrough elt)))
	(define set-text-decoration-linethrough!
	 (lambda (s)
	   (mxprims:element-set-text-decoration-linethrough! elt s)))
	(define text-decoration-blink
	 (lambda ()
	   (mxprims:element-text-decoration-blink elt)))
	(define set-text-decoration-blink!
	 (lambda (s)
	   (mxprims:element-set-text-decoration-blink! elt s)))
					; pixel attributes are all longs
					; hence, no conversion to/from strings
	(define pixel-top
	 (lambda ()
	   (mxprims:element-pixel-top elt)))
	(define set-pixel-top!
	 (lambda (s)
	   (mxprims:element-set-pixel-top! elt s)))
	(define pixel-left
	 (lambda ()
	   (mxprims:element-pixel-left elt)))
	(define set-pixel-left!
	 (lambda (s)
	   (mxprims:element-set-pixel-left! elt s)))
	(define pixel-width
	 (lambda ()
	   (mxprims:element-pixel-width elt)))
	(define set-pixel-width!
	 (lambda (s)
	   (mxprims:element-set-pixel-width! elt s)))
	(define pixel-height
	 (lambda ()
	   (mxprims:element-pixel-height elt)))
	(define set-pixel-height!
	 (lambda (s)
	   (mxprims:element-set-pixel-height! elt s)))
  	; position attributes are all floats
	; hence no conversion to/from strings
	(define pos-top
	 (lambda ()
	   (mxprims:element-pos-top elt)))
	(define set-pos-top!
	 (lambda (s)
	   (mxprims:element-set-pos-top! elt s)))
	(define pos-left
	 (lambda ()
	   (mxprims:element-pos-left elt)))
	(define set-pos-left!
	 (lambda (s)
	   (mxprims:element-set-pos-left! elt s)))
	(define pos-width
	 (lambda ()
	   (mxprims:element-pos-width elt)))
	(define set-pos-width!
	 (lambda (s)
	   (mxprims:element-set-pos-width! elt s)))
	(define pos-height
	 (lambda ()
	   (mxprims:element-pos-height elt)))
	(define set-pos-height!
	 (lambda (s)
	   (mxprims:element-set-pos-height! elt s)))
	(define font-size
	 (lambda ()
	   (let ([s (mxprims:element-font-size elt)])
	     (if (empty-string? s)
		 (empty-property-error "font-size")
		 (style:string->font-size s)))))
	(define font-size-native
	 (lambda ()
	   (mxprims:element-font-size elt)))
	(define set-font-size!
	 (lambda (sz)
	   (let ([s (cond
		     [(font-size? sz)
		      (symbol->string sz)]
		     [(style:percentage-or-length? sz)
		      (style:percentage-or-length->string sz)]
		     [else
		      (error
		       (format (string-append
				"set-font-size!: Expected element of ~a, "
				"a CSS length, or CSS percentage. Got: ~a")
			       *font-sizes* sz))])])
	     (mxprims:element-set-font-size! elt s))))
	(define set-font-size-native!
	 (lambda (s)
	   (mxprims:element-set-font-size! elt s)))
	(define color-raw
	 (style:make-color-getter elt mxprims:element-color "color"))
	(define color
	  (lambda args
	    (apply color-raw args)))
	(define color-native-raw
	  (lambda ()
	    (mxprims:element-color elt)))
	(define color-native
	  (lambda args
	    (apply color-native-raw args)))
	(define set-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-color! "set-color!"))
	(define set-color!
	  (lambda args
	    (apply set-color-raw! args)))
	(define set-color-native!
	 (lambda (s)
	   (mxprims:element-set-color! elt s)))
	(define background-color-raw
	 (style:make-color-getter
	  elt mxprims:element-background-color
	  "background-color"))
	(define background-color
	  (lambda args
	    (apply background-color-raw args)))
	(define background-color-native
	 (lambda ()
	   (mxprims:element-background-color elt)))
	(define set-background-color-raw!
	 (style:make-color-setter
	  elt mxprims:element-set-background-color!
	  "set-background-color!"))
	(define set-background-color!
	  (lambda args
	    (apply set-background-color-raw! args)))
	(define set-background-color-native!
	 (lambda (s)
	   (mxprims:element-set-background-color! elt s)))
	(define background-position-x-raw
	 (style:make-bg-pos-getter
	  elt
	  mxprims:element-background-position-x
	  "background-position-x"))
	(define background-position-x
	  (lambda args
	    (apply background-position-x-raw args)))
	(define background-position-x-native
	 (lambda ()
	   (mxprims:element-background-position-x elt)))
	(define set-background-position-x-raw!
	 (style:make-bg-pos-setter
	  elt
	  mxprims:element-set-background-position-x!
	  horizontal? *horizontals*
	  "x"))
	(define set-background-position-x!
	  (lambda args
	    (apply set-background-position-x-raw! args)))
	(define set-background-position-x-native!
	 (lambda (n)
	   (mxprims:element-set-background-position-x! elt n)))
	(define background-position-y-raw
	 (style:make-bg-pos-getter
	  elt
	  mxprims:element-background-position-y
	  "background-position-y"))
	(define background-position-y
	  (lambda args
	    (apply background-position-y-raw args)))
	(define background-position-y-native
	 (lambda ()
	   (mxprims:element-background-position-y elt)))
	(define set-background-position-y-raw!
	 (style:make-bg-pos-setter
	  elt
	  mxprims:element-set-background-position-y!
	  vertical? *verticals*
	  "y"))
	(define set-background-position-y!
	  (lambda args
	    (apply set-background-position-y-raw! args)))
	(define set-background-position-y-native!
	 (lambda (s)
	   (mxprims:element-set-background-position-y! elt s)))
	(define letter-spacing-raw
	 (style:make-normal-or-css-getter elt
					  mxprims:element-letter-spacing
					  "letter-spacing"))
	(define letter-spacing
	  (lambda args
	    (apply letter-spacing-raw args)))
	(define letter-spacing-native
	 (lambda ()
	   (mxprims:element-letter-spacing elt)))
	(define set-letter-spacing-raw!
	 (style:make-normal-or-css-setter elt
					  mxprims:element-set-letter-spacing!
					  "set-letter-spacing!"))
	(define set-letter-spacing!
	  (lambda args
	    (apply set-letter-spacing-raw! args)))
	(define set-letter-spacing-native!
	 (lambda (s)
	   (mxprims:element-set-letter-spacing! elt s)))
	(define vertical-align
	 (lambda ()
	   (let ([s (mxprims:element-vertical-align elt)])
	     (when (empty-string? s)
		   (empty-property-error "vertical-align"))
	     (string->symbol s))))
	(define vertical-align-native
	 (lambda ()
	   (mxprims:element-vertical-align elt)))
	(define set-vertical-align!
	 (lambda (sym)
	   (unless (vertical-align? sym)
		   (error
		    (format
		     (string-append "set-vertical-align!: "
				    "Expected element of ~a, got ~a")
		     *vertical-aligns* sym)))
	   (mxprims:element-set-vertical-align!
	    elt
	    (symbol->string sym))))
	(define set-vertical-align-native!
	 (lambda (s)
	   (mxprims:element-set-vertical-align! elt s)))
	(define text-indent-raw
	 (style:make-css-getter elt
				mxprims:element-text-indent "text-indent"))
	(define text-indent
	  (lambda args
	    (apply text-indent-raw args)))
	(define text-indent-native
	 (lambda ()
	   (mxprims:element-text-indent elt)))
	(define set-text-indent-raw!
	 (style:make-css-setter elt
				mxprims:element-set-text-indent!
                               "set-text-indent!"))
	(define set-text-indent!
	  (lambda args
	    (apply set-text-indent-raw! args)))
	(define set-text-indent-native!
	 (lambda (s)
	   (mxprims:element-set-text-indent! elt s)))
	(define line-height-raw
	 (style:make-normal-or-css-getter elt
					  mxprims:element-line-height
					  "line-height"))
	(define line-height
	  (lambda args
	    (apply line-height-raw args)))
	(define line-height-native
	 (lambda ()
	   (mxprims:element-line-height elt)))
	(define set-line-height-raw!
	 (style:make-normal-or-css-setter elt
					  mxprims:element-set-line-height!
					  "set-line-height!"))
	(define set-line-height!
	  (lambda args
	    (apply set-line-height-raw! args)))
	(define set-line-height-native!
	 (lambda (s)
	   (mxprims:element-set-line-height! elt s)))
	(define margin-top-raw
	 (style:make-auto-or-css-getter elt
					mxprims:element-margin-top
					"margin-top"))
	(define margin-top
	  (lambda args
	    (apply margin-top-raw args)))
	(define margin-top-native
	 (lambda ()
	   (mxprims:element-margin-top elt)))
	(define set-margin-top-raw!
	 (style:make-auto-or-css-setter elt
					mxprims:element-set-margin-top!
					"set-margin-top!"))
	(define set-margin-top!
	  (lambda args
	    (apply set-margin-top-raw! args)))
	(define set-margin-top-native!
	 (lambda (s)
	   (mxprims:element-set-margin-top! elt s)))
	(define margin-bottom-raw
	 (style:make-auto-or-css-getter elt
					mxprims:element-margin-bottom
					"margin-bottom"))
	(define margin-bottom
	  (lambda args
	    (apply margin-bottom-raw args)))
	(define margin-bottom-native
	 (lambda ()
	   (mxprims:element-margin-bottom elt)))
	(define set-margin-bottom-raw!
	 (style:make-auto-or-css-setter elt
					mxprims:element-set-margin-bottom!
					"set-margin-bottom!"))
	(define set-margin-bottom!
	  (lambda args
	    (apply set-margin-bottom-raw! args)))
	(define set-margin-bottom-native!
	 (lambda (s)
	   (mxprims:element-set-margin-bottom! elt s)))
	(define margin-left-raw
	 (style:make-auto-or-css-getter elt
					mxprims:element-margin-left
					"margin-left"))
	(define margin-left
	  (lambda args
	    (apply margin-left-raw args)))
	(define margin-left-native
	 (lambda ()
	   (mxprims:element-margin-left elt)))
	(define set-margin-left-raw!
	 (style:make-auto-or-css-setter elt
					mxprims:element-set-margin-left!
					"set-margin-left!"))
	(define set-margin-left!
	  (lambda args
	    (apply set-margin-left-raw! args)))
	(define set-margin-left-native!
	 (lambda (s)
	   (mxprims:element-set-margin-left! elt s)))
	(define margin-right-raw
	 (style:make-auto-or-css-getter elt
					mxprims:element-margin-right
					"margin-right"))
	(define margin-right
	  (lambda args
	    (apply margin-right-raw args)))
	(define margin-right-native
	 (lambda ()
	   (mxprims:element-margin-right elt)))
	(define set-margin-right-raw!
	 (style:make-auto-or-css-setter elt
					mxprims:element-set-margin-right!
					"set-margin-right!"))
	(define set-margin-right!
	  (lambda args
	    (apply set-margin-right-raw! args)))
	(define set-margin-right-native!
	 (lambda (s)
	   (mxprims:element-set-margin-right! elt s)))
	(define padding-top-raw
	 (style:make-css-getter elt
				mxprims:element-padding-top "padding-top"))
	(define padding-top
	  (lambda args
	    (apply padding-top-raw args)))
	(define padding-top-native
	 (lambda ()
	   (mxprims:element-padding-top elt)))
	(define set-padding-top-raw!
	 (style:make-css-setter elt
				mxprims:element-set-padding-top! "set-padding-top!"))
	(define set-padding-top!
	  (lambda args
	    (apply set-padding-top-raw! args)))
	(define set-padding-top-native!
	 (lambda (s)
	   (mxprims:element-set-padding-top! elt s)))
	(define padding-bottom-raw
	 (style:make-css-getter elt
				mxprims:element-padding-bottom "padding-bottom"))
	(define padding-bottom
	  (lambda args
	    (apply padding-bottom-raw args)))
	(define padding-bottom-native
	 (lambda ()
	   (mxprims:element-padding-bottom elt)))
	(define set-padding-bottom-raw!
	 (style:make-css-setter elt
				mxprims:element-set-padding-bottom! "set-padding-bottom!"))
	(define set-padding-bottom!
	  (lambda args
	    (apply set-padding-bottom-raw! args)))
	(define set-padding-bottom-native!
	 (lambda (s)
	   (mxprims:element-set-padding-bottom! elt s)))
	(define padding-left-raw
	 (style:make-css-getter elt
				mxprims:element-padding-left "padding-left"))
	(define padding-left
	  (lambda args
	    (apply padding-left-raw args)))
	(define padding-left-native
	 (lambda ()
	   (mxprims:element-padding-left elt)))
	(define set-padding-left-raw!
	 (style:make-css-setter elt
				mxprims:element-set-padding-left! "set-padding-left!"))
	(define set-padding-left!
	  (lambda args
	    (apply set-padding-left-raw! args)))
	(define set-padding-left-native!
	 (lambda (s)
	   (mxprims:element-set-padding-left! elt s)))
	(define padding-right-raw
	 (style:make-css-getter elt
				mxprims:element-padding-right "padding-right"))
	(define padding-right
	  (lambda args
	    (apply padding-right-raw args)))
	(define padding-right-native
	 (lambda ()
	   (mxprims:element-padding-right elt)))
	(define set-padding-right-raw!
	 (style:make-css-setter elt
				mxprims:element-set-padding-right! "set-padding-right!"))
	(define set-padding-right!
	  (lambda args
	    (apply set-padding-right-raw! args)))
	(define set-padding-right-native!
	 (lambda (s)
	   (mxprims:element-set-padding-right! elt s)))
	(define width-raw
	 (style:make-auto-or-css-getter
	  elt mxprims:element-width "width"))
	(define width
	  (lambda args
	    (apply width-raw args)))
	(define width-native
	 (lambda ()
	   (mxprims:element-width elt)))
	(define set-width-raw!
	 (style:make-auto-or-css-setter
	  elt mxprims:element-set-width!
	  "set-width!"))
	(define set-width!
	  (lambda args
	    (apply set-width-raw! args)))
	(define set-width-native!
	 (lambda (s)
	   (mxprims:element-set-width! elt s)))
	(define height-raw
	 (style:make-auto-or-css-getter
	  elt mxprims:element-height "height"))
	(define height
	  (lambda args
	    (apply height-raw args)))
	(define height-native
	 (lambda ()
	   (mxprims:element-height elt)))
	(define set-height-raw!
	 (style:make-auto-or-css-setter
	  elt mxprims:element-set-height!
	  "set-height!"))
	(define set-height!
	  (lambda args
	    (apply set-height-raw! args)))
	(define set-height-native!
	 (lambda (s)
	   (mxprims:element-set-height! elt s)))
	(define top-raw
	 (style:make-auto-or-css-getter
	  elt mxprims:element-top "top"))
	(define top
	  (lambda args
	    (apply top-raw args)))
	(define top-native
	 (lambda ()
	   (mxprims:element-top elt)))
	(define set-top-raw!
	 (style:make-auto-or-css-setter
	  elt mxprims:element-set-top!
	  "set-top!"))
	(define set-top!
	  (lambda args
	    (apply set-top-raw! args)))
	(define set-top-native!
	 (lambda (s)
	   (mxprims:element-set-top! elt s)))
	(define left-raw
	 (style:make-auto-or-css-getter
	  elt mxprims:element-left "left"))
	(define left
	  (lambda args
	    (apply left-raw args)))
	(define left-native
	 (lambda ()
	   (mxprims:element-left elt)))
	(define set-left-raw!
	 (style:make-auto-or-css-setter
	  elt
	  mxprims:element-set-left!
	  "set-left!"))
	(define set-left!
	  (lambda args
	    (apply set-left-raw! args)))
	(define set-left-native!
	 (lambda (s)
	   (mxprims:element-set-left! elt s)))
	(define z-index
	 (lambda ()
	   (let ([s (mxprims:element-z-index elt)])
	     (when (empty-string? s)
		   (empty-property-error "z-index"))
	     (if (and (string? s) (string=? s "auto"))
		 'auto
		 s))))
	(define z-index-native
	 (lambda ()
	   (mxprims:element-z-index elt)))
	(define set-z-index!
	 (lambda (zi)
	   (let ([s (cond
		     [(eq? zi 'auto) "auto"]
		     [(and (number? zi)
			   (exact? zi)) zi]
		     [else
		      (error
		       (string-append "set-z-index!: "
				      "Expected 'auto or exact integer, "
				      "got")
		       zi)])])
	     (mxprims:element-set-z-index! elt s))))
	(define set-z-index-native!
	 (lambda (s)
	   (mxprims:element-set-z-index! elt s)))
	(define set-list-style-position-raw!
	  (style:make-element-setter elt
				     list-style-position?
				     *list-style-positions*
				     mxprims:element-set-list-style-position!))
	(define set-list-style-position!
	  (lambda args
	    (apply set-list-style-position-raw! args)))
	(define set-list-style-type-raw!
	  (style:make-element-setter elt
				     list-style-type?
				     *list-style-types*
				     mxprims:element-set-list-style-type!))
	(define set-list-style-type!
	  (lambda args
	    (apply set-list-style-type-raw! args)))
	(super-make-object)))

  (define mx-event%
    (class object% (init dhtml-event)

	; private fields

	(define event dhtml-event)

	(public
	 keypress?
	 keydown?
	 keyup?
	 mousedown?
	 mousemove?
	 mouseover?
	 mouseout?
	 mouseup?
	 click?
	 dblclick?
	 error?
	 tag
	 id
	 from-tag
	 from-id
	 to-tag
	 to-id
	 keycode
	 shift-key
	 ctrl-key
	 alt-key
	 x
	 y)

	; predicates

	(define keypress? (lambda () (mxprims:event-keypress? event)))
	(define keydown? (lambda () (mxprims:event-keydown? event)))
	(define keyup? (lambda () (mxprims:event-keyup? event)))
	(define mousedown? (lambda () (mxprims:event-mousedown? event)))
	(define mousemove? (lambda () (mxprims:event-mousemove? event)))
	(define mouseover? (lambda () (mxprims:event-mouseover? event)))
	(define mouseout? (lambda () (mxprims:event-mouseout? event)))
	(define mouseup? (lambda () (mxprims:event-mouseup? event)))
	(define click? (lambda () (mxprims:event-click? event)))
	(define dblclick? (lambda () (mxprims:event-dblclick? event)))
	(define error? (lambda () (mxprims:event-error? event)))

	; attributes

	(define tag (lambda () (mxprims:event-tag event)))
	(define id (lambda () (mxprims:event-id event)))
	(define from-tag (lambda () (mxprims:event-from-tag event)))
	(define from-id (lambda () (mxprims:event-id event)))
	(define to-tag (lambda () (mxprims:event-to-tag event)))
	(define to-id (lambda () (mxprims:event-to-id event)))
	(define keycode (lambda () (mxprims:event-keycode event)))
	(define shift-key (lambda () (mxprims:event-shiftkey event)))
	(define ctrl-key (lambda () (mxprims:event-ctrlkey event)))
	(define alt-key (lambda () (mxprims:event-altkey event)))
	(define x (lambda () (mxprims:event-x event)))
	(define y (lambda () (mxprims:event-y event)))
	(super-make-object)))

  (define mx-event<%> (class->interface mx-event%))

  (define mx-browser%
    (class object% (init (label "MysterX")
			 (width 'default)
			 (height 'default)
			 (x 'default)
			 (y 'default)
			 (style-options null))

	   ; private fields
	   (define browser (mxprims:make-browser label width height x y style-options))
	    (define thread-sem (make-semaphore 1))
	    (define thread-wait (lambda () (semaphore-wait thread-sem)))
	    (define thread-post (lambda () (semaphore-post thread-sem)))
	    (define navigate-sem (make-semaphore 0))
	    (define navigate-mutex (make-semaphore 1))
	    (define navigate-url #f)
	    (define handler-sem (make-semaphore 1))
	    (define handler-wait (lambda () (semaphore-wait handler-sem)))
	    (define handler-post (lambda () (semaphore-post handler-sem)))
	    (define handler-table (make-hash-table))
	    (define handler-thread #f)
	    (define make-navigator
	     (lambda (navigate-fun name)
	       (lambda url
		 (let ([actual-url #f])
		   (semaphore-wait navigate-mutex)
		   (if (apply navigate-fun (cons browser url))
		       (begin
			 (semaphore-wait navigate-sem)
			 (set! actual-url navigate-url))
		       (begin (semaphore-post navigate-mutex)
			      (error name "Error navigating browser")))
                   (semaphore-post navigate-mutex)
		   actual-url))))
	    (define block-until-event
	     (lambda () (mxprims:block-until-event browser)))
	    (define make-event-key
	     (lambda (tag id) ; string x string -> symbol
	       (let ([new-tag (string-copy tag)]
		     [new-id (string-copy id)])
		 (string-uppercase! new-tag)
		    (string-uppercase! new-id)
		    (string->symbol
		     (string-append new-tag "@" new-id)))))

	   (public
	    show
	    navigate
	    navigate/status
	    go-back
	    go-forward
	    refresh
	    iconize
	    restore
	    current-url
	    current-document
	    print-document
	    register-event-handler
	    unregister-event-handler
	    handle-events
	    stop-handling-events)

	    (define show
	     (lambda (b)
	       (mxprims:browser-show browser b)))
	    (define navigate/status
	     (lambda (url)
	       (let ([actual (navigate url)])
		 (if (and (>= (string-length actual) 7)
			  (string=? (substring actual 0 7)
				    "http://"))
		     (let* ([p (get-impure-port (string->url actual))]
			    [response (read-line p)]
			    [raw-status
			     (regexp-match "[0-9][0-9][0-9]" response)])
		       (close-input-port p)
		       (list actual
			     (if raw-status
				 (string->number (car raw-status))
				 #f)))
		     (list actual 'no-status)))))
	    (define navigate-raw
	     (make-navigator mxprims:navigate 'navigate))
	    (define navigate
	      (lambda args
		(apply navigate-raw args)))
	    (define go-back-raw
	     (make-navigator mxprims:go-back 'go-back))
	    (define go-back
	      (lambda args
		(apply go-back-raw args)))
	    (define go-forward-raw
	     (make-navigator mxprims:go-forward 'go-forward))
	    (define go-forward
	      (lambda args
		(apply go-forward-raw args)))
	    (define refresh
	     (lambda () (mxprims:refresh browser)))
	    (define iconize
	     (lambda () (mxprims:iconize browser)))
	    (define restore
	     (lambda () (mxprims:restore browser)))
	    (define current-url
	     (lambda ()
	       (mxprims:current-url browser)))
	    (define current-document
	     (lambda ()
	       (make-object mx-document%
			    (mxprims:current-document browser))))
	    (define print-document
	     (lambda ()
	       (mxprims:print-document browser)))
	    (define register-event-handler
	     (lambda (elt fn)
	       (dynamic-wind
		handler-wait
		(lambda ()
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")])
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key)
			 (hash-table-put! handler-table key fn))))
		handler-post)))
	    (define unregister-event-handler
	     (lambda (elt)
	       (dynamic-wind
		handler-wait
		(lambda ()
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")])
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key))))
		handler-post)))
	    (define handle-events
	     (lambda ()
	       (dynamic-wind
		thread-wait
                (lambda ()	; no-op if existing handler-thread
		  (unless handler-thread
			  (dynamic-wind
			   handler-wait
			   (lambda ()
			     (let* ([handler-thunk
				     (lambda ()
				       (let loop ()
					 (block-until-event)
				         (let* ([prim-event
						 (with-handlers
						  ([void
						    (lambda (e)
						      (printf "~a\n" (exn-message e))
						      (loop))])
						    (mxprims:get-event browser))]
						[event (make-object mx-event% prim-event)]
						[tag (send event tag)]
						[id (send event id)]
						[key (make-event-key tag id)]
						[handler (hash-table-get handler-table key void)])
					   (unless (void? handler)
						   (handler event))
					   (loop))))])
			       (set! handler-thread (thread handler-thunk))))
			   handler-post)))
		thread-post)))
	    (define stop-handling-events
	     (lambda ()
	       (dynamic-wind
		thread-wait
		(lambda ()
		  (when handler-thread
			(kill-thread handler-thread))
		  (set! handler-thread #f))
		thread-post)))

	    (super-make-object)
	    (mxprims:register-navigate-handler
	     browser
	     (lambda (_ boxed-url)
	       (set! navigate-url (current-url))
	       (semaphore-post navigate-sem)))))

  (define mx-document%
    (class object%
	   (init the-doc)

     ; private fields

     (define doc the-doc)

     (define insert-object-maker
       (lambda (name->html)
	 (opt-lambda
	  (object width height [size 'pixels])
	  (dynamic-wind
	      html-wait
	      (lambda ()
		(mxprims:document-insert-html
		 doc
		 (name->html object width height size))
		(car (mxprims:document-objects doc)))
	      html-post))))

     (define append-object-maker
       (lambda (name->html)
	 (opt-lambda
	  (object width height [size 'pixels])
	  (dynamic-wind
	      html-wait
	      (lambda ()
		(mxprims:document-append-html
		    doc
		    (name->html object width height size))
		(car (mzlib:last-pair (mxprims:document-objects doc))))
	      html-post))))

     (define html-insertion-maker
       (lambda (f)
	 (lambda (s)
	   (dynamic-wind
	       html-wait
	       (lambda () (f doc s))
	       html-post))))

     (public
      title
      find-element
      find-element-by-id-or-name
      elements-with-tag
      objects
      insert-html
      append-html
      replace-html
      insert-object-from-coclass
      append-object-from-coclass
      insert-object-from-progid
      append-object-from-progid)

     (define title
      (lambda ()
	(mxprims:document-title doc)))
     (define find-element
       (lambda (tag id . n)
	 (make-object mx-element% doc
		      (apply mxprims:document-find-element
			     doc tag id n))))
     (define find-element-by-id-or-name
       (lambda (id . n)
	 (make-object
	  mx-element% doc
	  (apply mxprims:document-find-element-by-id-or-name
		 doc id n))))
     (define elements-with-tag
       (lambda (tag)
	 (map
	  (lambda (elt)
	    (make-object mx-element% doc elt))
	  (mxprims:document-elements-with-tag doc tag))))
     (define objects
       (lambda ()
	 (mxprims:document-objects doc)))
     (define insert-html-raw
       (html-insertion-maker mxprims:document-insert-html))
     (define insert-html
       (lambda args
	 (apply insert-html-raw args)))
     (define append-html-raw
       (html-insertion-maker mxprims:document-append-html))
     (define append-html
       (lambda args
	 (apply append-html-raw args)))
     (define replace-html-raw
       (html-insertion-maker mxprims:document-replace-html))
     (define replace-html
       (lambda args
	 (apply replace-html-raw args)))
     (define insert-object-from-coclass-raw
       (insert-object-maker coclass->html))
     (define insert-object-from-coclass
       (lambda args
	 (apply insert-object-from-coclass-raw args)))
     (define append-object-from-coclass-raw
       (append-object-maker coclass->html))
     (define append-object-from-coclass
       (lambda args
	 (apply append-object-from-coclass-raw args)))
     (define insert-object-from-progid-raw
       (insert-object-maker progid->html))
     (define insert-object-from-progid
       (lambda args
	 (apply insert-object-from-progid-raw args)))
     (define append-object-from-progid-raw
       (append-object-maker progid->html))
     (define append-object-from-progid
       (lambda args
	 (apply append-object-from-progid-raw args)))

     (super-make-object)))

  (define mx-document<%> (class->interface mx-document%))

  (thread
   (lambda ()
     (let loop ()
       (mxprims:process-win-events)
       (sleep 0.01)
       (loop)))))
