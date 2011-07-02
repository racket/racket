;;; filter.rkt

(module filter mzscheme
  (require mzlib/string
           "properties.rkt"
           "util.rkt")

  (provide
   string->filter
   filter->string)

  (define (alpha-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(finish-opacity opacity) 
	 (exact-with-bounds? val 0 100)]
	[(finish-x finish-y)
	 (and (number? val)
	      (exact? val))]
	[(finish-x finish-y start-x starty-y)
	 (and (number? val)
	      (exact? val))]
	[(style)
	 (alpha-filter-style? val)]
	[else #f])))

  (define (alpha-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(finish-opacity opacity finish-x finish-y start-x start-y)
	 (number->string val)]
	[(style)
	 (number->string (list-pos val *alpha-filter-styles*))]
	[else (error "Can't translate alpha filter option" opt)])))

  (define (blend-trans-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enable) #t]
	[(duration) (number? val)]
	[(status) (trans-filter-status? val)] 
	[else #f])))

  (define (blend-trans-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enable) (bool->string val)]
	[(duration) (number->string val)]
	[(status) (list-pos val *trans-filter-statuses*)]
	[else (error "Can't translate blend-trans filter option" opt)])))

  (define (blur-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(add enabled) #t]
	[(direction) (filter-direction? val)]
	[(strength) (exact-with-bounds? val 1 100)]
	[else #f])))

  (define (blur-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(add enabled) (bool->string val)]
	[(direction strength) (number->string val)]
	[else (error "Can't translate blur filter option" opt)])))

  (define (chroma-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(color) (hex-color-string? val)]
	[else #f])))

  (define (chroma-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(color) val]
	[else (error "Can't translate chroma filter option" opt)])))

  (define (drop-shadow-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled positive) #t]
	[(color) (hex-color-string? val)]
	[(off-x off-y) 
	 (and (number? val)
	      (exact? val))]
	[else #f])))

  (define (drop-shadow-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled positive) (bool->string val)]
	[(color) val]
	[(off-x off-y) (number->string val)]
	[else (error "Can't translate drop-shadow filter option" opt)])))

  (define (basic-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled positive) #t]
	[else #f])))

  (define (basic-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled positive) (bool->string val)]
	[else (error "Can't translate basic filter option" opt)])))

  (define (glow-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(color) (hex-color-string? val)]
	[(strength) (exact-with-bounds? val 1 100)]
	[else #f])))

  (define (glow-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(color) val]
	[(strength) (number->string val)]
	[else (error "Can't translate glow filter option" opt)])))


  (define (mask-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(color) (hex-color-string? val)]
	[else #f])))

  (define (mask-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(color) val]
	[else (error "Can't translate mask filter option" opt)])))

  (define (reveal-trans-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(duration) (number? val)]
	[(status) (trans-filter-status? val)] 
	[else #f])))

  (define (reveal-trans-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(duration) (number->string val)]
	[(status) (list-pos val *trans-filter-statuses*)]
	[else (error "Can't translate reveal-trans filter option" opt)])))

  (define (shadow-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) #t]
	[(color) (hex-color-string? val)]
	[(direction) (filter-direction? val)]
	[else #f])))

  (define (shadow-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled) (bool->string val)]
	[(color) val]
	[(direction) (number->string val)]
	[else #f])))

  (define (wave-filter-options-validator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled add) #t]
	[(freq) (and (number? val)
		     (>= val 0))]
	[(light-strength phase) (exact-with-bounds? val 0 100)]
	[(strength) (exact-with-bounds? val 1 100)]
	[else #f])))

  (define (wave-filter-options-translator opt)
    (let ([name (car opt)]
	  [val (cadr opt)])
      (case name
	[(enabled add) (bool->string val)]
	[(freq) (number->string val)]
	[(light-strength phase) (number->string val)]
	[(strength) (number->string val)]
	[else (error "Can't translate wave filter option" opt)])))

  (define (filter->string filter options)
    (unless (filter? filter)
	    (error 
	     (format "Expected filter (element of '~a), got ~a" 
		     *filters* filter)))
    (for-each
     (lambda (opt)
       (unless (and (pair? opt)
		    (symbol? (car opt))
		    (eq? (length opt) 2))
	       (error (string-append
		       "Filter options must be of the form (option value), "
		       "got") opt)))
     options)
    (let* ([validators
	    `((alpha ,alpha-filter-options-validator 
		     ,alpha-filter-options-translator)
	      (blend-trans ,blend-trans-filter-options-validator
			   ,blend-trans-filter-options-translator)
	      (blur ,blur-filter-options-validator
		    ,blur-filter-options-translator)
	      (chroma ,chroma-filter-options-validator
		      ,chroma-filter-options-translator)
	      (drop-shadow ,drop-shadow-filter-options-validator
			   ,drop-shadow-filter-options-translator)
	      (flip-horizontal ,basic-filter-options-validator
			       ,basic-filter-options-translator)
	      (flip-vertical ,basic-filter-options-validator
			     ,basic-filter-options-translator)
	      (glow ,glow-filter-options-validator
		    ,glow-filter-options-translator)
	      (gray ,basic-filter-options-validator
		    ,basic-filter-options-translator)
	      (invert ,basic-filter-options-validator
		      ,basic-filter-options-translator)
	      (light ,basic-filter-options-validator
		     ,basic-filter-options-translator)
	      (mask ,mask-filter-options-validator
		    ,mask-filter-options-translator)
	      (redirect ,basic-filter-options-validator
			,basic-filter-options-translator)
	      (reveal-trans ,reveal-trans-filter-options-validator
			    ,reveal-trans-filter-options-translator)
	      (shadow ,shadow-filter-options-validator
		      ,shadow-filter-options-translator)
	      (wave ,wave-filter-options-validator
		    ,wave-filter-options-translator)
	      (x-ray ,basic-filter-options-validator
		     ,basic-filter-options-validator))]
	   [entry (assq filter validators)]
	   [validate-one (cadr entry)]
	   [translate-one (caddr entry)])
      ; validate
      (for-each
       (lambda (opt)
	 (unless (validate-one opt)
		 (error 
		  (format "Invalid ~a filter option: ~a"
			  filter opt))))
       options)
      ; translate
      (let* ([translated-options 
	      (map (lambda (opt)
		     (list 
		      (symbol->string (car opt))
		      (translate-one opt)))
		   options)]
	     [flat-string-options
	      (let loop ([opts translated-options])
		(cond
		 [(null? opts) ""]
		 [(null? (cdr opts))
		  (string-append 
		   (caar opts)
		   "="
		   (cadar opts))]
		 [else
		  (string-append 
		   (caar opts)
		   "="
		   (cadar opts)
		   ","
		   (loop (cdr opts)))]))])
      (string-append
       (symbol->string (cadr (assq filter *filters-and-official-names*))) "("  
       flat-string-options
       ")"))))

  (define (filter-opt-string->value s)
   (cond
    [(string=? s "true") #t]
    [(string=? s "false") #f]
    [(hex-digit-string? s) s]
    [(string->number s) => 
     (lambda (n) n)]
    [else
     (let ([sym (string->symbol s)])
       (cond
	[(alpha-filter-style? sym)
	 (list-pos sym *alpha-filter-styles*)]
	[(trans-filter-status? sym)
	 (list-pos sym *trans-filter-statuses*)]
	[(reveal-transition? sym)
	 (list-pos sym *reveal-transitions*)]
	[else ; guess
	 s]))]))

 (define (string->filter s)
    ; s should be of form "name(opt=val,...)"
   (string-lowercase! s)
   (let* ([filter-name
	   (regexp-match *filter-re* s)]
	  [filter-sym
	   (if filter-name
	       (let ([entry
		      (assq (string->symbol (car filter-name))
			    *official-names-and-filters*)])
		 (if entry
		     (cadr entry)
		     'empty-filter))
	       'empty-filter)]
	  [re-opt (regexp "[^,()]*=[^,()]*")]
	  [re-opt-name (regexp ".*=")]
	  [re-opt-val (regexp "=.*")])
     (if (eq? filter-sym 'empty-filter)
	 '(empty-filter)
	 (let loop ([rs (regexp-replace (car filter-name) s "")]
		    [opts '()])
	   (let ([opt-match (regexp-match re-opt rs)])
	     (if opt-match
		 (let* ([one-opt (car opt-match)]
			[opt-name
			 (let ([name 
				(car (regexp-match re-opt-name one-opt))])
			   (string->symbol 
			    (substring name 0 (sub1 (string-length name)))))]
			[opt-val
			 (let ([val 
				(car (regexp-match re-opt-val one-opt))])
			   (substring val 1 (string-length val)))])
		   (loop (regexp-replace one-opt rs "")
			 (cons (list opt-name 
				     (filter-opt-string->value
				      opt-val)) opts)))
		 (cons filter-sym (reverse opts)))))))))

