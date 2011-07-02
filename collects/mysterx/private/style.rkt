;;; style.rkt

(module style mzscheme
  (require mzlib/string)
  (require "util.rkt")
  (require "properties.rkt")

  (provide
   make-css-percentage
   css-percentage?
   css-percentage-num
   make-css-length
   css-length?
   css-length-num
   css-length-units
   font-families->string
   string->font-families
   string->font-size
   valid-css-length?
   css-length->string
   percentage-or-length?
   percentage-or-length->string
   make-bg-pos-getter	
   make-bg-pos-setter	
   make-element-getter
   make-element-setter
   make-pagebreak-getter
   make-pagebreak-setter
   list->background-position
   border-width?
   border-style->string
   border-width->string
   border->string
   border-items->string
   set-border-with-fun
   string->border-item
   make-border-getter
   make-border-style-getter
   make-border-style-setter
   make-border-width-getter
   make-border-width-setter
   string->html-color
   html-color->string
   make-color-getter
   make-color-setter
   make-css-getter
   make-css-setter
   make-const-or-css-getter-maker
   make-normal-or-css-getter
   make-auto-or-css-getter
   make-const-or-css-setter-maker
   make-normal-or-css-setter
   make-auto-or-css-setter
   parse-string
   parse-decoration
   validated-string->symbols
   string->list-style-item
   list-style-item->string
   string->background-position
   string->margin
   margin->string
   string->padding
   padding->string
   url->string
   string->url
   clip-rect?
   clip-rect->symbols)

  (define-struct css-percentage (num))
  (define-struct css-length (num units))

  (define font-family->string
    (lambda (ff)
      (if (regexp-match ".* .*" ff)  ; contains a space
	  (string-append "\"" ff "\"")
	  ff)))

  (define font-families->string
    (lambda (ffs)
      (let loop ([ffs ffs])
	(cond 
	 [(null? ffs) 
	  ""]
	 [(null? (cdr ffs))
	  (font-family->string (car ffs))]
	 [else 
	  (string-append (font-family->string (car ffs))
			 ","
			 (loop (cdr ffs)))]))))

   (define string->font-families
    (lambda (s)
      (let ([lst (string->list s)]
	    [build-curr 
	     (lambda (cs)
	       (list->string (reverse (remove-ws cs))))])
	(let loop ([lst lst]
		   [curr '()])
          (cond
	   [(null? lst) 
	    (if (null? curr)
		'()
		(list (build-curr curr)))]
	   [(char-ci=? #\, (car lst))
            ; strip leading whitespace
	    ; start on new current word
	    (let ([tail (loop (remove-ws (cdr lst)) '())])
	      (if (null? curr)
		  tail
		  (cons (build-curr curr) tail)))]
	   [(member (car lst) '(#\" #\')) ; strip quotes
	    (loop (cdr lst) curr)]
	   [else
	    (loop (cdr lst) (cons (car lst) curr))])))))

  (define (string->font-size s)
    (let ([sym (string->symbol s)])
      (cond
       [(font-size? sym) sym]
       [(parse-css-length (string->list s)) => 
	(lambda (val-rest) (car val-rest))] 
       [else s])))

   (define (valid-css-length? elt)
     (and (css-length? elt)
	  (exact? (css-length-num elt))
	  (css-unit? (css-length-units elt))))

   (define (css-length->string elt)
     (string-append (number->string (css-length-num elt))
		    (symbol->string (css-length-units elt))))

   (define percentage-or-length?
     (lambda (elt)
	(or (and (css-percentage? elt)
		 (exact? (css-percentage-num elt)))
	    (and (valid-css-length? elt)))))

   (define percentage-or-length->string
     (lambda (elt)
       (cond
	[(css-length? elt)
	 (css-length->string elt)]
	[(css-percentage? elt)
	 (string-append (number->string (css-percentage-num elt)) "%")]
	[else
	 (error "Not a CSS percentage or length: ~a" elt)])))

   (define (make-bg-pos-getter elt f fname)
     (lambda ()
       (let ([pos (f elt)])
	 (when (empty-string? pos)
	       (empty-property-error fname))
	    (car (list->background-position (string->list pos))))))

   (define (make-bg-pos-setter elt f pred? elts coord) 
     (lambda (pos)
       (cond
	[(pred? pos)
	 (f elt (symbol->string pos))]
	[(percentage-or-length? pos)
	 (f elt (percentage-or-length->string pos))]
	[else 
	 (error 
	  (format
	   (string-append 
	    "set-background-position-~a!: "
	    "Expected value in ~a, "
	    "CSS length, or CSS percentage, "
	    "got ~a") 
	   coord elts pos))])))

   (define (make-element-getter elt getter name)
     (lambda ()
       (let ([s (getter elt)])
	 (if (empty-string? s)
	     (empty-property-error name)
	     (string->symbol s)))))

   (define (make-element-setter elt pred? props f!)
     (lambda (s)
       (unless (pred? s)
	       (error 
		(format "Expected element of ~a, got: ~a"
			props s)))
       (f! elt (symbol->string s))))

   (define (make-pagebreak-getter elt f)       
     (lambda ()
       (let ([s (f elt)])
	 (if (empty-string? s)
	     'none
	     (string->symbol s)))))

   (define (make-pagebreak-setter elt f! name) 
     (lambda (s)
       (unless (pagebreak? s)
	       (error
		(format "~a: Expected element of ~a, got: ~a"
			name *page-breaks* s)))
       (let ([str (if (eq? s 'none)
		      ""
		      (symbol->string s))])
	 (f! elt str))))

   (define (border-width? elt)
     (or (memq elt *border-widths*)
	 (valid-css-length? elt)))

   (define border-style->string symbol->string)

   (define (border-width->string elt)
     (if (css-length? elt)
	 (css-length->string elt)
	 (symbol->string elt)))

   (define (border->string elt)
     (cond 
      [(border-width? elt)
       (border-width->string elt)]
      [(border-style? elt)
       (border-style->string elt)]
      [(html-color? elt)
       (html-color->string elt)]))

   (define (border-items->string elts)
     (fold-strings-with-spaces (map border->string elts)))

   (define (set-border-with-fun elt cs f) 
     (for-each
      (lambda (c)
	(unless (or (border-width? c)
		    (border-style? c)
		    (html-color? c))
		(error 
		 (format 
		  (string-append 
		   "set-border!: expected nonempty list where each "
		   "element is either "
		   "a border width (one of '~a, or a CSS length), " 
		   "a border style (one of '~a, or "
		   "an HTML color, got ~a")
		  *border-widths*
		  *border-styles*
		  c))))
      cs)
     (f elt (border-items->string cs)))

   (define (string->border-item s)
     (let ([sym (string->symbol s)])
       (cond
       ;color
	[(memq sym *html-colors*)
	 sym]
	[(hex-digit-string? s)
	 s]
       ;style
	[(memq sym *border-styles*)
	 sym]
       ;width
	[(memq sym *border-widths*)
	 sym]
	[(parse-css-length (string->list s)) => car]
       ;error
	[else 
	 (error (format "Expected border item, got: ~a" s))])))

   (define (string->border-list s)
     (map string->border-item (parse-string s)))

   (define (make-border-getter elt f name)
     (lambda ()
       (let ([s (f elt)])
	 (if (empty-string? s)
	     (empty-property-error name)
	     (string->border-list s)))))

   (define (make-border-style-getter elt f name)
     (lambda ()
       (let ([s (f elt)])
	 (if (empty-string? s)
	     (empty-property-error name)
	     (string->border-item s)))))
     
   (define (make-border-style-setter elt f name)
     (lambda (s)
       (unless (border-style? s)
	       (error 
		(format "~a: Expected element of ~a, got ~a"
			name *border-styles* s)))
       (f elt (border-style->string s))))

   (define (make-border-width-getter elt f name)
     (lambda ()
       (let ([s (f elt)])
	 (if (empty-string? s)
	     (empty-property-error name)
	     (string->border-item s)))))
     
   (define (make-border-width-setter elt f name)
     (lambda (s)
       (unless (border-width? s)
	       (error 
		(format "~a: Expected element of ~a or CSS length, got ~a"
			name *border-widths* s)))
       (f elt (border->string s))))

   (define (string->html-color s)
     (if (char=? (string-ref s 0) #\#)
	 s
	 (string->symbol s)))

   (define (html-color->string s)
     (if (symbol? s)
	 (symbol->string s)
	 s))

   (define (make-color-getter elt f name)
     (lambda ()
       (let ([s (f elt)])
	 (if (empty-string? s)
	     (empty-property-error name)
	     (string->html-color s)))))

   (define (make-color-setter elt f name)
     (lambda (s)
       (unless 
	(html-color? s)
	(error 
	 (format "~a: Expected HTML color, got: ~a"
		 name s)))
       (f elt (html-color->string s))))

  (define (make-css-getter elt f fname)
    (lambda ()
      (let ([s (f elt)])
	(when (empty-string? s)
	      (empty-property-error fname))
        (cond
	 [(parse-css-length (string->list s)) => car]
	 [else s]))))

  (define (make-css-setter elt f fname)
    (lambda (css)
      (let ([s (cond
		[(percentage-or-length? css)
		 (percentage-or-length->string css)]
		[else 
		 (error 
		  (string-append 
		   fname ": Expected "
		   "CSS length or percentage, got")
		  css)])])
	(f elt s))))

  (define (make-const-or-css-getter-maker c)
    (lambda (elt f fname)
      (lambda ()
	(let ([s (f elt)])
	  (when (empty-string? s)
		(empty-property-error fname))
	     (cond 
	      [(string=? (symbol->string c) s) c] 
	      [(parse-css-length (string->list s)) => car]
	      [else s])))))

  (define make-normal-or-css-getter
    (make-const-or-css-getter-maker 'normal))

  (define make-auto-or-css-getter 
    (make-const-or-css-getter-maker 'auto))

  (define (make-const-or-css-setter-maker c)
    (lambda (elt f fname)
      (lambda (v)
	(let ([s (cond
		  [(eq? v c) (symbol->string c)]
		  [(percentage-or-length? v)
		   (percentage-or-length->string v)]
		  [else 
		   (error 
		    (string-append 
		     fname ": Expected 'normal, "
		     "CSS length or percentage, got")
		    v)])])
	  (f elt s)))))

  (define make-normal-or-css-setter
    (make-const-or-css-setter-maker 'normal))

  (define make-auto-or-css-setter 
    (make-const-or-css-setter-maker 'auto))

  (define (html-color? s)
    (or (hex-color-string? s)
	(memq s *html-colors*)))

  (define parse-number ; returns number, rest of list
    (lambda (lst)
      (let loop ([num? #f]
		 [seen-dot #f]
		 [digits '()]
		 [lst lst])
	(let ([c (car lst)])
	  (cond
	   [(char-numeric? c) 
	    (loop #t seen-dot (cons c digits) (cdr lst))]
	   [(eq? c #\.)
	    (if seen-dot
		(error "More than one period in number")
		(loop #t #t (cons c digits) (cdr lst)))]
	   [else
	    (if num? 
		(cons (string->number 
		       (list->string (reverse digits))) lst)
		#f)])))))

   ; (listof char) -> (cons symbol (listof char))
   (define parse-units ; returns unit symbol, rest of list
     (lambda (lst)
       (let* ([sym-rest
	       (let loop ([word '()]
			  [lst lst])
		 (cond 
		  [(or (null? lst) (char-whitespace? (car lst)))
		   (cons (string->symbol (list->string (reverse word)))
			 lst)]
		  [else
		   (loop (cons (car lst) word) (cdr lst))]))]
	      [sym (car sym-rest)]
	      [rst (cdr sym-rest)])
	 (if (or (eq? sym '%) (css-unit? sym))
	     (cons sym rst)
	     (error "Unable to parse units")))))

   ; (listof symbols) -> (listof char) -> (union #f (listof symbol))
   (define make-words-parser 
     (lambda (syms)
       (lambda (lst)
	 (let loop ([word '()]
		    [lst lst])
	   (cond 
	    [(or (null? lst)
		 (char-whitespace? (car lst)))
	     (let ([sym (string->symbol (list->string (reverse word)))])
	       (if (memq sym syms)
		   (cons sym lst)
		   #f))]
	    [else
	     (loop (cons (car lst) word) (cdr lst))])))))

   (define parse-horizontal 
     (make-words-parser *horizontals*))

   (define parse-vertical
     (make-words-parser *verticals*))

   (define parse-string ; string -> (listof string)
     (lambda (s)
       (let ([tack-on-word 
	      (lambda (word words)
		(if (null? word)
		    words
		    (cons (list->string (reverse word)) words)))])
	 (let loop ([word '()]
		    [words '()]
		    [lst (string->list s)])
	   (cond 
	    [(null? lst)
	     (reverse (tack-on-word word words))]
	    [(char-whitespace? (car lst))
	     (loop '() (tack-on-word word words) (cdr lst))]
	    [else
	     (loop (cons (car lst) word) words (cdr lst))])))))

   (define (parse-css-length lst)
     (cond 
      [(parse-number lst) =>
       (lambda (num-rest)
	 (let ([num (car num-rest)]
	       [rst (cdr num-rest)])
	   (cond 
	    [(parse-units rst) =>
	     (lambda (unit-rest)
	       (let* ([units (car unit-rest)]
		      [val (if (eq? units '%)
			       (make-css-percentage num)
			       (make-css-length num units))])
		 (cons val (cdr unit-rest))))]
	    [else #f])))]
      [else #f]))

   (define parse-decoration
     (make-words-parser *decorations*))

   (define validated-string->symbols
     (lambda (s funname parser)
       (if (empty-string? s)
	   (empty-property-error funname)
	   (let ([lst (string->list s)])
	     (let loop ([lst lst]
			[words '()])
	       (let ([word-rest (parser lst)])
		 (if word-rest
		     (loop (remove-ws (cdr word-rest)) 
			   (cons (car word-rest) words))
		     (reverse words))))))))

   (define (string->list-style-item s)
     (if (string-ci=? (substring s 0 4) "url(")
	 (url->string s)
	 (let ([sym (string->symbol s)])
	   (if (or (list-style-type? sym)
		   (list-style-position? sym))
	       sym
	       (error (format "Expected list-style item, got: ~a"
			      s))))))

  (define (list-style-item->string s) 
     (cond
      [(or (list-style-type? s)
	   (list-style-position? s))
       (symbol->string s)]
      [(string? s)
       (string->url s)]
      [else 
       (error 
	(format "Expected list-style item, got: ~a"))]))

   (define string->background-position
     (lambda (s)
       (let ([bp (list->background-position (string->list s))])
	 (case (length bp)
	   [(1) (car bp)]
	   [(2) bp]
	   [else "Invalid background position"]))))

   (define list->background-position
     (lambda (lst)
       (cond
	[(null? lst) '()]
	[(parse-css-length lst) =>
	 (lambda (val-rest)
	   (cons (car val-rest)
		 (list->background-position 
		  (remove-ws (cdr val-rest)))))]
	[(parse-horizontal lst) =>
	 (lambda (x-rest)
	   (cons (car x-rest)
		 (list->background-position (remove-ws (cdr x-rest)))))]
	[(parse-vertical lst) =>
	 (lambda (y-rest)
	   (cons (car y-rest)
		 (list->background-position (remove-ws (cdr y-rest)))))]
	[else
	 (error "Can't parse background-position")])))

  (define (make-css-parser loop lst)
    (lambda (num-rest)
      (let ([num (car num-rest)]
	    [rst (cdr num-rest)])
	(let* ([unit-rest (parse-units rst)]
	       [units (car unit-rest)]
	       [the-val (if (eq? units '%)
			    (make-css-percentage num)
			    (make-css-length num units))])
	  (cons the-val (loop (cdr lst)))))))

  (define string->margin
     (lambda (s)
       (let loop ([lst (parse-string s)])
	 (when (> (length lst) 4)
	       (error "Only four margin values allowed, got" s))
	 (cond
	  [(null? lst)
	   '()]
	  [(string=? (car lst) "auto")
	   (cons 'auto (loop (cdr lst)))]
	  [(parse-number (string->list (car lst))) =>
	   (make-css-parser loop lst)]
	  [else
	   (error (string-append 
		   "Expected margin string with up to four of "
		   "CSS length, CSS percentage, or auto.  Got")
		   s)]))))

  (define string->padding
     (lambda (s)
       (let loop ([lst (parse-string s)])
	 (when (> (length lst) 4)
	       (error "Only four padding values allowed, got" s))
	 (cond
	  [(null? lst)
	   '()]
	  [(parse-number (string->list (car lst))) =>
	   (make-css-parser loop lst)]
	  [else
	   (error (string-append 
		   "Expected padding string with up to four of "
		   "CSS lengths or CSS percentages.  Got")
		   s)]))))

  (define string->percentage-or-length
    (lambda (s)
      (let ([lst (string->list s)])
	  (cond
	   [(parse-number (string->list (car lst))) =>
	    (lambda (num-rest)
	      (let ([num (car num-rest)]
		    [rst (cdr num-rest)])
		(let* ([unit-rest (parse-units rst)]
		       [units (car unit-rest)]
		       [the-val (if (eq? units '%)
				    (make-css-percentage num)
				    (make-css-length num units))])
		 the-val)))]
	   [else
	    (error "Expected string with percentage or length, got ~a"
		   s)]))))

  (define margin-item->string
    (lambda (item)
      (cond 
       [(eq? item 'auto)
	"auto"]
       [else 
	(percentage-or-length->string item)])))

  (define margin->string
    (map-to-string margin-item->string)) 

  (define padding->string
    (map-to-string percentage-or-length->string))

  (define (url->string s)
    ; "url(foo)" -> "foo"
    (substring s 4 (sub1 (string-length s))))

  (define (string->url s)
    ; ""foo" -> url(foo)"
    (string-append "url(" s ")"))

  (define (clip-rect? s)
    (string-ci=? 
     (substring s 0 5) "rect("))

  (define (clip-rect->symbols s)
    (map (lambda (s)
	   (if (string=? s "auto") 
	       'auto
	       (car (parse-css-length (string->list s)))))
	 (parse-string (substring s 5 (sub1 (string-length s)))))))


