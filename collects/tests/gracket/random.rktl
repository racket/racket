
; (require-library "errortrace.ss" "errortrace")
(require mzlib/list
	 mzlib/etc
	 mzlib/class100
	 mzlib/defmacro)

(define example-list%
  (class100 object% (-name-in -parents [-filter (lambda (x) (not (void? x)))])
    (private-field
       [name-in -name-in]
       [parents -parents]
       [filter -filter]
       [name name-in]
       [items '()]
       [num-items 0]
       [baddies null]
       
       [parents-count 
	(if parents
	    (map (lambda (parent)
		   (send parent get-count))
		 parents)
	    '())]
       [parents-choose
	(if parents
	    (map (lambda (parent)
		   (send parent get-choose-example))
		 parents)
	    '())])
      (public
       [get-count (lambda () (lambda () (count)))]
       [get-name (lambda () name)]
       [get-choose-example (lambda () (opt-lambda ([which #f]) (choose-example which)))]
       [choose-parent-example
	(lambda (which)
	  (let loop ([pos which][counts parents-count][chooses parents-choose])
	    (if (null? counts)
		(void)
		(let ([c ((car counts))])
		  (if (< pos c)
		      ((car chooses) pos)
		      (loop (- pos c) (cdr counts) (cdr chooses)))))))]

       [count 
	(lambda () (+ num-items (apply + (map (lambda (x) (x)) parents-count))))]
       [set-filter
	(lambda (f)
	  (set! filter f))])
      (private-field
       [prepare values])
      (public
       [set-prepare
	(lambda (f)
	  (set! prepare f))]
       [add
	(lambda (x)
	  (if (filter x)
	      (begin
		(set! num-items (add1 num-items))
		(set! items (cons x items)))
	      (error 'add "rejected: ~a in: ~a" x name)))]
       [all-examples
	(lambda ()
	  (apply append items (map (lambda (p) (send p all-examples)) parents)))]
       [choose-example
	(opt-lambda ([which #f])
	  (let ([n (if which 
		       which
		       (let ([c (count)])
			 (if (zero? c)
			     0
			     (random c))))])
	    (if (< n num-items)
		(prepare (list-ref items n))
		(choose-parent-example (- n num-items)))))]
       [add-bad
	(lambda (i)
	  (set! baddies (cons i baddies)))]
       [bad-examples
	(lambda () baddies)])
      (sequence (super-init))))

(define boxed-example-list%
  (class100 object% (-parent)
    (private-field [parent -parent])
    (public
     [get-name (lambda () `(boxed ,(send parent get-name)))]
     [all-examples
      (lambda ()
	(let ([l (map box (send parent all-examples))])
	  l))]
     [choose-example
      (opt-lambda ([which #f])
	(let ([ex (send parent choose-example)])
	  (if (void? ex)
	      (void)
	      (box ex))))]
     [bad-examples
      (lambda () (cons 5 (map box (send parent bad-examples))))])
    (sequence (super-init))))

(define listed-example-list%
  (class100 object% (-parent)
    (private-field [parent -parent])
    (public
     [get-name (lambda () `(listed ,(send parent get-name)))]
     [all-examples
      (lambda ()
	(let ([l (map list (send parent all-examples))])
	  l))]
     [add
      (lambda (v)
	(unless (list? v)
	  (error 'add "rejected: ~a in: ~a" v name))
	(for-each
	 (lambda (i)
	   (send parent add i))
	 v))]
     [choose-example
      (opt-lambda ([which #f])
	(let ([ex (send parent choose-example)])
	  (if (void? ex)
	      (void)
	      (list ex))))]
     [bad-examples
      (lambda ()
	(cons 5 (map list (send parent bad-examples))))])
    (sequence (super-init))))

(define optional-example-list%
  (class100 object% (-parent -val)
    (private-field [parent -parent][val -val])
    (public
     [get-name (lambda () `(optional ,(send parent get-name)))]
     [all-examples
      (lambda ()
	(let ([l (map box (send parent all-examples))])
	  (cons val l)))]
     [add
      (lambda (x)
	(and x (send parent add x)))]
     [choose-example
      (opt-lambda ([which #f])
	(if (zero? (random 2))
	    val
	    (send parent choose-example)))]
     [bad-examples
      (lambda () (cons #t (send parent bad-examples)))])
    (sequence (super-init))))

(define choose-example-list%
  (class100 object% (-parents)
    (private-field [parents -parents])
    (public
     [get-name (lambda () `(choose ,(map (lambda (p) (send p get-name)) parents)))]
     [all-examples
      (lambda ()
	(apply append (map (lambda (p) (send p all-examples)) parents)))]
     [add (lambda (x) (void))]
     [choose-example
      (opt-lambda ([which #f])
	(send (list-ref parents (random (length parents)))
	      choose-example which))]
     [bad-examples
      (lambda () null)])
    (sequence (super-init))))

(define unknown-example-list%
  (class100 object% (-who)
    (private-field [who -who])
    (public
     [get-name (lambda () `(unknown ,who))]
     [all-examples (lambda () null)]
     [add (lambda (x) (void))]
     [choose-example
      (opt-lambda ([which #f])
	(format "[dummy for ~a]" (get-name)))]
     [bad-examples
      (lambda () null)])
    (sequence (super-init))))

(define discrete-example-list%
  (class100 object% (-vals)
    (private-field [vals -vals])
    (public
     [get-name (lambda () `(one-of ,@vals))]
     [all-examples (lambda () vals)]
     [add (lambda (x) (unless (member x vals)
			(error '|add in discrete-example-list|
				 "no good: ~a" x)))]
     [choose-example
      (opt-lambda ([which #f])
	(list-ref vals (random (length vals))))]
     [bad-examples
      (lambda ()
	(if (member 'bad-example-symbol vals)
	    null
	    (list 'bad-example-symbol)))])
    (sequence (super-init))))

(define number-example-list%
  (class100 object% (-parent -start -end)
    (private-field [parent -parent]
		   [start -start]
		   [end -end])
    (public
      [get-name (lambda () `(number in ,start ,end))]
      [all-examples
       (lambda ()
	 (filter (lambda (x) (ok x)) (send parent all-examples)))]
      [ok (lambda (v) (<= start v end))]
      [add (lambda (v)
	     (send parent add v)
	     (unless (ok v)
	       (error 'add "rejected (late): ~a in: ~a" v name)))]
      [choose-example
       (opt-lambda ([which #f])
	 (let loop ()
	   (let ([v (send parent choose-example which)])
	     (if (ok v)
		 v
		 (loop)))))]
      [bad-examples
       (lambda ()
	 (list* (sub1 start)
		(if (= (add1 end) end)
		    (- start 2)
		    (add1 end))
		(send parent bad-examples)))])
    (sequence (super-init))))


(define maker-example-list%
  (class100 object% (-maker)
    (private-field [maker -maker])
    (public
     [get-name (lambda () `(make ,maker))]
     [all-examples
      (lambda ()
	(list (maker)))]
     [add (lambda (x) (void))]
     [choose-example
      (opt-lambda ([which #f])
	(maker))]
     [bad-examples
      (lambda () null)])
    (sequence (super-init))))

(define-struct (fatal-exn exn) ())

(define (fatal-error name str . args)
  (raise (make-fatal-exn (apply format (string-append "~a: " str) name args)
			 ((debug-info-handler)))))

(define trying-class #f)
(define trying-method #f)

(define null-results null)

(define-macro define-main 
  (lambda list
    (let loop ([l list][rest '()])
      (if (null? l)
	  (cons 'begin rest)
	  (loop (cdr l)
		(let* ([first (car l)]
		       [name (if (symbol? first)
				 first
				 (car first))]
		       [strname (symbol->string name)]
		       [bases (if (symbol? first)
				  ()
				  (cdr first))]
		       [el-name (lambda (s)
				  (if s
				      (string->symbol
				       (string-append
					(symbol->string s)
					"-example-list"))
				      #f))])
		  (append
		   `((define ,(el-name name)
		       (make-object example-list% 
				    ',name
				    (list ,@(map el-name bases))
				    (lambda (v) (when (null? v)
						  (set! null-results (cons (list trying-class trying-method ',name)
									   null-results))
						  (error ',name "got null"))))))
		   (if (or (regexp-match "%$" strname) (regexp-match "<%>$" strname))
		       `((send ,(el-name name) set-filter (lambda (x) (is-a? x ,name)))
			 (send ,(el-name name) add-bad 5))
		       null)
		   rest)))))))

(define-main
  void
  (value char real string-list subarea<%>)
  char
  ubyte
  integer
  integer-list
  symbol
  real
  real-list
  string
  string-list
  mutable-string
  bytes
  path
  labelstring
  labelstring-list
  boolean
  procedure
  eventspace
  container-alignment

  (area<%> window<%> subarea<%> area-container<%>)

  (subarea<%> subwindow<%> pane%)

  (window<%> subwindow<%> area-container-window<%>)

  (area-container<%> area-container-window<%> pane%)

  (subwindow<%> control<%> canvas<%> panel%)

  (area-container-window<%> top-level-window<%> panel%)

  (control<%> message% button% check-box% slider% gauge% text-field% combo-field% radio-box% list-control<%>)

  (list-control<%> choice% list-box%)

  (top-level-window<%> frame% dialog%)

  (pane% horizontal-pane% vertical-pane% grow-box-spacer-pane%)
  
  (panel% horizontal-panel% vertical-panel%)

  (canvas<%> canvas% editor-canvas%)

  message%
  button%
  check-box%
  slider%
  gauge%
  text-field%
  combo-field%
  radio-box%

  choice%
  list-box%

  canvas%
  editor-canvas%

  horizontal-pane%
  vertical-pane%
  grow-box-spacer-pane%

  horizontal-panel%
  (vertical-panel% tab-panel% group-box-panel%)
  tab-panel%
  group-box-panel%

  frame%
  dialog%

  point%

  ps-setup%
  gl-config%
  gl-context<%>

  color%
  font%
  brush%
  pen%
  region%
  dc-path%

  font-list%
  pen-list%
  brush-list%
  color-database<%>
  font-name-directory<%>

  cursor%
  bitmap%

  (event% control-event% scroll-event% mouse-event% key-event%)
  control-event%
  scroll-event%
  mouse-event%
  key-event%

  (dc<%> bitmap-dc% post-script-dc% printer-dc%)
  bitmap-dc%
  post-script-dc%
  printer-dc%
  
  (menu-item-container<%> menu% menu-bar% popup-menu%)

  popup-menu%
  menu-bar%
  
  (menu-item<%> separator-menu-item%  labelled-menu-item<%>)
  (labelled-menu-item<%> selectable-menu-item<%> menu%)
  (selectable-menu-item<%> menu-item% checkable-menu-item%)
  separator-menu-item%
  menu-item%
  checkable-menu-item%

  menu%

  timer%

  add-color<%>
  mult-color<%>
  style-delta%
  style<%>
  style-list%

  (editor-admin%  editor-snip-editor-admin<%>)
  editor-snip-editor-admin<%>
  snip-admin%
  
  (editor<%> text% pasteboard%)
  text%
  pasteboard%

  (snip% string-snip% image-snip% editor-snip% readable-snip<%>)
  (string-snip% tab-snip%)
  tab-snip%
  image-snip%
  editor-snip%
  readable-snip<%>

  snip-class%
  snip-class-list<%>

  editor-data%
  editor-data-class%
  editor-data-class-list<%>

  keymap%
  editor-wordbreak-map%

  (editor-stream-in-base% editor-stream-in-bytes-base%)
  (editor-stream-out-base% editor-stream-out-bytes-base%)

  editor-stream-in-bytes-base%
  editor-stream-out-bytes-base%

  editor-stream-in%
  editor-stream-out%
  
  clipboard<%>
  clipboard-client%)

(send bitmap%-example-list set-filter (lambda (bm) (send bm ok?)))

; Avoid stuck states in random testing:
(send frame%-example-list set-prepare (lambda (w) (send w enable #t) w))
(send dialog%-example-list set-prepare (lambda (w) (send w enable #t) w))

(send boolean-example-list set-filter boolean?)
(send char-example-list set-filter char?)
(send string-example-list set-filter string?)
(send bytes-example-list set-filter bytes?)
(send mutable-string-example-list set-filter (lambda (x) (and (string? x) (not (immutable? x)))))
(send labelstring-example-list set-filter (lambda (x) (and (string? x) ((string-length x) . <= . 200))))
(send path-example-list set-filter path?)
(send symbol-example-list set-filter symbol?)
(send real-example-list set-filter real?)
(send integer-example-list set-filter (lambda (x) (and (number? x) (exact? x) (integer? x))))
(send integer-list-example-list set-filter (lambda (x) (and (list? x) (andmap (lambda (x) (and (number? x) (exact? x) (integer? x))) x))))
(send real-list-example-list set-filter (lambda (x) (and (list? x) (andmap (lambda (x) (and (number? x) (real? x))) x))))

(define false-example-list (make-object example-list% 'false '()))
(send false-example-list add #f)
(send false-example-list add-bad #t)

(send char-example-list add-bad 'not-a-char)
(send string-example-list add-bad 'not-a-string)
(send mutable-string-example-list add-bad 'not-a-string)
(send bytes-example-list add-bad 'not-a-bytes)
(send path-example-list add-bad 'not-a-path)
(send labelstring-example-list add-bad 'not-a-string)
(send labelstring-example-list add-bad (make-string 255 #\x))
(send symbol-example-list add-bad "not a symbol")
(send real-example-list add-bad 4+5i) 
(send integer-example-list add-bad 5.0)
(send integer-list-example-list add-bad 7)
(send real-list-example-list add-bad 7.0)

(for-each (lambda (h)
	    (for-each (lambda (v)
			(send container-alignment-example-list add (list h v)))
		      '(top center bottom)))
	  '(left center right))

(define input-port-example-list (make-object maker-example-list%
					     (lambda () 
					       (open-input-string (send string-example-list choose-example)))))

(define empty-list-example-list (make-object example-list% 'empty-list '()))
(send empty-list-example-list add null)
(send empty-list-example-list add-bad #f)

(send* boolean-example-list
       (add #t)
       (add #f))

(send* integer-example-list
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add -1)
       (add -2)
       (add -3)
       (add -1000)
       (add 1)
       (add 2)
       (add 3)
       (add 4)
       (add 5)
       (add 6)
       (add 7)
       (add 8)
       (add 9)
       (add 10)
       (add 16)
       (add 32)
       (add 64)
       (add 128)
       (add 256)
       (add 255)
       (add 1023)
       (add 1000))

(send* real-example-list
       (add 0.0) (add 0.0)
       (add -1.0)
       (add -2.0)
       (add -1000.0)
       (add 1.0)
       (add 2.0)
       (add 256.0)
       (add +inf.0)
       (add -inf.0)
       (add 2/3)
       (add -100/9))

(define non-negative-integer-example-list (make-object number-example-list% integer-example-list 0 +inf.0))
(define positive-integer-example-list (make-object number-example-list% integer-example-list 1 +inf.0))

(define non-negative-real-example-list (make-object number-example-list% real-example-list 0 +inf.0))
(define positive-integer-example-list (make-object number-example-list% real-example-list 1e-200 +inf.0))

(define (range-integer-example-list s e)
  (make-object number-example-list% integer-example-list s e))

(define (range-real-example-list s e)
  (make-object number-example-list% real-example-list s e))

(send* symbol-example-list
       (add 'ok) (add 'change-family))

(send* string-list-example-list
       (add '("apple" "banana" "coconut")))

(send* labelstring-list-example-list
       (add '("apple" "banana" "coconut")))

(send* char-example-list
       (add #\nul)
       (add #\a)
       (add #\1)
       (add #\newline)
       (add #\tab)
       (add #\z)
       (add #\C))

(send* real-example-list
       (add 0.)
       (add 0.)
       (add 0.)
       (add -1.)
       (add -2.)
       (add -3.)
       (add -1000.)
       (add 1.)
       (add 2.)
       (add 3.)
       (add 1000.)
       (add 5))

(send* string-example-list
       (add "")
       (add "hello")
       (add "random/mred.xbm")
       (add "random/mred.bmp")
       (add "mred.gif")
       (add "goodbye adious see you later zai jian seeya bye-bye"))

(send* mutable-string-example-list
       (add (make-string 10 #\x))
       (add (make-string 10 #\nul)))

(send* bytes-example-list
       (add #"")
       (add #"hello")
       (add #"random/mred.xbm")
       (add #"random/mred.bmp")
       (add #"mred.gif")
       (add #"goodbye adious see you later zai jian seeya bye-bye"))

(send* labelstring-example-list
       (add "")
       (add "hello")
       (add "goodbye adious see you later zai jian seeya bye-bye"))

(send* path-example-list
       (add (string->path "hello"))
       (add (string->path "random/mred.xbm"))
       (add (string->path "random/mred.bmp"))
       (add (string->path "mred.gif")))


(send procedure-example-list add void)

(define classinfo (make-hash-table))

(define (add-all-combinations example-list items)
  (for-each
   (lambda (i) (send example-list add i))
   (let loop ([items items])
     (cond
      [(null? (cdr items)) items]
      [else (let ([l (loop (cdr items))])
	      (append
	       (map (lambda (x) (bitwise-ior (car items) x)) l)
	       l))]))))

(define (optional v l) (make-object optional-example-list% l v))
(define (boxed l) (make-object boxed-example-list% l))
(define (unknown s) (make-object unknown-example-list% s))
(define (choice . l) (make-object choose-example-list% l))
(define (style-list . l) (make-object listed-example-list% (make-object discrete-example-list% l)))
(define (symbol-in l) (make-object discrete-example-list% l))

(define message-label-example-list (choice labelstring-example-list bitmap%-example-list (symbol-in '(app caution stop))))

(load-relative "windowing-classes.ss")
(load-relative "drawing-classes.ss")
(load-relative "editor-classes.ss")

(define (get-args l)
  (let/ec bad
    (let loop ([l l])
      (if (null? l)
	  '()
	  (let* ([source (car l)]
		 [value (send source choose-example #f)])
	    (if (void? value)
		(bad (format "no examples: ~a" (send source get-name)))
		(cons value (loop (cdr l)))))))))

(define (get-all-args l)
  (let loop ([l l])
    (if (null? l)
	'()
	(let* ([source (car l)]
	       [values (send source all-examples)]
	       [rest (loop (cdr l))])
	  (if (null? (cdr l))
	      (list values)
	      (apply append
		     (map (lambda (other)
			    (map (lambda (v) (cons v other)) values))
			  rest)))))))

(define-struct posargs (good bads))

(define (get-bad-args l)
  (let/ec bad
    (let loop ([l l])
      (if (null? l)
	  '()
	  (let* ([source (car l)]
		 [good (send source choose-example #f)]
		 [bads (send source bad-examples)])
	    (if (void? good)
		(bad (format "no examples: ~a" (send source get-name)))
		(cons (make-posargs good bads) (loop (cdr l)))))))))

(define thread-output-port current-output-port)

(define print-only? #f)

(define (apply-args v dest name k)
  (if (list? v)
      (begin
	(fprintf (thread-output-port) "~a: ~s" name v)
	(flush-output (thread-output-port))
	(if print-only?
	    (newline)
	    (with-handlers (((lambda (x) (not (fatal-exn? x)))
			     (lambda (x)
			       (fprintf (thread-output-port)
					": error: ~a\n"
					(exn-message x)))))
	      (if (eq? dest 'values)
		  (k v)
		  (send dest add (k v)))
	      (flush-display)
	      (fprintf (thread-output-port) ": success\n"))))
      (fprintf (thread-output-port) "~a: failure: ~a\n" name v)))

(define (try-args arg-types dest name k)
  (apply-args (get-args arg-types) dest name k))

(define (try-all-args arg-types dest name k)
  (let ([vs (get-all-args arg-types)])
    (for-each (lambda (v)
		(apply-args v dest name k))
	      vs)))

(define (apply-bad-args v dest name k bad)
  (fprintf (thread-output-port) "~a: ~s" name v)
  (flush-output (thread-output-port))
  (with-handlers ([exn:fail:contract?
		   (lambda (x)
		     (fprintf (thread-output-port) ": exn: ~a\n"
			      (exn-message x))
		     ;; Check that exn is from the right place:
		     (let ([class (if (list? name) 
				      (let ([n (car name)])
					(if (symbol? n)
					    n
					    '|.|))
				      name)]
			   [method (if (list? name) (cadr name) 'initialization)])
		       (when (eq? method 'initialization)
			 ; init is never inherited, so class name really should be present
			 (unless (regexp-match (symbol->string class) (exn-message x))
			   (fprintf (thread-output-port) 
				    "  NO OCCURRENCE of class name ~a in the error message\n"
				    class)))
		       (unless (regexp-match (symbol->string method) (exn-message x))
			 (fprintf (thread-output-port) 
				  "  NO OCCURRENCE of method ~a in the error message\n"
				  method))))]
		  [exn:fail:contract:arity?
		   (lambda (x)
		     (fprintf (thread-output-port)
			      ": UNEXPECTED ARITY MISMATCH: ~a\n"
			      (exn-message x)))]
		  [(lambda (x) (not (fatal-exn? x)))
		   (lambda (x)
		     (fprintf (thread-output-port)
			      ": WRONG EXN TYPE: ~a\n"
			      (exn-message x)))])
    (k v)
    (flush-display)
    (fprintf (thread-output-port) ": NO EXN RAISED\n")))

(define (try-bad-args arg-types dest name k)
  (let ([args (get-bad-args arg-types)])
    (cond
     [(not (list? args)) (fprintf (thread-output-port) "~a: failure in bad-testing: ~a\n" name args)]
     [else
      (let loop ([pres null][posts args])
	(unless (null? posts)
	  (for-each
	   (lambda (bad)
	     (apply-bad-args (append
			      (map posargs-good pres)
			      (list bad)
			      (map posargs-good (cdr posts)))
			     dest name k bad))
	   (posargs-bads (car posts)))
	  (loop (append pres (list (car posts))) (cdr posts))))])))

(define (create-some cls try)
  (when (class? cls)
	(let* ([v (hash-table-get classinfo cls)]
	       [dest (car v)]
	       [name (cadr v)]
	       [creators (caddr v)])
	  (let loop ([l creators])
	    (unless (null? l)
		    (try (car l) dest name
			 (lambda (v)
			   (apply make-object cls v)))
		    (loop (cdr l)))))))

(define (create-all-random)
  (fprintf (thread-output-port) "creating all randomly...\n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-args))))
(define (create-all-exhaust)
  (fprintf (thread-output-port) "creating all exhaustively...\n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-all-args))))

(define (create-all-bad)
  (fprintf (thread-output-port) "creating all with bad arguments...\n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-bad-args))))

(define (try-methods cls try)
  (let* ([v (hash-table-get classinfo cls)]
	 [source (car v)]
	 [use (if source (send source choose-example) #f)]
	 [name (cadr v)]
	 [methods (cdddr v)])
    (if (void? use)
	(fprintf (thread-output-port) "~s: no examples\n" name)
	(let loop ([l methods])
	  (unless (null? l)
	    (unless (symbol? (car l))
	      (let* ([method (car l)]
		     [iv (car method)]
		     [resulttype (caddr method)]
		     [argtypes (cdddr method)])
		(set! trying-class (and source (send source get-name)))
		(set! trying-method iv)
		(try argtypes resulttype (list name iv use)
		     (lambda (args)
		       (if use
			   (begin

			     ;; Avoid showing a disabled dialog
			     (when (and (is-a? use dialog%)
					(eq? iv 'show)
					(equal? args '(#t)))
				 (send use enable #t))

			     ;; Avoid excessive scaling
			     (when (eq? iv 'set-scale)
			       (set! args (map (lambda (x) (min x 10)) args)))

			     (send-generic use (make-generic (object-interface use) iv) . args))

			   (apply (namespace-variable-value iv) args))))))
	    (loop (cdr l)))))))

(define (call-random except)
  (fprintf (thread-output-port) "calling all except ~a randomly...\n" except)
  (hash-table-for-each classinfo (lambda (k v)
				   (unless (member k except)
				     (try-methods k try-args)))))

(define (call-all-random)
  (call-random null))

(define (call-all-bad)
  (fprintf (thread-output-port) "calling all with bad arguments...\n")
  (hash-table-for-each classinfo (lambda (k v) (try-methods k try-bad-args))))

(define (call-all-non-editor)
  (call-random (list :editor-buffer% :editor-edit% :editor-snip% :editor-pasteboard% 'EditorGlobal)))

(define (init)
  (create-all-random)
  (create-all-random)
  (create-all-random)
  (create-all-random))

(printf " Creating Example Instances\n")  

(define f (make-object frame% "Example Frame 1"))
(send frame%-example-list add f)

(define d (make-object dialog% "Example Dialog 1"))
(send dialog%-example-list add d)

(define hpl (make-object horizontal-panel% f))
(send horizontal-panel%-example-list add hpl)
(define vpl (make-object vertical-panel% d))
(send vertical-panel%-example-list add vpl)
(define gbpl (make-object group-box-panel% "ok" d))
(send group-box-panel%-example-list add gbpl)
(define tpl (make-object tab-panel% '("Apple" "Banana" "Coconut") d void))
(send tab-panel%-example-list add tpl)
(define hp (make-object horizontal-pane% d))
(send horizontal-pane%-example-list add hp)
(define vp (make-object vertical-pane% f))
(send vertical-pane%-example-list add vp)
(define sp (make-object grow-box-spacer-pane% f))
(send grow-box-spacer-pane%-example-list add sp)

(send message%-example-list add (make-object message% "Message 1" hpl))
(send button%-example-list add (make-object button% "Button 1" vpl void))
(send check-box%-example-list add (make-object check-box% "Check Box 1" hp void))
(send slider%-example-list add (make-object slider% "Slider 1" -10 10 vp void))
(send gauge%-example-list add (make-object gauge% "Gauge 1" 100 hpl))
(send text-field%-example-list add (make-object text-field% "Text Field 1" vpl void))
(send combo-field%-example-list add (make-object combo-field% "Combo Field 1" '("A" "B") vpl void))
(send radio-box%-example-list add (make-object radio-box% "Radio Box 1" '("Radio Button 1.1" "Radio Button 1.2") hp void))
(send choice%-example-list add (make-object choice% "Choice 1" '("Choice 1.1" "Choice 1.2" "Choice 1.3") vp void))
(send list-box%-example-list add (make-object list-box% "List Box 1" '("List Box 1.1" "List Box 1.2" "List Box 1.3") hpl void))

(send canvas%-example-list add (make-object canvas% f))
(define c (make-object editor-canvas% d))
(send editor-canvas%-example-list add c)

(send point%-example-list add (make-object point% 50 60))

(send ps-setup%-example-list add (make-object ps-setup%))

(send color%-example-list add (make-object color% "RED"))
(send font%-example-list add (make-object font% 12 'roman 'normal 'normal))
(send brush%-example-list add (make-object brush% "GREEN" 'solid))
(send pen%-example-list add (make-object pen% "BLUE" 1 'solid))
(send region%-example-list add (make-object region% (send c get-dc)))
(send dc-path%-example-list add (make-object dc-path%))
(send gl-config%-example-list add (make-object gl-config%))
(send gl-context<%>-example-list add (send (send c get-dc) get-gl-context))

(send font-list%-example-list add the-font-list)
(send pen-list%-example-list add the-pen-list)
(send brush-list%-example-list add the-brush-list)
(send color-database<%>-example-list add the-color-database)
(send font-name-directory<%>-example-list add the-font-name-directory)

(send cursor%-example-list add (make-object cursor% 'watch))
(send bitmap%-example-list add (make-object bitmap% (build-path (collection-path "icons") "bb.gif")))

(send control-event%-example-list add (make-object control-event% 'list-box))
(send scroll-event%-example-list add (make-object scroll-event%))
(send mouse-event%-example-list add (make-object mouse-event% 'left-down))
(send key-event%-example-list add (make-object key-event%))

(send bitmap-dc%-example-list add (make-object bitmap-dc%))
(send post-script-dc%-example-list add (make-object post-script-dc% #f))

(with-handlers ([void void])
  (send printer-dc%-example-list add (make-object printer-dc%)))

(define mb (make-object menu-bar% f))
(send menu-bar%-example-list add mb)
(define m (make-object menu% "Menu1" mb))
(send menu%-example-list add m)
(send popup-menu%-example-list add (make-object popup-menu% "Popup Menu 1"))
  
(send separator-menu-item%-example-list add (make-object separator-menu-item% m))
(send menu-item%-example-list add (make-object menu-item% "Menu Item 1" m void))
(send checkable-menu-item%-example-list add (make-object checkable-menu-item% "Checkable Menu Item 1" m void))

(send timer%-example-list add (make-object timer%))

(define sd (make-object style-delta%))
(send add-color<%>-example-list add (send sd get-background-add))
(send mult-color<%>-example-list add (send sd get-background-mult))
(send style-delta%-example-list add sd)
(define sl (make-object style-list%))
(send style-list%-example-list add sl)
(send style<%>-example-list add (send sl basic-style))

(define e (make-object text%))
(send c set-editor e)
(send text%-example-list add e)
(send pasteboard%-example-list add (make-object pasteboard%))

(define s (make-object editor-snip%))
(send e insert s)
(send editor-snip-editor-admin<%>-example-list add (send (send s get-editor) get-admin))
(send snip-admin%-example-list add (make-object snip-admin%))

(send tab-snip%-example-list add (make-object tab-snip%))
(send image-snip%-example-list add (make-object image-snip%))
(send editor-snip%-example-list add (make-object editor-snip%))

(require (prefix graph: (lib "graph.ss" "mrdemo")))
(send readable-snip<%>-example-list add (make-object graph:graph-snip% '(lambda (x) x)))

(send snip-class%-example-list add (make-object snip-class%))
(send snip-class-list<%>-example-list add (get-the-snip-class-list))

(send editor-data%-example-list add (make-object editor-data%))
(send editor-data-class%-example-list add (make-object editor-data-class%))
(send editor-data-class-list<%>-example-list add (get-the-editor-data-class-list))

(send keymap%-example-list add (make-object keymap%))
(send editor-wordbreak-map%-example-list add the-editor-wordbreak-map)

(define sib (make-object editor-stream-in-bytes-base% #"Hello"))
(send editor-stream-in-bytes-base%-example-list add sib)
(define sob (make-object editor-stream-out-bytes-base%))
(send editor-stream-out-bytes-base%-example-list add sob)

(send editor-stream-in%-example-list add (make-object editor-stream-in% sib))
(send editor-stream-out%-example-list add (make-object editor-stream-out% sob))
  
(send clipboard<%>-example-list add the-clipboard)
(send clipboard-client%-example-list add (make-object clipboard-client%))

(printf " Done Creating Example Instances\n")  

(printf " Checking all methods\n")
(define in-top-level null)
(hash-table-for-each classinfo 
		     (lambda (key v)
		       (let* ([methods (cdddr v)]
			      [names (map (lambda (x) (if (pair? x) (car x) x)) methods)])
			 (if (string? key)
			     ;; Check global procs/values
			     (for-each
			      (lambda (name method)
				(if (void? (with-handlers ([void void])
					     (namespace-variable-value name)))
				    ;; Not there
				    (printf "No such procedure/value: ~a\n" name)
				    
				    (let ([v (namespace-variable-value name)])
				      (when (procedure? v)
					;; check arity
					(unless (or (equal? (procedure-arity v) (cadr method))
						    (let ([a (procedure-arity v)]
							  [b (cadr method)])
						      (and (list? a)
							   (list? b)
							   (andmap integer? a)
							   (andmap integer? b)
							   (equal? (sort a <) (sort b <)))))
					  (printf "Arity mismatch for ~a, real: ~a documented: ~a\n" 
						  name (procedure-arity v) (cadr method))))))
				
				(set! in-top-level (cons name in-top-level)))
			      names methods)
			     ;; Check intf/class methods
			     (begin
			       (set! in-top-level (cons (cadr v) in-top-level))
			       
			       ; Check printed form:
			       (let ([p (open-output-string)])
				 (display key p)
				 (let ([sp (get-output-string p)]
				       [ss (let ([s (symbol->string (cadr v))])
					     (format "#<struct:~a:~a>"
						     (if (interface? key) "interface" "class")
						     s))])
				   (unless (string=? sp ss)
				     (printf "bad printed form: ~a != ~a\n" sp ss))))
			       
			       ; Check documented methods are right
			       (let ([ex (send (car v) choose-example)])
				 (unless (is-a? ex key)
				   (printf "Bad example: ~a for ~a\n" ex key))
				 (for-each
				  (lambda (name method)
				    (if (or (and (interface? key)
						 (method-in-interface? name key))
					    (and (class? key)
						 (method-in-interface? name (class->interface key))))
					
					;; Method is there, check arity
					'(when (is-a? ex key)
					  (let ([m (make-generic ex name)])
					    (unless (equal? (arity m) (cadr method))
					      (printf "Warning: arity mismatch for ~a in ~a, real: ~a documented: ~a\n" 
						      name key
						      (arity m) (cadr method)))))
					
					;; Not there
					(printf "No such method: ~a in ~a\n" name key)))
				  names methods))
			       
			       ; Check everything is documented
			       (for-each
				(lambda (n)
				  (unless (memq n names)
				    (printf "Undocumented method: ~a in ~a\n" n key)))
				(interface->method-names (if (interface? key) key (class->interface key)))))))))
(printf " Method-checking done\n")

(let* ([get-all (lambda (n)
		  (parameterize ([current-namespace n])
		    (namespace-mapped-symbols)))]
       [expect-n (list* 'mred@ 'mred^ 
			(append (get-all (let ([n (make-base-namespace)])
					   (parameterize ([current-namespace n])
					     (namespace-require 'mzlib/class))
					   n))
				in-top-level))]
       [actual-n (get-all (make-gui-namespace))])
  (for-each
   (lambda (i)
     (unless (memq i expect-n)
       (printf "Undocumented global: ~a\n" i)))
   actual-n))

(unless (and (>= (vector-length argv) 1)
	     (string=? (vector-ref argv 0) "-r"))
  (exit 0))

;; Disallow writes outside of random/...
(unless (directory-exists? "random")
  (make-directory "random"))
(current-security-guard (make-security-guard (current-security-guard)
					     (lambda (who what why)
					       (when (memq why '(write delete))
						 (unless (regexp-match "/random/" what)
						   (error who
							  "not allowed to write file: ~e"
							  what))))
					     (lambda args 'ok)))

;; Remove some things:
(for-each (lambda (p)
	    (let ([k (ormap (lambda (k)
			      (and (equal? k (car p))
				   k))
			    (hash-table-map classinfo (lambda (k v) k)))])
	      (hash-table-put! 
	       classinfo k
	       (let ([l (hash-table-get classinfo k)])
		 (let loop ([l l])
		   (cond
		    [(null? l) null]
		    [(and (pair? (car l))
			  (eq? (cadr p) (caar l)))
		     (cdr l)]
		    [else (cons (car l) (loop (cdr l)))]))))))
	  '(("Eventspaces" sleep/yield)))

(random-seed 179)

(create-all-bad)
(call-all-bad)

(create-all-random)
(call-all-random)
