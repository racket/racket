(module scheme (lib "lang.ss" "big")
  (require "struct.ss"
           "basic.ss"
           (lib "class.ss")
           (lib "for.ss")
           (lib "main-collects.ss" "setup")
           (lib "modresolve.ss" "syntax"))

  (provide define-code
           to-element
           to-element/no-color
           to-paragraph
           to-paragraph/prefix
           register-scheme-definition
           register-scheme-form-definition
           syntax-ize
           syntax-ize-hook
           current-keyword-list
           current-variable-list
           current-meta-list

           (struct shaped-parens (val shape))
           (struct just-context (val ctx)))

  (define no-color "schemeplain")
  (define reader-color "schemereader")
  (define keyword-color "schemekeyword")
  (define comment-color "schemecomment")
  (define paren-color "schemeparen")
  (define meta-color "schememeta")
  (define value-color "schemevalue")
  (define symbol-color "schemesymbol")
  (define variable-color "schemevariable")
  (define opt-color "schemeopt")

  (define current-keyword-list 
    (make-parameter null))
  (define current-variable-list 
    (make-parameter null))
  (define current-meta-list 
    (make-parameter null))

  (define defined-names (make-hash-table))

  (define-struct (sized-element element) (length))

  (define-struct spaces (pre cnt post))

  (define (literalize-spaces i)
    (let ([m (regexp-match-positions #rx"  +" i)])
      (if m
          (make-spaces (literalize-spaces (substring i 0 (caar m)))
                       (- (cdar m) (caar m))
                       (literalize-spaces (substring i (cdar m))))
          i)))

  (define (typeset-atom c out color? quote-depth)
    (let-values ([(s it? sub?)
                  (let ([c (syntax-e c)])
                    (let ([s (format "~s" c)])
                      (if (and (symbol? c)
                               ((string-length s) . > . 1)
                               (char=? (string-ref s 0) #\_))
                          (values (substring s 1) #t #f)
                          (values s #f #f))))]
                 [(is-var?) (and (identifier? c)
                                 (memq (syntax-e c) (current-variable-list)))])
      (if (or (element? (syntax-e c))
              (delayed-element? (syntax-e c)))
          (out (syntax-e c) #f)
          (out (if (and (identifier? c)
                        color?
                        (quote-depth . <= . 0)
                        (not (or it? is-var?)))
                   (let ([tag (register-scheme c)])
                     (if tag
                         (make-delayed-element
                          (lambda (renderer sec ri)
                            (let* ([vtag `(def ,tag)]
                                   [stag `(form ,tag)]
                                   [sd (resolve-get/tentative sec ri stag)])
                              (list
                               (cond
                                [sd 
                                 (make-link-element "schemesyntaxlink" (list s) stag)]
                                [else
                                 (make-link-element "schemevaluelink" (list s) vtag)]))))
                          (lambda () s)
                          (lambda () s))
                         s))
                   (literalize-spaces s))
               (cond
                [(positive? quote-depth) value-color]
                [(let ([v (syntax-e c)])
                   (or (number? v)
                       (string? v)
                       (bytes? v)
                       (char? v)
                       (regexp? v)
                       (byte-regexp? v)
                       (boolean? v)))
                 value-color]
                [(identifier? c) 
                 (cond
                  [is-var?
                   variable-color]
                  [(and (identifier? c)
                        (memq (syntax-e c) (current-keyword-list)))
                   keyword-color]
                  [(and (identifier? c)
                        (memq (syntax-e c) (current-meta-list)))
                   meta-color]
                  [it? variable-color]
                  [else symbol-color])]
                [else paren-color])
               (string-length s)))))

  (define (gen-typeset c multi-line? prefix1 prefix suffix color?)
    (let* ([c (syntax-ize c 0)]
           [content null]
           [docs null]
           [first (syntax-case c (code:line)
                    [(code:line e . rest) #'e]
                    [else c])]
           [init-col (or (syntax-column first) 0)]
           [src-col init-col]
           [dest-col 0]
           [highlight? #f]
           [col-map (make-hash-table 'equal)]
           [next-col-map (make-hash-table 'equal)]
           [line (or (syntax-line first) 0)])
      (define (finish-line!)
        (when multi-line?
          (set! docs (cons (make-flow (list (make-paragraph (reverse content))))
                           docs))
          (set! content null)))
      (define out
        (case-lambda
         [(v cls)
          (out v cls (let sz-loop ([v v])
                       (cond
                        [(string? v) (string-length v)]
                        [(sized-element? v) (sized-element-length v)]
                        [(and (element? v)
                              (= 1 (length (element-content v))))
                         (sz-loop (car (element-content v)))]
                        [(element? v)
                         (element-width v)]
                        [(delayed-element? v)
                         (element-width v)]
                        [(spaces? v)
                         (+ (sz-loop (spaces-pre v))
                            (spaces-cnt v)
                            (sz-loop (spaces-post v)))]
                        [else 1])))]
         [(v cls len)
          (unless (equal? v "")
            (cond
             [(spaces? v)
              (out (spaces-pre v) cls 0)
              (out (make-element 'hspace (list (make-string (spaces-cnt v) #\space))) #f 0)
              (out (spaces-post v) cls len)]
             [(equal? v "\n")
              (if multi-line?
                  (begin
                    (finish-line!)
                    (out prefix cls))
                  (out " " cls))]
             [else
              (set! content (cons ((if highlight?
                                       (lambda (c)
                                         (make-element "highlighted" (list c)))
                                       values)
                                   (if color?
                                       (make-element cls (list v))
                                       (make-element #f (list v))))
                                  content))
              (set! dest-col (+ dest-col len))]))]))
      (define advance
        (case-lambda
         [(c init-line! delta)
          (let ([c (+ delta (or (syntax-column c) 0))]
                [l (syntax-line c)])
            (let ([new-line? (and l (l . > . line))])
              (when new-line?
                (for ([i (in-range (- l line))])
                  (out "\n" no-color))
                (set! line l)
                (set! col-map next-col-map)
                (set! next-col-map (make-hash-table 'equal))
                (init-line!))
              (let ([d-col (let ([def-val (+ dest-col (- c src-col))])
                             (if new-line?
                                 (hash-table-get col-map c def-val)
                                 def-val))])
                (let ([amt (- d-col dest-col)])
                  (when (positive? amt)
                    (let ([old-dest-col dest-col])
                      (out (if (and (= 1 amt) (not multi-line?))
                               (make-element 'tt (list " ")) ; allows a line break to replace the space
                               (make-element 'hspace (list (make-string amt #\space))))
                           #f)
                      (set! dest-col (+ old-dest-col amt))))))
              (set! src-col c)
              (hash-table-put! next-col-map src-col dest-col)))]
         [(c init-line!) (advance c init-line! 0)]))
      (define (convert-infix c quote-depth)
        (let ([l (syntax->list c)])
          (and l
               ((length l) . >= . 3)
               ((or (syntax-position (car l)) -inf.0)
                . > .
                (or (syntax-position (cadr l)) +inf.0))
               (let ([a (car l)])
                 (let loop ([l (cdr l)]
                            [prev null])
                   (cond
                    [(null? l) #f] ; couldn't unwind
                    [else (let ([p2 (syntax-position (car l))])
                            (if (and p2
                                     (p2 . > . (syntax-position a)))
                                (datum->syntax-object c
                                                      (append 
                                                       (reverse prev)
                                                       (list
                                                        (datum->syntax-object 
                                                         a
                                                         (let ([val? (positive? quote-depth)])
                                                           (make-sized-element 
                                                            (if val? value-color #f)
                                                            (list
                                                             (make-element (if val? value-color paren-color) '(". "))
                                                             (typeset a #f "" "" "" (not val?))
                                                             (make-element (if val? value-color paren-color) '(" .")))
                                                            (+ (syntax-span a) 4)))
                                                         (list (syntax-source a)
                                                               (syntax-line a)
                                                               (- (syntax-column a) 2)
                                                               (- (syntax-position a) 2)
                                                               (+ (syntax-span a) 4))
                                                         a))
                                                       l)
                                                      c
                                                      c)
                                (loop (cdr l)
                                      (cons (car l) prev))))]))))))
      (define (no-fancy-chars s)
        (cond
         [(eq? s 'rsquo) "'"]
         [else s]))
      (define (loop init-line! quote-depth)
        (lambda (c)
          (cond
           [(eq? 'code:blank (syntax-e c))
            (advance c init-line!)]
           [(and (pair? (syntax-e c))
                 (eq? (syntax-e (car (syntax-e c))) 'code:comment))
            (advance c init-line!)
            (out "; " comment-color)
            (let ([v (syntax-object->datum (cadr (syntax->list c)))])
              (if (paragraph? v)
                  (map (lambda (v) 
                         (let ([v (no-fancy-chars v)])
                           (if (string? v)
                               (out v comment-color)
                               (out v #f))))
                       (paragraph-content v))
                  (out (no-fancy-chars v) comment-color)))]
           [(and (pair? (syntax-e c))
                 (eq? (syntax-e (car (syntax-e c))) 'code:contract))
            (advance c init-line!)
            (out "; " comment-color)
            (let* ([l (cdr (syntax->list c))]
                   [s-col (or (syntax-column (car l)) src-col)])
              (set! src-col s-col)
              (for-each (loop (lambda ()
                                (set! src-col s-col)
                                (set! dest-col 0)
                                (out "; " comment-color))
                              0)
                        l))]
           [(and (pair? (syntax-e c))
                 (eq? (syntax-e (car (syntax-e c))) 'code:line))
            (let ([l (cdr (syntax->list c))])
              (for-each (loop init-line! quote-depth) 
                        l))]
           [(and (pair? (syntax-e c))
                 (eq? (syntax-e (car (syntax-e c))) 'code:hilite))
            (let ([l (syntax->list c)]
                  [h? highlight?])
              (unless (and l (= 2 (length l)))
                (error "bad code:redex: ~e" (syntax-object->datum c)))
              (advance c init-line!)
              (set! src-col (syntax-column (cadr l)))
              (hash-table-put! next-col-map src-col dest-col)
              (set! highlight? #t)
              ((loop init-line! quote-depth) (cadr l))
              (set! highlight? h?)
              (set! src-col (add1 src-col)))]
           [(and (pair? (syntax-e c))
                 (eq? (syntax-e (car (syntax-e c))) 'code:quote))
            (advance c init-line!)
            (out "(" (if (positive? quote-depth) value-color paren-color))
            (set! src-col (+ src-col 1))
            (hash-table-put! next-col-map src-col dest-col)
            ((loop init-line! quote-depth) 
             (datum->syntax-object #'here 'quote (car (syntax-e c))))
            (for-each (loop init-line! (add1 quote-depth))
                      (cdr (syntax->list c)))
            (out ")" (if (positive? quote-depth) value-color paren-color))
            (set! src-col (+ src-col 1))
            #;
            (hash-table-put! next-col-map src-col dest-col)]
           [(and (pair? (syntax-e c))
                 (memq (syntax-e (car (syntax-e c))) 
                       '(quote quasiquote unquote unquote-splicing
                               quasisyntax syntax unsyntax unsyntax-splicing)))
            (advance c init-line!)
            (let-values ([(str quote-delta)
                          (case (syntax-e (car (syntax-e c)))
                            [(quote) (values "'" +inf.0)]
                            [(unquote) (values "," -1)]
                            [(unquote-splicing) (values ",@" -1)]
                            [(quasiquote) (values "`" +1)]
                            [(syntax) (values "#'" 0)]
                            [(quasisyntax) (values "#`" 0)]
                            [(unsyntax) (values "#," 0)]
                            [(unsyntax-splicing) (values "#,@" 0)])])
              (out str (if (positive? (+ quote-depth quote-delta))
                           value-color
                           reader-color))
              (let ([i (cadr (syntax->list c))])
                (set! src-col (or (syntax-column i) src-col))
                (hash-table-put! next-col-map src-col dest-col)
                ((loop init-line! (+ quote-depth quote-delta)) i)))]
           [(and (pair? (syntax-e c))
                 (convert-infix c quote-depth))
            => (lambda (converted)
                 ((loop init-line! quote-depth) converted))]
           [(or (pair? (syntax-e c))
                (null? (syntax-e c))
                (vector? (syntax-e c)))
            (let* ([sh (or (syntax-property c 'paren-shape)
                           #\()]
                   [quote-depth (if (vector? (syntax-e c))
                                    +inf.0
                                    quote-depth)]
                   [p-color (if (positive? quote-depth) 
                                value-color
                                (if (eq? sh #\?)
                                    opt-color
                                    paren-color))])
              (advance c init-line!)
              (when (vector? (syntax-e c))
                (let ([vec (syntax-e c)])
                  (out (format "#~a" (vector-length vec)) p-color)
                  (if (zero? (vector-length vec))
                      (set! src-col (+ src-col (- (syntax-span c) 2)))
                      (set! src-col (+ src-col (- (syntax-column (vector-ref vec 0))
                                                  (syntax-column c)
                                                  1))))))
              (out (case sh
                     [(#\[ #\?) "["]
                     [(#\{) "{"]
                     [else "("])
                   p-color)
              (set! src-col (+ src-col 1))
              (hash-table-put! next-col-map src-col dest-col)
              (let lloop ([l (if (vector? (syntax-e c))
                                 (vector->short-list (syntax-e c) syntax-e)
                                 c)])
                (cond
                 [(and (syntax? l)
                       (pair? (syntax-e l))
                       (not (memq (syntax-e (car (syntax-e l)))
                                  '(quote unquote syntax unsyntax quasiquote quasiunsyntax))))
                  (lloop (syntax-e l))]
                 [(or (null? l)
                      (and (syntax? l)
                           (null? (syntax-e l))))
                  (void)]
                 [(pair? l)
                  ((loop init-line! quote-depth) (car l))
                  (lloop (cdr l))]
                 [else
                  
                  (advance l init-line! -2)
                  (out ". " (if (positive? quote-depth) value-color paren-color))
                  (set! src-col (+ src-col 3))
                  (hash-table-put! next-col-map src-col dest-col)
                  ((loop init-line! quote-depth) l)]))
              (out (case sh
                     [(#\[ #\?) "]"]
                     [(#\{) "}"]
                     [else ")"])
                   p-color)
              (set! src-col (+ src-col 1))
              #;
              (hash-table-put! next-col-map src-col dest-col))]
           [(box? (syntax-e c))
            (advance c init-line!)
            (out "#&" value-color)
            (set! src-col (+ src-col 2))
            (hash-table-put! next-col-map src-col dest-col)
            ((loop init-line! +inf.0) (unbox (syntax-e c)))]
           [(hash-table? (syntax-e c))
            (advance c init-line!)
            (let ([equal-table? (hash-table? (syntax-e c) 'equal)])
              (out (if equal-table?
                       "#hash"
                       "#hasheq")
                   value-color)
              (let ([delta (+ 5 (if equal-table? 2 0))]
                    [orig-col src-col])
                (set! src-col (+ src-col delta))
                (hash-table-put! next-col-map src-col dest-col)
                ((loop init-line! +inf.0)
                 (syntax-ize (hash-table-map (syntax-e c) cons)
                             (+ (syntax-column c) delta)))
                (set! src-col (+ orig-col (syntax-span c)))))]
           [else
            (advance c init-line!)
            (typeset-atom c out color? quote-depth)
            (set! src-col (+ src-col (or (syntax-span c) 1)))
            #;
            (hash-table-put! next-col-map src-col dest-col)])))
      (out prefix1 #f)
      (set! dest-col 0)
      (hash-table-put! next-col-map init-col dest-col)
      ((loop (lambda () (set! src-col init-col) (set! dest-col 0)) 0) c)
      (if (list? suffix)
          (map (lambda (sfx)
                 (finish-line!)
                 (out sfx #f))
               suffix)
          (out suffix #f))
      (unless (null? content)
        (finish-line!))
      (if multi-line?
          (if (= 1 (length docs))
              (car (flow-paragraphs (car docs)))
              (make-table "schemeblock" (map list (reverse docs))))
          (make-sized-element #f (reverse content) dest-col))))

  (define (typeset c multi-line? prefix1 prefix suffix color?)
    (let* ([c (syntax-ize c 0)]
           [s (syntax-e c)])
      (if (or multi-line?
              (eq? 'code:blank s)
              (pair? s)
              (vector? s)
              (box? s)
              (null? s)
              (hash-table? s))
          (gen-typeset c multi-line? prefix1 prefix suffix color?)
          (typeset-atom c 
                        (case-lambda 
                         [(elem color)
                          (make-sized-element (and color? color) (list elem) (or (syntax-span c) 1))]
                         [(elem color len)
                          (make-sized-element (and color? color) (list elem) len)])
                        color? 0))))
    
  (define (to-element c)
    (typeset c #f "" "" "" #t))

  (define (to-element/no-color c)
    (typeset c #f "" "" "" #f))

  (define (to-paragraph c)
    (typeset c #t "" "" "" #t))

  (define ((to-paragraph/prefix pfx1 pfx sfx) c)
    (typeset c #t pfx1 pfx sfx #t))

  (define-syntax (define-code stx)
    (syntax-case stx ()
      [(_ code typeset-code uncode d->s stx-prop)
       (syntax/loc stx
	 (define-syntax (code stx)
	   (define (stx->loc-s-expr v)
	     (cond
	      [(syntax? v)
	       (let ([mk `(,#'d->s
			   (quote-syntax ,(datum->syntax-object v 'defcode))
			   ,(syntax-case v (uncode)
			      [(uncode e) #'e]
			      [else (stx->loc-s-expr (syntax-e v))])
			   '(code
                             ,(syntax-line v)
                             ,(syntax-column v)
                             ,(syntax-position v)
                             ,(syntax-span v)))])
		 (let ([prop (syntax-property v 'paren-shape)])
		   (if prop
		       `(,#'stx-prop ,mk 'paren-shape ,prop)
		       mk)))]
	      [(pair? v) `(cons ,(stx->loc-s-expr (car v))
				,(stx->loc-s-expr (cdr v)))]
	      [(vector? v) `(vector ,@(map
				       stx->loc-s-expr
				       (vector->list v)))]
	      [(box? v) `(box ,(stx->loc-s-expr (unbox v)))]
	      [(null? v) 'null]
	      [else `(quote ,v)]))
	   (define (cvt s)
	     (datum->syntax-object #'here (stx->loc-s-expr s) #f))
	   (syntax-case stx ()
	     [(_ expr) #`(typeset-code #,(cvt #'expr))]
	     [(_ expr (... ...))
	      #`(typeset-code #,(cvt #'(code:line expr (... ...))))])))]
      [(_ code typeset-code uncode d->s)
       #'(define-code code typeset-code uncode d->s syntax-property)]
      [(_ code typeset-code uncode)
       #'(define-code code typeset-code uncode datum->syntax-object syntax-property)]
      [(_ code typeset-code) #'(define-code code typeset-code unsyntax)]))

  
  (define (register-scheme stx [warn-if-no-label? #f])
    (unless (identifier? stx)
      (error 'register-scheme-definition "not an identifier: ~e" (syntax-object->datum stx)))
    (let ([b (identifier-label-binding stx)])
      (if (or (not b)
              (eq? b 'lexical))
          (if warn-if-no-label?
              (begin
                (fprintf (current-error-port)
                         "~a\n"
                         ;; Call raise-syntax-error to capture error message:
                         (with-handlers ([exn:fail:syntax? (lambda (exn)
                                                             (exn-message exn))])
                           (raise-syntax-error 'WARNING
                                               "no for-label binding of identifier"
                                               stx)))
                (format ":NOLABEL:~a" (syntax-e stx)))
              #f)
          (format ":~a:~a" 
                  (if (module-path-index? (car b))
                      (let ([p (resolve-module-path-index (car b) #f)])
                        (path->main-collects-relative p))
                      (car b))
                  (cadr b)))))

  (define (register-scheme/invent stx warn-if-no-label?)
    (or (register-scheme stx warn-if-no-label?)
        (format ":UNKNOWN:~a" (syntax-e stx))))

  (define (register-scheme-definition stx [warn-if-no-label? #f])
    `(def ,(register-scheme/invent stx warn-if-no-label?)))

  (define (register-scheme-form-definition stx [warn-if-no-label? #f])
    `(form ,(register-scheme/invent stx warn-if-no-label?)))

  (define syntax-ize-hook (make-parameter (lambda (v col) #f)))

  (define (vector->short-list v extract)
    (let ([l (vector->list v)])
      (reverse (list-tail
                (reverse l)
                (- (vector-length v)
                   (let loop ([i (sub1 (vector-length v))])
                     (cond
                      [(zero? i) 1]
                      [(eq? (extract (vector-ref v i))
                            (extract (vector-ref v (sub1 i))))
                       (loop (sub1 i))]
                      [else (add1 i)])))))))

  (define (short-list->vector v l)
    (list->vector
     (let ([n (length l)])
       (if (n . < . (vector-length v))
           (reverse (let loop ([r (reverse l)][i (- (vector-length v) n)])
                      (if (zero? i)
                          r
                          (loop (cons (car r) r) (sub1 i)))))
           l))))

  (define-struct shaped-parens (val shape))
  (define-struct just-context (val ctx))

  (define (syntax-ize v col)
    (cond
     [((syntax-ize-hook) v col)
      => (lambda (r) r)]
     [(shaped-parens? v)
      (syntax-property (syntax-ize (shaped-parens-val v) col)
                       'paren-shape
                       (shaped-parens-shape v))]
     [(just-context? v)
      (let ([s (syntax-ize (just-context-val v) col)])
        (datum->syntax-object (just-context-ctx v)
                              (syntax-e s)
                              s
                              s
                              (just-context-ctx v)))]
     [(and (list? v)
           (pair? v)
           (memq (let ([s (car v)])
                   (if (just-context? s)
                       (just-context-val s)
                       s))
                 '(quote unquote unquote-splicing)))
      (let ([c (syntax-ize (cadr v) (+ col 1))])
        (datum->syntax-object #f
                              (list (syntax-ize (car v) col)
                                    c)
                              (list #f 1 col (+ 1 col)
                                    (+ 1 (syntax-span c)))))]
     [(or (list? v)
          (vector? v))
      (let* ([vec-sz (if (vector? v)
                         (+ 1 (string-length (format "~a" (vector-length v))))
                         0)])
        (let ([l (let loop ([col (+ col 1 vec-sz)]
                            [v (if (vector? v)
                                   (vector->short-list v values)
                                   v)])
                   (if (null? v)
                       null
                       (let ([i (syntax-ize (car v) col)])
                         (cons i
                               (loop (+ col 1 (syntax-span i)) (cdr v))))))])
          (datum->syntax-object #f
                                (if (vector? v)
                                    (short-list->vector v l)
                                    l)
                                (list #f 1 col (+ 1 col)
                                      (+ 2
                                         vec-sz
                                         (if (zero? (length l))
                                             0
                                             (sub1 (length l)))
                                         (apply + (map syntax-span l)))))))]
     [(pair? v)
      (let* ([a (syntax-ize (car v) (+ col 1))]
             [sep (if (pair? (cdr v)) 0 3)]
             [b (syntax-ize (cdr v) (+ col 1 (syntax-span a) sep))])
        (datum->syntax-object #f
                              (cons a b)
                              (list #f 1 col (+ 1 col)
                                    (+ 2 sep (syntax-span a) (syntax-span b)))))]
     [(box? v)
      (let ([a (syntax-ize (unbox v) (+ col 2))])
        (datum->syntax-object #f
                              (box a)
                              (list #f 1 col (+ 1 col)
                                    (+ 2 (syntax-span a)))))]
     [else
      (datum->syntax-object #f v (list #f 1 col (+ 1 col) 1))])))
