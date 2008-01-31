(module scheme scheme/base
  (require "struct.ss"
           "basic.ss"
           "search.ss"
           mzlib/class
           mzlib/for
           setup/main-collects
           syntax/modresolve
           syntax/modcode
           (for-syntax scheme/base))
  
  (provide define-code
           to-element
           to-element/no-color
           to-paragraph
           to-paragraph/prefix
           syntax-ize
           syntax-ize-hook
           current-keyword-list
           current-variable-list
           current-meta-list

           (struct-out shaped-parens)
           (struct-out just-context))

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

  (define-struct (spaces element) (cnt))

  (define (literalize-spaces i)
    (let ([m (regexp-match-positions #rx"  +" i)])
      (if m
          (let ([cnt (- (cdar m) (caar m))])
            (make-spaces #f
                         (list
                          (literalize-spaces (substring i 0 (caar m)))
                          (make-element 'hspace (list (make-string cnt #\space)))
                          (literalize-spaces (substring i (cdar m))))
                         cnt))
          i)))

  (define (typeset-atom c out color? quote-depth)
    (let*-values ([(is-var?) (and (identifier? c)
                                  (memq (syntax-e c) (current-variable-list)))]
                  [(s it? sub?)
                   (let ([sc (syntax-e c)])
                     (let ([s (format "~s" sc)])
                       (if (and (symbol? sc)
                                ((string-length s) . > . 1)
                                (char=? (string-ref s 0) #\_)
                                (not (or (identifier-label-binding c)
                                         is-var?)))
                           (values (substring s 1) #t #f)
                           (values s #f #f))))])
      (if (or (element? (syntax-e c))
              (delayed-element? (syntax-e c))
              (part-relative-element? (syntax-e c)))
          (out (syntax-e c) #f)
          (out (if (and (identifier? c)
                        color?
                        (quote-depth . <= . 0)
                        (not (or it? is-var?)))
                   (if (pair? (identifier-label-binding c))
                       (make-delayed-element
                        (lambda (renderer sec ri)
                          (let* ([tag (find-scheme-tag sec ri c 'for-label)])
                            (if tag
                                (list
                                 (case (car tag)
                                   [(form)
                                    (make-link-element "schemesyntaxlink" (list s) tag)]
                                   [else
                                    (make-link-element "schemevaluelink" (list s) tag)]))
                                (list 
                                 (make-element "badlink"
                                               (list (make-element "schemevaluelink" (list s))))))))
                        (lambda () s)
                        (lambda () s))
                       s)
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
                        [(part-relative-element? v)
                         (element-width v)]
                        [(spaces? v)
                         (+ (sz-loop (car (element-content v)))
                            (spaces-cnt v)
                            (sz-loop (caddr (element-content v))))]
                        [else 1])))]
         [(v cls len)
          (unless (equal? v "")
            (cond
             [(spaces? v)
              (out (car (element-content v)) cls 0)
              (out (cadr (element-content v)) #f 0)
              (out (caddr (element-content v)) cls len)]
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
                                   (if (and color? cls)
                                       (make-element cls (list v))
                                       v))
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
                                (datum->syntax c
                                               (append 
                                                (reverse prev)
                                                (list
                                                 (datum->syntax 
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
            (let ([v (syntax->datum (cadr (syntax->list c)))])
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
                (error "bad code:redex: ~e" (syntax->datum c)))
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
             (datum->syntax #'here 'quote (car (syntax-e c))))
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
                  (out "#" #;(format "#~a" (vector-length vec)) p-color)
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
           [(graph-reference? (syntax-e c))
            (out (format "#~a#" (unbox (graph-reference-bx (syntax-e c)))) 
                 (if (positive? quote-depth) 
                     value-color
                     paren-color))]
           [(graph-defn? (syntax-e c))
            (let ([bx (graph-defn-bx (syntax-e c))])
              (set-box! bx 0)
              (out (format "#~a=" (unbox bx))
                   (if (positive? quote-depth) 
                       value-color
                       paren-color))
              (set! src-col (+ src-col 3))
              ((loop init-line! quote-depth) (graph-defn-r (syntax-e c))))]
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
              (hash-table? s)
              (graph-defn? s)
              (graph-reference? s))
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
			   (quote-syntax ,(datum->syntax v 'defcode))
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
	     (datum->syntax #'here (stx->loc-s-expr s) #f))
	   (syntax-case stx ()
	     [(_ expr) #`(typeset-code #,(cvt #'expr))]
	     [(_ expr (... ...))
	      #`(typeset-code #,(cvt #'(code:line expr (... ...))))])))]
      [(_ code typeset-code uncode d->s)
       #'(define-code code typeset-code uncode d->s syntax-property)]
      [(_ code typeset-code uncode)
       #'(define-code code typeset-code uncode datum->syntax syntax-property)]
      [(_ code typeset-code) #'(define-code code typeset-code unsyntax)]))

  
  (define syntax-ize-hook (make-parameter (lambda (v col) #f)))

  (define (vector->short-list v extract)
    (vector->list v)
    #;
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

  (define-struct graph-reference (bx))
  (define-struct graph-defn (r bx))

  (define (syntax-ize v col)
    (do-syntax-ize v col (make-hash-table) #f))

  (define (graph-count ht graph?)
    (and graph?
         (let ([n (hash-table-get ht '#%graph-count 0)])
           (hash-table-put! ht '#%graph-count (add1 n))
           n)))

  (define (do-syntax-ize v col ht graph?)
    (cond
     [((syntax-ize-hook) v col)
      => (lambda (r) r)]
     [(shaped-parens? v)
      (syntax-property (do-syntax-ize (shaped-parens-val v) col ht #f)
                       'paren-shape
                       (shaped-parens-shape v))]
     [(just-context? v)
      (let ([s (do-syntax-ize (just-context-val v) col ht #f)])
        (datum->syntax (just-context-ctx v)
                       (syntax-e s)
                       s
                       s
                       (just-context-ctx v)))]
     [(hash-table-get ht v #f)
      => (lambda (m)
           (unless (unbox m)
             (set-box! m #t))
           (datum->syntax #f
                          (make-graph-reference m)
                          (list #f 1 col (+ 1 col) 1)))]
     [(and (list? v)
           (pair? v)
           (memq (let ([s (car v)])
                   (if (just-context? s)
                       (just-context-val s)
                       s))
                 '(quote unquote unquote-splicing)))
      (let ([c (do-syntax-ize (cadr v) (+ col 1) ht #f)])
        (datum->syntax #f
                       (list (do-syntax-ize (car v) col ht #f)
                             c)
                       (list #f 1 col (+ 1 col)
                             (+ 1 (syntax-span c)))))]
     [(or (list? v)
          (vector? v))
      (let ([graph-box (box (graph-count ht graph?))])
        (hash-table-put! ht v graph-box)
        (let ([r (let* ([vec-sz (+ (if graph? 
                                       (+ 2 (string-length (format "~a" (unbox graph-box)))) 
                                       0)
                                   (if (vector? v)
                                       (+ 1 #;(string-length (format "~a" (vector-length v))))
                                       0))])
                   (let ([l (let loop ([col (+ col 1 vec-sz)]
                                       [v (if (vector? v)
                                              (vector->short-list v values)
                                              v)])
                              (if (null? v)
                                  null
                                  (let ([i (do-syntax-ize (car v) col ht #f)])
                                    (cons i
                                          (loop (+ col 1 (syntax-span i)) (cdr v))))))])
                     (datum->syntax #f
                                    (if (vector? v)
                                        (short-list->vector v l)
                                        l)
                                    (list #f 1 col (+ 1 col)
                                          (+ 2
                                             vec-sz
                                             (if (zero? (length l))
                                                 0
                                                 (sub1 (length l)))
                                             (apply + (map syntax-span l)))))))])
          (unless graph?
            (hash-table-put! ht v #f))
          (cond
           [graph? (datum->syntax #f
                                  (make-graph-defn r graph-box)
                                  r)]
           [(unbox graph-box)
            ;; Go again, this time knowing that there will be a graph:
            (do-syntax-ize v col ht #t)]
           [else r])))]
     [(pair? v)
      (let ([graph-box (box (graph-count ht graph?))])
        (hash-table-put! ht v graph-box)
        (let* ([inc (if graph? 
                        (+ 2 (string-length (format "~a" (unbox graph-box)))) 
                        0)]
               [a (do-syntax-ize (car v) (+ col 1 inc) ht #f)]
               [sep (if (and (pair? (cdr v))
                             ;; FIXME: what if it turns out to be a graph reference?
                             (not (hash-table-get ht (cdr v) #f)))
                        0 
                        3)]
               [b (do-syntax-ize (cdr v) (+ col 1 inc (syntax-span a) sep) ht #f)])
          (let ([r (datum->syntax #f
                                  (cons a b)
                                  (list #f 1 (+ col inc) (+ 1 col inc)
                                        (+ 2 sep (syntax-span a) (syntax-span b))))])
            (unless graph?
              (hash-table-put! ht v #f))
            (cond
             [graph? (datum->syntax #f
                                    (make-graph-defn r graph-box)
                                    (list #f 1 col (+ 1 col)
                                          (+ inc (syntax-span r))))]
             [(unbox graph-box)
              ;; Go again...
              (do-syntax-ize v col ht #t)]
             [else r]))))]
     [(box? v)
      (let ([a (do-syntax-ize (unbox v) (+ col 2) ht #f)])
        (datum->syntax #f
                       (box a)
                       (list #f 1 col (+ 1 col)
                             (+ 2 (syntax-span a)))))]
     [else
      (datum->syntax #f v (list #f 1 col (+ 1 col) 1))])))
