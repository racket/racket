
(module ho-contracts mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "match.ss")
           (lib "list.ss"))
  
  (initial-font-size 7) (reduction-steps-cutoff 10)
  ;(initial-font-size 36) (reduction-steps-cutoff 1)
  
  (define lang
    (language
     (p ((d ...) e))
     (d (valrec x : e = e))
     (e (lambda (x) e)
        (e e)
        (let ((x e) ...) e)
        x
        (fix x e)
        number
        (aop e e)
        (rop e e)
        (cons e e)
        empty
        (hd e)
        (tl e)
        (mt e)
        (if e e e)
        true
        false
        string
        (--> e e)
        (contract e)
        (flatp e)
        (pred e)
        (dom e)
        (rng e)
        (blame e))
     (x (variable-except valrec lambda let fix aop rop cons empty hd tl mt if true false --> contract flatp pred dom rng blame))
     
     (p-ctxt (((valrec x : v = v) ...
               (valrec x : e-ctxt = e)
               d ...)
              e)
             (((valrec x : v = v) ...
               (valrec x : v = e-ctxt)
               d ...)
              e)
             (((valrec x : v = v) ...)
              e-ctxt))
     (e-ctxt (e-ctxt e)
             (v e-ctxt)
             (let ((x v) ... (x e-ctxt) (x e) ...) e)
             (aop e-ctxt e)
             (aop v e-ctxt)
             (rop e-ctxt e)
             (rop v e-ctxt)
             (cons e-ctxt e)
             (cons v e-ctxt)
             (hd e-ctxt)
             (tl e-ctxt)
             (mt e-ctxt)
             (if e-ctxt e e)
             (--> v e-ctxt)
             (--> e-ctxt e)
             (contract e-ctxt)
             (flatp e-ctxt)
             (pred e-ctxt)
             (dom e-ctxt)
             (rng e-ctxt)
             (blame e-ctxt)
             hole)
     (v (cons v v)
        (lambda (x) e)
        string
        number
        true
        false 
        (--> v v)
        (contract v)
        (ob v (--> v v) x x)
        compile-v1
        compile-v2)
     
     (aop + - * /)
     (rop = >=)))
  
  (define ho-contracts-subst
    (subst
     [`(let ([,a-vars ,rhs-exps] ...) ,body)
      (all-vars a-vars)
      (build (lambda (vars body . rhss) 
               `(let (,@(map (lambda (var rhs) `[,var ,rhs]) vars rhss))
                  ,body)))
      (subterm a-vars body)
      (subterms '() rhs-exps)]
     [`(lambda (,var) ,body)
      (all-vars (list var))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list var) body)]
     [`(fix ,variable ,e)
      (all-vars (list variable))
      (build (lambda (vars body) `(fix ,(car vars) ,body)))
      (subterm (list variable) e)]
     [(? number?) (constant)]
     [`(,(and op (? (lambda (x) (memq x '(cons + - = > / -->))))) ,e1 ,e2)
      (all-vars '())
      (build (lambda (vars e1 e2) `(,op ,e1 ,e2)))
      (subterm '() e1)
      (subterm '() e2)]
     [`empty (constant)]
     [`(,(and op (? (lambda (x) (memq x '(hd tl mt contract flat pred dom rng blame))))) ,e1)
      (all-vars '())
      (build (lambda (vars e1) `(,op ,e1)))
      (subterm '() e1)]
     [`(if ,e1 ,e2 ,e3)
      (all-vars '())
      (build (lambda (vars e1 e2 e3) `(if ,e1 ,e2 ,e3)))
      (subterm '() e1)
      (subterm '() e2)
      (subterm '() e3)]
     [`(,e1 ,e2)
      (all-vars '())
      (build (lambda (vars e1 e2) `(,e1 ,e2)))
      (subterm '() e1)
      (subterm '() e2)]
     [`true (constant)]
     [`false (constant)]
     [(? string?) (constant)]
     [(? symbol?) (variable)]))
  
  (define reductions
    (list
     (reduction lang  
                (in-hole (name p p-ctxt) (/ number_n number_m)) 
                (if (= (term number_m) 0)
                    (term (error /))
                    (plug (term p) (/ (term number_n) (term number_m)))))
     (reduction lang  
                (in-hole (name p p-ctxt) (* number_n number_m))
                (plug (term p) (* (term number_n) (term number_m))))
     (reduction lang  
                (in-hole (name p p-ctxt) (+ number_n number_m))
                (plug (term p) (+ (term number_n) (term number_m))))
     (reduction lang  
                (in-hole (name p p-ctxt) (- number_n number_m))
                (plug (term p) (- (term number_n) (term number_m))))
     (reduction lang  
                (in-hole (name p p-ctxt) (>= number_n number_m))
                (plug (term p) (if (>= (term number_n) (term number_m)) 'true 'false)))
     (reduction lang  
                (in-hole (name p p-ctxt) (= number_n number_m))
                (plug (term p) (if (= (term number_n) (term number_m)) 'true 'false)))
     (reduction lang  
                (in-hole (name p p-ctxt) ((lambda (variable_x) e_body) v_arg))
                (plug (term p) (ho-contracts-subst (term variable_x) 
                                                   (term v_arg) 
                                                   (term e_body))))
     (reduction lang  
                (in-hole (name p p-ctxt) 
                         (let ((variable_i v_i) ...) e_body))
                (plug (term p) 
                      (foldl
                       ho-contracts-subst
                       (term e_body)
                       (term (variable_i ...))
                       (term (v_i ...)))))
     (reduction lang  
                (in-hole (name p p-ctxt) (name tot (fix (name x variable) (name body e))))
                (plug (term p) (ho-contracts-subst (term x) (term tot) (term body))))
     (reduction lang  
                ((name defns
                       ((valrec (name bvar variable) : (name bctc value) = (name brhs value)) ...
                        (valrec (name var variable) : value = (name rhs value))
                        (valrec variable : value = value) ...))
                 (in-hole (name p e-ctxt) (name var variable)))
                (term (defns ,(plug (term p) (term rhs)))))
     (reduction lang
                (in-hole (name p p-ctxt) (if true e_then e))
                (plug (term p) (term e_then)))
     (reduction lang  
                (in-hole (name p p-ctxt) (if false e e_else))
                (plug (term p) (term e_else)))
     (reduction lang  
                (in-hole (name p p-ctxt) (hd (cons v_fst v)))
                (plug (term p) (term v_fst)))
     (reduction lang  
                (in-hole (name p p-ctxt) (hd empty))
                (term (error hd)))
     (reduction lang  
                (in-hole (name p p-ctxt) (tl (cons v v_rst)))
                (plug (term p) (term v_rst)))
     (reduction lang  
                (in-hole (name p p-ctxt) (tl empty))
                (term (error tl)))
     (reduction lang  
                (in-hole (name p p-ctxt) (mt empty))
                (plug (term p) 'true))
     (reduction lang  
                (in-hole (name p p-ctxt) (mt (cons v v)))
                (plug (term p) 'false))
     (reduction lang  
                (in-hole (name p p-ctxt) (flatp (contract v)))
                (plug (term p) 'true))
     (reduction lang  
                (in-hole (name p p-ctxt) (flatp (--> v v)))
                (plug (term p) 'false))
     (reduction lang  
                (in-hole (name p p-ctxt) (pred (contract v_pred)))
                (plug (term p) (term v_pred)))
     (reduction lang  
                (in-hole (name p p-ctxt) (pred (--> v v)))
                (term (error pred)))
     (reduction lang  
                (in-hole (name p p-ctxt) (dom (--> v_dm v)))
                (plug (term p) (term v_dm)))
     (reduction lang  
                (in-hole (name p p-ctxt) (dom (contract v)))
                (term (error dom)))
     (reduction lang  
                (in-hole (name p p-ctxt) (rng (--> v v_rg)))
                (plug (term p) (term v_rg)))
     (reduction lang  
                (in-hole (name p p-ctxt) (rng (contract v)))
                (term (error rng)))
     (reduction lang  
                (in-hole (name p p-ctxt) (blame (name x variable)))
                (term (error x)))))
  
  (define (pp v port w spec)
    (parameterize ([current-output-port port])
      (pp-prog v spec)))
  
  (define (pp-prog prog spec)
    (for-each (lambda (x) (pp-defn x spec)) (car prog))
    (pp-expr (cadr prog) 0 spec)
    (display "\n"))
  
  (define (pp-defn defn spec)
    (let ([var (second defn)]
          [ctc (fourth defn)]
          [exp (sixth defn)])
      (printf "val rec ")
      (display var)
      (display " : ")
      (pp-expr ctc 0 spec)
      (display " = ")
      (pp-expr exp 0 spec)
      (display "\n")))
  
  (define (dp/ct x)
    (let* ([str (format "~a" x)]
           [ct (string-length str)])
      (display str)
      ct))
  
  ;; pp-expr : sexp number (snip -> void) -> (union #f number)
  ;; returns #f if it started a new line and a
  ;; number if it didn't. The number indicates
  ;; how many columns were printed
  (define (pp-expr x nl-col text)
    (cond
      [(equal? x wrapbar)
       (insert-wrapbar text)]
      [else
       (match x
         [`(lambda (,v) ,e)
          (insert-lambda text)
          (insert-variable text v)
          (dp/ct ". ")
          (next-line (+ nl-col 2))
          (pp-expr e (+ nl-col 2) text)
          #f]
         [`(let ((,vs ,rhss) ...) ,body)
          (insert-bold text "let")
          (dp/ct " ")
          (insert-variable text (car vs))
          (dp/ct " = ")
          (pp-expr (car rhss) (+ nl-col (string-length (format "let "))) text)
          (for-each (lambda (v rhs)
                      (next-line (+ nl-col (string-length (format "let "))))
                      (insert-variable text v)
                      (dp/ct " = ")
                      (pp-expr rhs (+ nl-col (string-length (format "let "))) text))
                    (cdr vs)
                    (cdr rhss))
          (next-line (+ nl-col 2))
          (pp-expr body (+ nl-col 2) text)
          #f]
         [`(fix ,v ,e)
          (insert-bold text "fix")
          (dp/ct " ")
          (dp/ct v)
          (dp/ct ". ")
          (next-line (+ nl-col 2))
          (pp-expr e (+ nl-col 2) text)
          #f]
         [`(if ,e1 ,e2 ,e3)
          (insert-bold text "if")
          (dp/ct " ")
          (pp-expr e1 (+ nl-col 3) text)
          (next-line (+ nl-col 2))
          (insert-bold text "then")
          (dp/ct " ")
          (pp-expr e2 (+ nl-col 2 5) text)
          (next-line (+ nl-col 2))
          (insert-bold text "else")
          (dp/ct " ")
          (pp-expr e3 (+ nl-col 2 5) text)
          #f]
         [`(,e1 ,e2)
          (let* ([fst-res
                  (cond
                    [(simple? e1)
                     (pp-expr e1 nl-col text)]
                    [else
                     (comb (dp/ct "(")
                           (pp-expr e1 (+ nl-col 1) text)
                           (dp/ct ")"))])]
                 [break-lines?
                  (or (not fst-res)
                      (>= fst-res 10))]
                 [_ (when break-lines?
                      (next-line nl-col))]
                 [snd-res
                  (cond
                    [(simple? e2)
                     (comb
                      (dp/ct " ")
                      (pp-expr e2 
                               (if break-lines?
                                   (+ nl-col 1)
                                   (+ fst-res nl-col 1))
                               text))]
                    [else
                     (comb (dp/ct " (")
                           (pp-expr e2 
                                    (if break-lines?
                                        (+ nl-col 2)
                                        (+ nl-col fst-res 2))
                                    text)
                           (dp/ct ")"))])])
              (comb
               fst-res
               snd-res))]
         [`(,biop ,e1 ,e2)
          (let* ([fst-res
                  (cond
                    [(simple? e1)
                     (pp-expr e1 nl-col text)]
                    [else
                     (comb
                      (dp/ct "(")
                      (pp-expr e1 (+ nl-col 1) text)
                      (dp/ct ")"))])]
                 [spc1 (if fst-res
                           (dp/ct " ")
                           (begin (next-line nl-col)
                                  #f))]
                 [middle 
                  (case biop
                    [(cons) (dp/ct "::")]
                    [(-->) 
                     (insert-symbol text (string (integer->char 174)))
                     1]
                    [else (dp/ct biop)])]
                 [spc2 (if fst-res
                           (dp/ct " ")
                           (begin (next-line nl-col)
                                  #f))]
                 [snd-res
                  (cond
                    [(simple? e2) 
                     (pp-expr e2 
                              (if fst-res
                                  (+ nl-col fst-res spc1 middle spc2)
                                  nl-col)
                              text)]
                    [else
                     (comb
                      (dp/ct "(")
                      (pp-expr e2 
                               (if fst-res
                                   (+ nl-col fst-res spc1 middle spc2 1)
                                   (+ nl-col 1))
                               text)
                      (dp/ct ")"))])])
            (comb fst-res
                  spc1
                  middle
                  spc2
                  snd-res))]
         [`compile-v1
          (insert-compile text "V" "1")]
         [`compile-v2
          (insert-compile text "V" "2")]
         [`compile-e
          (insert-compile text "e" #f)]
         [`empty 
          (dp/ct "[]")]
         [(? (lambda (x) 
               (and (symbol? x)
                    (memq x keywords))))
          (insert-bold text (symbol->string x))]
         [(? symbol?)
          (insert-variable text x)]
         [else 
          (dp/ct (format "~s" x))])]))
  
  (define keywords '(contract pred dom rng flatp error blame true false))

  (define (insert-lambda text)
    (insert-symbol text "l "))

  (define (insert-compile text arg subscript)
    (let ([sd (make-object style-delta% 'change-family 'script)]
          [b-sd (make-object style-delta% 'change-family 'script)])
      (send b-sd set-delta 'change-bold)
      (+ (insert/style text "C" b-sd)
         (insert/snip text (make-object sub-snip% "e"))
         (insert/style text "(" sd)
         (insert/style text arg #f)
         (if subscript
             (insert/snip text (make-object sub-snip% subscript))
             0)
         (insert/style text ")" sd))))

  (define (insert-wrapbar text)
    (send text insert (make-object wrap-bar%)
          (send text last-position)
          (send text last-position))
    4)
  
  (define (insert-symbol text str)
    (insert/style text str (make-object style-delta% 'change-family 'symbol)))
  
  (define (insert-bold text str)
    (insert/style text str (make-object style-delta% 'change-bold))
    (string-length str))
  
  (define (insert-variable text sym)
    (let ([d (make-object style-delta%)]
          [str (symbol->string sym)])
      (send d set-delta-foreground "forest green")
      (insert/style text str d)
      (string-length str)))
    
  (define wrap-bar%
    (class snip%
      (inherit get-style)
      (define/override (get-extent dc x y wb hb db ab lspace rspace)
        (set-box/f lspace 0)
        (set-box/f rspace 0)
        (let-values ([(w h d a) (send dc get-text-extent "wrap"
                                      (send (get-style) get-font))])
          (set-box/f wb w)
          (set-box/f hb h)
          (set-box/f db d)
          (set-box/f ab a)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret?)
        (let-values ([(w h d a) (send dc get-text-extent "wrap")])
          (send dc draw-text "wrap" x y)
          (send dc draw-line x (+ y 1) (+ x w -1) (+ y 1))))
      
      (super-instantiate ())))

  (define sub-snip%
    (class snip%
      (init-field str)
      (inherit get-style)
      (define/override (get-extent dc x y wb hb db ab lspace rspace)
        (set-box/f lspace 0)
        (set-box/f rspace 0)
        (let-values ([(w h d a) (send dc get-text-extent str
                                      (send (get-style) get-font))])
          (set-box/f wb w)
          (set-box/f hb (+ h (floor (/ h 3))))
          (set-box/f db (- (+ h (floor (/ h 3)))
                           (- h d)))
          (set-box/f ab a)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret?)
        (let-values ([(w h d a) (send dc get-text-extent str)])
          (send dc draw-text str x (+ y (floor (/ h 3))))))
      
      (super-instantiate ())))
  
  (define (set-box/f b v) (when (box? b) (set-box! b v)))
  
  ;; insert/snip : text snip -> number
  ;; returns an approximation to the width of what was inserted
  (define (insert/snip text snip)
    (send text insert snip (send text last-position) (send text last-position))
    1)
  
  ;; insert/style : text string style-delta% -> number
  ;; returns the number of characters in the string
  ;; (an approximation to the width of what was inserted)
  (define (insert/style text str sd)
    (let ([pos (send text last-position)])
      (send text insert str pos pos)
      (when sd
        (send text change-style 
              sd
              pos
              (send text last-position)))
      (string-length str)))
  
  ;; comb : (union #f number) *-> (union #f number)
  ;; sums up its arguments, unless it gets #f,
  ;; in which case it returns #f
  (define (comb . x)
    (if (memq #f x)
        #f
        (apply + x)))
  
  ;; simple : any -> bool
  ;; determines if an expression need parenthesis
  (define (simple? exp)
    (or (not (pair? exp))
        (equal? exp wrapbar)))
  
  ;; next-line : number -> void
  ;; dp/cts a newline and indents to the proper place
  (define (next-line n)
    (dp/ct "\n")
    (let loop ([n n])
      (unless (zero? n)
        (dp/ct " ")
        (loop (- n 1)))))
  
  (define wrapbar
    `(fix
      wrap
      (lambda (ct)
        (lambda (x)
          (lambda (p)
            (lambda (n)
              (if (flatp ct)
                  (if ((pred ct) x)
                      x
                      (blame p))
                  (let ((d (dom ct))
                        (r (rng ct)))
                    (lambda (y)
                      ((((wrap r)
                         (x ((((wrap d) y) n) p)))
                        p)
                       n))))))))))
  
  (define flat-case
    `(();; defns...
      ((((,wrapbar (contract compile-v2)) compile-v1) "p") "n")))
  
  (define ho-case
    `(();; defns...
      ((((,wrapbar (--> compile-v1 compile-v2)) (lambda (x) compile-e)) "p") "n")))
  
  
  (traces lang reductions flat-case pp)
  (traces lang reductions ho-case pp)
  )
