
(module core-layout mzscheme
  (require "loc-wrapper.ss"
           "matcher.ss"
           "reduction-semantics.ss"
           (lib "list.ss")
           (lib "utils.ss" "texpict")
           (lib "mrpict.ss" "texpict")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "struct.ss"))
  
  (provide find-enclosing-loc-wrapper
           lw->pict
           basic-text
           metafunction-text
           default-style
           label-style
           non-terminal-style
           non-terminal-subscript-style
           label-font-size
           default-font-size
           metafunction-font-size
           non-terminal
           literal-style
           metafunction-style
           open-white-square-bracket
           close-white-square-bracket
           just-before
           just-after
           with-unquote-rewriter
           with-compound-rewriter
           with-atomic-rewriter
           STIX?
           
           ;; for test suite
           build-lines
           (struct token (column span))
           (struct string-token (string style))
           (struct pict-token (pict))
           (struct spacer-token ())
           
           current-text)
  
  
  (define STIX? #f)
  
  ;; atomic-rewrite-table : (parameter (listof (list symbol (union string pict))))
  (define atomic-rewrite-table 
    (make-parameter 
     `((... ,(if STIX?
                 (basic-text "\u22ef" (default-style))
                 "..."))
       (hole "[]"))))
  
  (define-syntax (with-atomic-rewriter stx)
    (syntax-case stx ()
      [(_ name transformer e)
       #'(parameterize ([atomic-rewrite-table
                         (cons (list name transformer)
                               (atomic-rewrite-table))])
           e)]))
  
  ;; compound-rewrite-table : (listof lw) -> (listof (union lw pict string))
  (define compound-rewrite-table 
    (make-parameter 
     `((in-hole ,(λ (args)
                   (let ([context (list-ref args 2)]
                         [thing-in-hole (list-ref args 3)])
                     (if (and (lw? thing-in-hole)
                              (equal? (lw-e thing-in-hole) 'hole))
                         (list (blank) context (blank))
                         (list (blank) context "" "[" thing-in-hole "]")))))
       (in-named-hole ,(λ (args)
                         (let ([name (lw-e (list-ref args 2))]
                               [context (list-ref args 3)]
                               [thing-in-hole (list-ref args 4)])
                           (if (and (lw? thing-in-hole)
                                    (equal? (lw-e thing-in-hole) 'hole))
                               (list (blank) context "[]" 
                                     (basic-text (format "~a" name) (non-terminal-subscript-style)))
                               (list (blank) context "" "[" thing-in-hole "]" 
                                     (basic-text (format "~a" name) (non-terminal-subscript-style)))))))
       (hide-hole ,(λ (args)
                     (list (blank)
                           (list-ref args 2)
                           (blank))))
       (hole ,(λ (args)
                (let ([name (lw-e (list-ref args 2))])
                  (list "[]" 
                        (basic-text (format "~a" name) (non-terminal-subscript-style))))))
       (name ,(λ (args)
                (let ([open-paren (list-ref args 0)]
                      [the-name (list-ref args 2)]
                      [close-paren (list-ref args 4)])
                  (list (blank)
                        the-name
                        (blank))))))))
  
  (define-syntax (with-compound-rewriter stx)
    (syntax-case stx ()
      [(_ name transformer e)
       #'(parameterize ([compound-rewrite-table
                         (cons (list name transformer)
                               (compound-rewrite-table))])
           e)]))
  
  (define-syntax (with-unquote-rewriter stx)
    (syntax-case stx ()
      [(_ transformer e)
       #'(parameterize ([current-unquote-rewriter transformer])
           e)]))
  (define current-unquote-rewriter (make-parameter values))
                                    
  
  
  ;; token = string-token | spacer-token | pict-token | align-token
  
  (define-struct token (column span) (make-inspector))
  
  ;; string : string
  ;; style : valid third argument to mrpict.ss's `text' function
  (define-struct (string-token token) (string style) (make-inspector))
  
  ;; width : number
  ;; pict : pict
  (define-struct (pict-token token) (pict) (make-inspector))
  
  ;; spacer : number
  (define-struct (spacer-token token) () (make-inspector))

  ;; pict : pict
  ;; this token always appears at the beginning of a line and its width
  ;; is the x-coordinate of the pict inside itself (which must appear on
  ;; an earlier line)
  (define-struct align-token (pict) (make-inspector))

  (define (lw->pict nts lw)
    (lines->pict 
     (setup-lines 
      (build-lines 
       (if (compiled-lang? nts)
           (language-nts nts)
           nts)
       (apply-rewrites lw)))))
  
  (define (apply-rewrites orig-lw)
    (define (ar/lw an-lw)
      (cond
        [(eq? 'spring an-lw) an-lw]
        [(lw? an-lw)
         (let* ([w-out-term-let (remove-term-let an-lw)]
                [rewritten 
                 (if (lw-unq? w-out-term-let)
                     ((current-unquote-rewriter) w-out-term-let)
                     w-out-term-let)])
           (if (equal? rewritten an-lw)
               (copy-struct lw
                            an-lw
                            [lw-e (ar/e (lw-e an-lw)
                                        (lw-line an-lw)
                                        (lw-line-span an-lw)
                                        (lw-column an-lw)
                                        (lw-column-span an-lw))])
               (ar/lw rewritten)))]))
    
    (define (remove-term-let an-lw)
      (if (lw-unq? an-lw)
          (let ([content (lw-e an-lw)])
            (if (and (pair? content)
                     (pair? (cdr content))
                     (lw? (cadr content))
                     (equal? 'term-let (lw-e (cadr content))))
                (copy-struct lw
                             an-lw
                             [lw-e (lw-e (second-to-last content))])
                an-lw))
          an-lw))
    
    (define (ar/e e line line-span col col-span)
      (cond
        [(and (symbol? e) (assoc e (atomic-rewrite-table)))
         =>
         (λ (m)
           (when (eq? (cadr m) e)
             (error 'apply-rewrites "rewritten version of ~s is still ~s" e e))
           (let ([p (cadr m)])
             (if (procedure? p)
                 (p)
                 p)))]
        [(symbol? e) e]
        [(string? e) e]
        [(pict? e) e]
        [(and (pair? e)
              (lw? (car e))
              (member (lw-e (car e)) '("(" "[" "{")) ;; ensures we're really beginning a sequence
                                                     ;; only useful for typesetting grammars, due to
                                                     ;; the loc-wrappers that it synthesizes
              (pair? (cdr e))
              (lw? (cadr e))
              (assoc (lw-e (cadr e)) (compound-rewrite-table)))
         =>
         (λ (m)
           (let ([rewritten ((cadr m) e)])
             (when (and (pair? rewritten)
                        (pair? (cdr rewritten))
                        (eq? (cadr rewritten) 
                             (cadr e)))
               (error 'apply-rewrites "rewritten version still has symbol of the same name as original: ~s" 
                      (cadr rewritten)))
             (let ([adjusted 
                    (adjust-spacing rewritten 
                                    line line-span col col-span
                                    (lw-e (cadr e)))])
                            (map ar/lw adjusted))))]
        [(and (pair? e)
              (pair? (cdr e))
              (lw? (cadr e))
              (lw-metafunction-name (cadr e)))
         (map ar/lw (adjust-spacing (rewrite-metafunction-app (lw-metafunction-name (cadr e)) e)
                                    line line-span col col-span 
                                    (lw-e (cadr e))))]
        [else
         (map ar/lw e)]))
    (ar/lw orig-lw))
  
  (define (second-to-last l)
    (cond
      [(null? l) (error 'second-to-last "empty list")]
      [(null? (cdr l)) (error 'second-to-last "one element list")]
      [else (let loop ([l (cddr l)]
                       [fst (car l)]
                       [snd (cadr l)])
              (cond
                [(null? l) fst]
                [else (loop (cdr l)
                            snd
                            (car l))]))]))
    
  (define (rewrite-metafunction-app kind lst)
    (case kind
      [(single) 
       (list* (hbl-append
               (metafunction-text (symbol->string (lw-e (cadr lst))))
               (open-white-square-bracket))
              (let loop ([lst (cddr lst)])
                (cond
                  [(null? lst) null]
                  [(null? (cdr lst))
                   (let ([last (car lst)])
                     (list (just-before (close-white-square-bracket) last)))]
                  [else (cons (car lst) (loop (cdr lst)))])))]
      [(multi)
       (cons (hbl-append 
              (metafunction-text (symbol->string (lw-e (cadr lst))))
              (open-white-square-bracket))
             (let loop ([lst (cddr lst)])
               (cond
                 [(null? lst) null]
                 [(null? (cdr lst))
                  (let ([last (car lst)])
                    (list (just-before (close-white-square-bracket) last)))]
                 [(null? (cddr lst))
                  (cons (car lst) (loop (cdr lst)))]
                 [else (list* (car lst) 
                              (basic-text ", " (default-style))
                              (loop (cdr lst)))])))]))
  
  (define (just-before what lw)
    (build-lw (if (symbol? what)
                  (symbol->string what)
                  what)
              (lw-line lw)
              0
              (lw-column lw)
              0))
  
  (define (just-after what lw)
    (build-lw (if (symbol? what)
                  (symbol->string what)
                  what)
              (+ (lw-line lw) (lw-line-span lw))
              0
              (+ (lw-column lw) (lw-column-span lw))
              0))
  
  ;; adjust-spacing : (listof (union string pict loc-wrapper))
  ;;                  number
  ;;                  number
  ;;                  symbol
  ;;               -> (listof loc-wrapper)
  ;; builds loc-wrappers out of the strings in the rewrittens, 
  ;; using the originals around the string in order to find column numbers for the strings
  ;; NB: there is still an issue with this code -- if the rewrite drops stuff that
  ;;     appears at the end of the sequence, blank space will still appear in the final output ...
  ;;     When this is fixed, remove the workaround for the `in-hole' rewriter.
  (define (adjust-spacing in-rewrittens init-line init-line-span init-column init-column-span who)
    (let loop ([rewrittens in-rewrittens]
               [line init-line]
               [column init-column])
      (let* ([to-wrap (collect-non-lws rewrittens)]
             [next-lw (first-lws rewrittens)]
             [after-next-lw (drop-to-lw-and1 rewrittens)]
             [next-lw-line (if next-lw
                               (lw-line next-lw)
                               (+ init-line init-line-span))]
             [next-lw-column (if next-lw
                                 (lw-column next-lw)
                                 (+ init-column init-column-span))])
        ;; error checking
        (cond
          [(= line next-lw-line)
           (when (next-lw-column . < . column)
             (error 'adjust-spacing "for ~a; loc-wrapper takes up too many columns. Expected it to not pass ~a, but it went to ~a"
                    who
                    next-lw-column
                    column))]
          [(next-lw-line . < . line)
           (error 'adjust-spacing "for ~a; last loc-wrapper takes up too many lines. Expected it to not pass line ~a, but it went to ~a"
                  who
                  next-lw-line
                  line)])
        (let* ([next-line (+ next-lw-line
                             (if next-lw
                                 (lw-line-span next-lw)
                                 0))]
               [next-column (+ next-lw-column 
                               (if next-lw
                                   (lw-column-span next-lw)
                                   0))])
          (cond
            [(and after-next-lw (null? to-wrap))
             (cons next-lw (loop after-next-lw next-line next-column))]
            [(and (not after-next-lw) (null? to-wrap))
             '()]
            [else
             (let-values ([(to-wrap1 to-wrap2) (extract-pieces-to-wrap who to-wrap)])
               (let ([new-lw-col
                      (if (= line next-lw-line)
                          column
                          init-column)]
                     [new-lw-col-span
                      (if (= line next-lw-line)
                          (- next-lw-column column)
                          (- next-lw-column init-column))])
                 (list* (build-lw to-wrap1 line 0 new-lw-col 0)
                        (build-lw (blank)
                                           line
                                           (- next-lw-line line)
                                           new-lw-col 
                                           new-lw-col-span)
                        (build-lw to-wrap2 next-lw-line 0 (+ new-lw-col new-lw-col-span) 0)
                        (if after-next-lw
                            (cons next-lw (loop after-next-lw next-line next-column))
                            '()))))])))))
  
  (define (extract-pieces-to-wrap who lst)
    (let ([fst (car lst)])
      (if (pair? (cdr lst))
          (let ([snd (cadr lst)])
            (when (pair? (cddr lst))
              (error 'adjust-spacing 
                     "for ~a; found ~a consecutive loc-wrappers, expected at most 2: ~a"
                     who
                     (length lst)
                     (apply string-append
                            (format "~s" (car lst))
                            (map (λ (x) (format " ~s" x)) (cdr lst)))))
            (values fst snd))
          (values fst (blank)))))
  
  (define (combine-into-loc-wrapper to-wrap)
    (cond
      [(null? to-wrap) (blank)]
      [(null? (cdr to-wrap)) (car to-wrap)]
      [else 
       (apply hbl-append (map make-single-pict to-wrap))]))
  
  (define (make-single-pict x)
    (cond
      [(pict? x) x]
      [(string? x) (basic-text x (default-style))]))
  
  (define (drop-to-lw-and1 lst)
    (let loop ([lst lst])
      (cond
        [(null? lst) #f]
        [else
         (let ([ele (car lst)])
           (if (lw? ele)
               (cdr lst)
               (loop (cdr lst))))])))
    
  (define (collect-non-lws lst)
    (let loop ([lst lst])
      (cond
        [(null? lst) null]
        [else
         (let ([ele (car lst)])
           (if (lw? ele)
               null
               (cons ele (loop (cdr lst)))))])))
  
  (define (first-lws lst)
    (let loop ([lst lst])
      (cond
        [(null? lst) #f]
        [else
         (let ([ele (car lst)])
           (if (lw? ele) 
               ele
               (loop (cdr lst))))])))
  
  (define (build-lines all-nts lw)
    (define initial-column (lw-column lw))
    (define initial-line (lw-line lw))
    (define current-line (lw-line lw))
    (define current-column (lw-column lw))
    (define last-token-spring? #f)
    (define tokens '())
    (define lines '())
    (define (eject line col span atom unquoted?)
      (unless (= current-line line)
        ;; make new lines
        (for-each 
         (λ (x) 
           (set! lines (cons (reverse tokens) lines))
           (set! tokens '()))
         (build-list (max 0 (- line current-line)) (λ (x) 'whatever)))
        
        (set! tokens (cons (make-spacer-token 0 (- col initial-column))
                           tokens))
        
        (set! current-line line)
        (set! current-column col))
      (when (< current-column col)
        (let ([space-span (- col current-column)])
          (set! tokens (cons (make-blank-space-token unquoted?
                                                     (- current-column initial-column)
                                                     space-span)
                             tokens))))
      (set! last-token-spring? #f)
      (set! tokens (append 
                    (reverse
                     (atom->tokens (- col initial-column) span atom all-nts unquoted?))
                    tokens))
      (set! current-column (+ col span)))
    
    (define (make-blank-space-token unquoted? col span)
      (if last-token-spring?
          (make-pict-token col span (blank))
          (let ([str (apply string (build-list span (λ (x) #\space)))])
            (if unquoted?
                (make-pict-token col span (pink-background ((current-text) str 'modern (default-font-size))))
                (make-string-token col span str (default-style))))))
    
    (define (handle-loc-wrapped lw last-line last-column last-span)
      (cond
        [(eq? lw 'spring)
         (set! last-token-spring? #t)]
        [else
         (handle-object (lw-e lw)
                        (lw-line lw)
                        (lw-column lw)
                        (lw-column-span lw)
                        (lw-unq? lw))]))
    
    (define (handle-object obj line col span unquoted?)
      (cond
        [(symbol? obj) (eject line col span obj unquoted?)]
        [(string? obj) (eject line col span obj unquoted?)]
        [(pict? obj) (eject line col span obj unquoted?)]
        [(not obj) (eject line col span (blank) unquoted?)]
        [else
         (for-each (λ (x) (handle-loc-wrapped x line col span))
                   obj)]))
    
    (handle-loc-wrapped lw 0 0 0)
    (set! lines (cons (reverse tokens) lines)) ;; handle last line ejection
    lines)
  
  ;; setup-lines : (listof (listof token)) -> (listof (listof token))
  ;; removes the spacer tokens from the beginning of lines, replacing them with align tokens
  ;; expects the lines to be in reverse order
  (define (setup-lines lines)
    (let loop ([lines lines])
      (cond
        [(null? lines) null]
        [else 
         (let ([line (car lines)]
               [rst (cdr lines)])
           (if (null? line)
               (cons line (loop (cdr lines)))
               (if (spacer-token? (car line))
                   (let ([pict (blank)])
                     (if (andmap null? rst)
                         (cons (cdr line) (loop rst))
                         (let ([rst (split-out (token-span (car line))
                                               pict
                                               rst)])
                           (cons (cons (make-align-token pict) (cdr line))
                                 (loop rst)))))
                   (cons line (loop (cdr lines))))))])))
  
  (define (split-out col pict lines)
    (let ([new-token (make-pict-token col 0 pict)])
      (let loop ([lines lines])
        (cond
          [(null? lines)
           ;; this case can happen when the line in question is to the left of all other lines
           (error 'exchange-spacer "could not find matching line")]
          [else (let ([line (car lines)])
                  (if (null? line)
                      (cons line (loop (cdr lines)))
                      (let ([spacer (car line)])
                        (cond
                          [(not (spacer-token? spacer))
                           (cons (insert-new-token col new-token (token-column spacer) (car lines))
                                 (cdr lines))]
                          [(= (token-span spacer)
                              col)
                           (cons (list* spacer new-token (cdr line))
                                 (cdr lines))]
                          [(> (token-span spacer)
                              col)
                           (cons line (loop (cdr lines)))]
                          [(< (token-span spacer)
                              col)
                           (cons (insert-new-token col new-token (token-column spacer) (car lines))
                                 (cdr lines))]))))]))))
                  
  (define (insert-new-token column-to-insert new-token init-width line)
    (let loop ([line line]
               [column 0])
      (cond
        [(null? line)
         (list new-token)]
        [else
         (let ([tok (car line)])
           (unless (token? tok)
             (error 'insert-new-token "ack ~s" tok))
           (cond
             [(<= column-to-insert (token-column tok))
              (cons new-token line)]
             [(< (token-column tok)
                 column-to-insert
                 (+ (token-column tok) (token-span tok)))
              (append (split-token (- column-to-insert (token-column tok)) tok new-token)
                      (cdr line))]
             [(= column-to-insert (+ (token-column tok) (token-span tok)))
              (list* (car line) new-token (cdr line))]
             [else 
              (cons (car line)
                    (loop (cdr line)
                          (+ (token-column tok) (token-span tok))))]))])))
               
  (define (split-token offset tok new-token)
    (cond
      [(string-token? tok)
       (list (make-string-token (token-column tok)
                                offset
                                (substring (string-token-string tok)
                                           0 offset)
                                (string-token-style tok))
             new-token
             (make-string-token (+ (token-column tok) offset)
                                (- (token-span tok) offset)
                                (substring (string-token-string tok)
                                           offset 
                                           (string-length (string-token-string tok)))
                                (string-token-style tok)))]
      [(pict-token? tok)
       (list new-token)]))
  
  ;; lines->pict : (listof (listof token)) -> pict
  ;; expects the lines to be in order from bottom to top
  (define (lines->pict lines)
    (let loop ([lines lines])
      (cond
        [(null? lines) (blank)]
        [(null? (cdr lines))
         (handle-single-line (car lines) (blank))]
        [else
         (let ([rst (loop (cdr lines))])
           (vl-append rst (handle-single-line (car lines) rst)))])))
  
  (define (handle-single-line line rst)
    (cond
      [(null? line) 
       (let ([h (pict-height (token->pict (make-string-token 0 0 "x" (default-style))))])
         (blank 0 h))]
      [else
       (if (align-token? (car line))
           (let-values ([(x y) (lt-find rst (align-token-pict (car line)))])
             (apply htl-append 
                    (blank x 0)
                    (map token->pict (cdr line))))
           (apply htl-append (map token->pict line)))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  font specs
  ;;
  
  
  (define (token->pict tok)
    (cond
      [(string-token? tok)
       (basic-text (string-token-string tok) (string-token-style tok))]
      [(pict-token? tok) (pict-token-pict tok)]
      [else (error 'token->pict "~s" tok)]))
  
    
  (define (atom->tokens col span atom all-nts unquoted?)
    (cond
      [(pict? atom)
       (list (make-pict-token col span atom))]
      [unquoted?
       (list (make-pict-token col span 
                              (pink-background 
                               ((current-text) (if (string? atom) atom (format "~a" atom))
                                               'modern
                                               (default-font-size)))))]
      [(and (symbol? atom)
            (regexp-match #rx"^([^_]*)_(.*)$" (symbol->string atom)))
       =>
       (λ (m)
         (let* ([first-part (cadr m)]
                [second-part (caddr m)]
                [first-span (- span (string-length first-part))])
           (list 
            (make-string-token col
                               first-span
                               first-part
                               (non-terminal-style))
            (make-string-token (+ col first-span) 
                               (- span first-span)
                               second-part
                               (non-terminal-subscript-style)))))]
      [(or (memq atom all-nts)
           (memq atom '(number variable variable-except variable-not-otherwise-mentioned)))
       (list (make-string-token col span (format "~s" atom) (non-terminal-style)))]
      [(symbol? atom)
       (list (make-string-token col span (symbol->string atom) (literal-style)))]
      [(string? atom)
       (list (make-string-token col span atom (default-style)))]
      [else (error 'atom->tokens "unk ~s" atom)]))
  
  (define (pick-font lst fallback)
    (let ([fl (get-face-list 'all)])
      (let loop ([lst lst])
        (cond
          [(null? lst) fallback]
          [else (if (member (car lst) fl)
                    (car lst)
                    (loop (cdr lst)))]))))
  
  (define current-text (make-parameter text))

  (define (basic-text str style) ((current-text) str style (default-font-size)))
  (define (non-terminal str) ((current-text) str (non-terminal-style) (default-font-size)))
  (define (unksc str) (pink-background ((current-text) str 'modern (default-font-size))))
  (define non-terminal-style (make-parameter '(italic . roman)))
  (define non-terminal-subscript-style (make-parameter `(subscript . ,(non-terminal-style))))
  (define default-style (make-parameter 'roman))
  (define metafunction-style (make-parameter 'swiss))
  (define (metafunction-text str) ((current-text) str (metafunction-style) (metafunction-font-size)))
  (define literal-style (make-parameter 'swiss))
  (define label-style (make-parameter 'swiss))
  (define default-font-size (make-parameter 14))
  (define metafunction-font-size (make-parameter (default-font-size)))
  (define label-font-size (make-parameter 14))
  
  (define (open-white-square-bracket) (white-bracket "["))
  (define (close-white-square-bracket) (white-bracket "]"))
  
  (define (white-bracket str)
    (let ([inset-amt
           (case (default-font-size)
             [(9 10 11 12) -2]
             [else
              (- (floor (max 2 (* 2 (/ (default-font-size) 10)))))])])
      (hbl-append (basic-text str (default-style))
                  (inset (basic-text str (default-style)) inset-amt 0 0 0))))
  
  (define (pink-background p)
    (refocus
     (cc-superimpose 
      (colorize (filled-rectangle (pict-width p)
                                  (pict-height p))
                "pink")
      p)
     p))
  
  (define (add-between i l)
    (cond
      [(null? l) l]
      [else 
       (cons (car l)
             (apply append 
                    (map (λ (x) (list i x)) (cdr l))))]))
  
  
  ;; for use 
  (define (find-enclosing-loc-wrapper lws)
    (let* ([first-line (apply min (map lw-line lws))]
           [last-line (apply min (map (λ (x) (+ (lw-line x) (lw-line-span x))) lws))]
           [last-line-lws (find-lws-with-matching-last-line lws last-line)]
           [last-column (apply max (map (λ (x) (+ (lw-column x) (lw-column-span x))) last-line-lws))]
           [first-column (apply min (map lw-column last-line-lws))])
      (build-lw
       lws
       first-line
       (- last-line first-line)
       first-column
       (- last-column first-column))))
  
  (define (find-lws-with-matching-last-line in-lws line)
    (define lws '())
    
    (define (find/lw lw)
      (cond
        [(eq? lw 'spring) (void)]
        [(lw? lw)
         (when (= line (+ (lw-line lw) (lw-line-span lw)))
           (set! lws (cons lw lws))
           (find/e (lw-e lw)))]))
    
    (define (find/e e)
      (cond
        [(symbol? e) (void)]
        [(string? e) (void)]
        [(pict? e) (void)]
        [else (for-each find/lw e)]))
    
    (find/e in-lws)
    lws)

  )
