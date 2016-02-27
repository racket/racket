;;----------------------------------------------------------------------
;; pattern-matching utilities
;; based on Shriram's pattern matcher for Zodiac

(module sc '#%kernel
  (#%require "stx.rkt" "small-scheme.rkt"
             (for-template (only '#%kernel set!)
                           "ellipses.rkt"))

  ;; Checks whether s is "..."
  (-define (...? s)
           (if (symbol? (syntax-e s))
               (free-identifier=? s (quote-syntax ...))
               #f))

  (-define (wildcard? p)
           (free-identifier=? p (quote-syntax _)))

  ;; memq on a list of identifiers, and
  ;;  nested identifiers
  (-define (stx-memq ssym l)
           (ormap (lambda (p)
                    (and (syntax? p)
                         (bound-identifier=? ssym p)))
                  l))
  
  ;; memq on a list of identifiers and
  ;;  nested identifiers, returns a position
  (-define (stx-memq-pos ssym l)
           (let loop ([p 0][l l])
             (cond
              [(null? l) #f]
              [(and (syntax? (car l))
                    (bound-identifier=? ssym (car l)))
               p]
              [else (loop (add1 p) (cdr l))])))

  ;; Like stx-memq-pos, but goes into nestings to
  ;;  find identifiers at the same nesting.
  (-define (stx-memq*-pos ssym l)
           (let loop ([p 0][l l])
             (cond
              [(null? l) #f]
              [(let loop ([i (car l)][ssym ssym])
                 (if (syntax? i)
                     (if (syntax? ssym)
                         (bound-identifier=? i ssym)
                         #f)
                     (if (pair? ssym)
                         (loop (car i) (car ssym))
                         #f)))
               p]
              [else (loop (add1 p) (cdr l))])))

  ;; For error reporting:
  (-define (pick-specificity e de)
           (if (eq? e de)
               (list e)
               (list e de)))

  ;;----------------------------------------------------------------------
  ;; Input matcher
  
  ;; Takes syntax pattern and a keyword list and produces a
  ;; matcher. A matcher is a function that takes a syntax input
  ;; and produces a pattern-variable environment or #f.
  ;;
  ;; If `just-vars?' is #t, produces the variables instead of a matcher.
  ;; Each variable is nested in the list corresponding to its ellipsis depth
  ;; in the pattern. We call this the "environment prototype". For reporting
  ;; left-to-right errors, we assume that the function will be called with
  ;; `just-vars?' as #t first, to catch errors.
  ;;
  ;; In the pattern-variable environment produced by a matcher,
  ;; a variable under a single ellipsis has a list of matches,
  ;; a variable under two ellipses has a list of list of matches, etc.
  ;; The top-level environment is a list* --- i.e., a list, except that the last
  ;; element is in the cdr of the cons cell for the next-to-last element.
  ;;
  ;; An environment does not contain any indication of how far a
  ;; variable is nested. Uses of the variable should be checked separately
  ;; using an environment prototype. Furthermore, the environment
  ;; does not contain the pattern variables as "keys", since the positions
  ;; can also be determined by the prototype.
  ;;
  (-define (make-match&env/extract-vars who top p k just-vars? phase-param? interp-box s-exp?)
           ;;  The m&e function returns three values. If just-vars? is true,
           ;;  only the first result is used, and it is the variable list.
           ;;  Otherwise, the first result is the code assuming an input bound to `e'.
           ;;  The second result is #t if a variable was used, so that the code
           ;;  produces an environment rather than just a boolean.
           ;;  The last result is #t only when id-is-rest? was #t, and it indicates
           ;;  that the code refers to cap to get context for datum->syntax.
           (-define (m&e p local-top use-ellipses? last? id-is-rest?)
                    (cond
                     [(and use-ellipses? (ellipsis? p))
                      (if (stx-null? (stx-cdr (stx-cdr p)))
                          ;; Simple case: ellipses at the end
                          (let* ([p-head (stx-car p)]
                                 [nestings (get-ellipsis-nestings p-head k)])
                            (let-values ([(match-head mh-did-var? <false>) (m&e p-head p-head #t #f #f)])
                              (if just-vars?
                                  (values (map list nestings) #f #f)
                                  (let ([nest-vars (flatten-nestings nestings (lambda (x) #t))])
                                    (values
                                     (if interp-box
                                         (vector 'ellipses
                                                 match-head
                                                 (length nest-vars)
                                                 last?)
                                         `(lambda (e)
                                            (if (,(if s-exp? 'list? 'stx-list?) e)
                                                ,(let ([b (app-e match-head)])
                                                   (if (equal? b '(list e))
                                                       (if s-exp?
                                                           (if last? 'e '(list e))
                                                           (if last?
                                                               '(stx->list e)
                                                               '(list (stx->list e))))
                                                       (if (null? nest-vars)
                                                           `(andmap (lambda (e) ,b) ,(if s-exp? 'e '(stx->list e)))
                                                           `(let/ec esc
                                                              (let ([l (map (lambda (e) (stx-check/esc ,b esc))
                                                                            ,(if s-exp? 'e '(stx->list e)))])
                                                                (if (null? l)
                                                                    (quote ,(let ([empties (map (lambda (v) '()) nest-vars)])
                                                                              (if last?
                                                                                  (apply list* empties)
                                                                                  empties)))
                                                                    (,(if last? 'stx-rotate* 'stx-rotate) l)))))))
                                                #f)))
                                     mh-did-var?
                                     #f)))))
                          ;; More stuff after ellipses. We need to make sure that
                          ;;  the extra stuff doesn't include any ellipses or a dot 
                          (let ([hd (list (stx-car p) (stx-car (stx-cdr p)))]
                                [rest (stx-cdr (stx-cdr p))])
                            (let-values ([(tail-cnt prop?)
                                          (let loop ([rest rest][cnt 0])
                                            (if (stx-null? rest)
                                                (values cnt #t)
                                                (if (stx-pair? rest)
                                                    (begin
                                                      (when (...? (stx-car rest))
                                                        (raise-syntax-error 
                                                         (syntax-e who)
                                                         "misplaced ellipsis in pattern (follows other ellipsis)"
                                                         top
                                                         (stx-car rest)))
                                                      (loop (stx-cdr rest) (add1 cnt)))
                                                    (values (add1 cnt) #f))))])
                              ;; Like cons case, but with a more elaborate assembly:
                              (let*-values ([(-match-head -mh-did-var? <false>) (if just-vars?
                                                                                    (m&e hd hd use-ellipses? #f #f)
                                                                                    (values #f #f #f))]
                                            [(match-tail mt-did-var? cap?) (m&e rest local-top use-ellipses? 
                                                                                last? #t)]
                                            [(match-head mh-did-var? <false>) (if just-vars?
                                                                                  (values -match-head -mh-did-var? #f)
                                                                                  (m&e hd hd use-ellipses? 
                                                                                       (and last? (not mt-did-var?))
                                                                                       #f))])
                                (if just-vars?
                                    (values (append match-head match-tail) #f #f)
                                    (values
                                     (if interp-box
                                         (vector 'mid-ellipses
                                                 match-head
                                                 match-tail
                                                 tail-cnt
                                                 prop?
                                                 mh-did-var?
                                                 mt-did-var?)
                                         `(lambda (e)
                                            (let-values ([(pre-items post-items ok?) 
                                                          (split-stx-list e ,tail-cnt ,prop?)])
                                              (if ok?
                                                  ,(let ([s (let ([apph (app match-head 'pre-items)]
                                                                  [appt (app match-tail 'post-items)])
                                                              (if mh-did-var?
                                                                  (app-append apph appt)
                                                                  `(if ,apph ,appt #f)))])
                                                     (if (and cap? (not s-exp?))
                                                         (if id-is-rest?
                                                             `(let ([cap (if (syntax? e) e cap)]) ,s)
                                                             `(let ([cap e]) ,s))
                                                         s))
                                                  #f))))
                                     (or mh-did-var? mt-did-var?)
                                     (and cap? id-is-rest?)))))))]
                     [(stx-pair? p)
                      (let ([hd (stx-car p)])
                        (if (and use-ellipses?
                                 (...? hd))
                            (if (and (stx-pair? (stx-cdr p))
                                     (stx-null? (stx-cdr (stx-cdr p))))
                                (let ([dp (stx-car (stx-cdr p))])
                                  (m&e dp dp #f last? #f))
                                (raise-syntax-error 
                                 (syntax-e who)
                                 "misplaced ellipsis in pattern"
                                 top
                                 hd))
                            ;; When just-vars?, do head first for good error ordering.
                            ;; Otherwise, do tail first to find out if it has variables.
                            (let*-values ([(-match-head -mh-did-var? <false>) (if just-vars?
                                                                                  (m&e hd hd use-ellipses? #f #f)
                                                                                  (values #f #f #f))]
                                          [(match-tail mt-did-var? cap?) (m&e (stx-cdr p) local-top use-ellipses? 
                                                                              last? #t)]
                                          [(match-head mh-did-var? <false>) (if just-vars?
                                                                                (values -match-head -mh-did-var? #f)
                                                                                (m&e hd hd use-ellipses? 
                                                                                     (and last? (not mt-did-var?))
                                                                                     #f))])
                              (if just-vars?
                                  (values (append match-head match-tail) #f #f)
                                  (values
                                   (if interp-box
                                       (vector 'pair
                                               match-head
                                               match-tail
                                               mh-did-var?
                                               mt-did-var?)
                                       `(lambda (e)
                                          (if (,(if s-exp? 'pair? 'stx-pair?) e)
                                              ,(let ([s (let ([apph (app match-head (if s-exp? '(car e) '(stx-car e)))]
                                                              [appt (app match-tail (if s-exp? '(cdr e) '(stx-cdr e)))])
                                                          (if mh-did-var?
                                                              (if mt-did-var?
                                                                  (app-append apph appt)
                                                                  `(let ([mh ,apph]) (and mh ,appt mh)))
                                                              `(if ,apph ,appt #f)))])
                                                 (if (and cap? (not s-exp?))
                                                     (if id-is-rest?
                                                         `(let ([cap (if (syntax? e) e cap)]) ,s)
                                                         `(let ([cap e]) ,s))
                                                     s))
                                              #f)))
                                   (or mh-did-var? mt-did-var?)
                                   (and cap? id-is-rest?))))))]
                     [(stx-null? p)
                      (if just-vars?
                          (values null #f #f)
                          (values (if interp-box
                                      '()
                                      (if s-exp?
                                          '(lambda (e) (if (null? e) null #f))
                                          'stx-null/#f))
                                  #f 
                                  #f))]
                     [(identifier? p)
                      (if (stx-memq p k)
                          (if just-vars?
                              (values null #f #f)
                              (values
                               (if interp-box
                                   (let ([pos (let ([pos (let loop ([l (unbox interp-box)]
                                                                    [pos (sub1 (length (unbox interp-box)))])
                                                           (cond
                                                            [(null? l) #f]
                                                            [(bound-identifier=? (car l) p) pos]
                                                            [else (loop (cdr l) (sub1 pos))]))])
                                                (if pos
                                                    pos
                                                    (begin
                                                      (set-box! interp-box (cons p (unbox interp-box)))
                                                      (sub1 (length (unbox interp-box))))))])
                                     pos)
                                   `(lambda (e)
                                      (if (,(if s-exp? 'symbol? 'identifier?) e)
                                          ;; This free-identifier=? can be turned into
                                          ;;  free-transformer-identifier=? by an
                                          ;;  enclosing binding.
                                          (if (free-identifier=? e (,(if s-exp? 'quote 'quote-syntax) ,p))
                                              null
                                              #f)
                                          #f)))
                               #f
                               #f))
                          (if (and use-ellipses?
                                   (...? p))
                              (raise-syntax-error 
                               (syntax-e who)
                               "misplaced ellipsis in pattern"
                               top
                               p)
                              (if (wildcard? p)
                                  ;; Wildcard
                                  (if just-vars?
                                      (values null #f #f)
                                      (values
                                       (if interp-box
                                           #f
                                           `(lambda (e) null))
                                       #f
                                       #f))
                                  ;; Pattern variable
                                  (if just-vars?
                                      (values (list p) #f #f)
                                      (values
                                       (if interp-box
                                           (vector 'bind last? id-is-rest?)
                                           (let ([wrap (if last?
                                                           (lambda (x) `(lambda (e) ,x))
                                                           (lambda (x) `(lambda (e) (list ,x))))])
                                             (if (and id-is-rest? (not s-exp?))
                                                 (wrap '(datum->syntax cap e cap))
                                                 (wrap 'e))))
                                       #t
                                       id-is-rest?)))))]
                     [(stx-vector? p #f)
                      (let ([l (vector->list (syntax-e p))])
                        ;; If no top-level ellipses, match one by one:
                        (if (and (not just-vars?)
                                 (or (not use-ellipses?)
                                     (andmap (lambda (x) (not (...? x))) l)))
                            ;; Match one-by-one:
                            ;; Do tail first to find out if it has variables.
                            (let ([len (vector-length (syntax-e p))])
                              (let loop ([pos len][did-var? (not last?)][body null])
                                (if (zero? pos)
                                    (values 
                                     (if interp-box
                                         (vector 'vector len body)
                                         `(lambda (e)
                                            (if ,(if s-exp?
                                                     `(and (vector? e) (= ,len (vector-length e)))
                                                     `(stx-vector? e ,len))
                                                ,body
                                                #f)))
                                     did-var?
                                     #f)
                                    (let-values ([(match-elem elem-did-var? <false>) 
                                                  (let ([e (vector-ref (syntax-e p) (sub1 pos))])
                                                    (m&e e e use-ellipses? (not did-var?) #f))])
                                      (loop (sub1 pos)
                                            (or did-var? elem-did-var?)
                                            (if interp-box
                                                (cons (cons match-elem elem-did-var?) body)
                                                (let ([app-elem (app match-elem `(,(if s-exp? 'vector-ref 'stx-vector-ref)
                                                                                  e 
                                                                                  ,(sub1 pos)))])
                                                  (if (null? body)
                                                      app-elem
                                                      (if elem-did-var?
                                                          (app-append app-elem body)
                                                          `(if ,app-elem ,body #f))))))))))
                            ;; Match as a list:
                            (let-values ([(match-content did-var? <false>) (m&e l p use-ellipses? last? #f)])
                              (if just-vars?
                                  (values match-content #f #f)
                                  (values
                                   (if interp-box
                                       (vector 'veclist match-content)
                                       `(lambda (e)
                                          (if ,(if s-exp?
                                                   '(vector? e)
                                                   '(stx-vector? e #f))
                                              ,(app match-content `(vector->list ,(if s-exp? 'e '(syntax-e e))))
                                              #f)))
                                   did-var?
                                   #f)))))]
                     [(stx-box? p)
                      (let*-values ([(content) (unbox (syntax-e p))]
                                    [(match-content did-var? <false>) (m&e content content use-ellipses? last? #f)])
                        (if just-vars?
                            (values match-content #f #f)
                            (values
                             (if interp-box
                                 (vector 'box match-content)
                                 `(lambda (e)
                                    (if ,(if s-exp? '(box? e) '(stx-box? e))
                                        ,(app match-content `(unbox ,(if s-exp? 'e '(syntax-e e))))
                                        #f)))
                             did-var?
                             #f)))]
                     [(and (syntax? p)
                           (prefab-struct-key (syntax-e p)))
                      =>
                      (lambda (key)
                        (let ([l (vector->list (struct->vector (syntax-e p)))])
                          ;; Match as a list:
                          (let-values ([(match-content did-var? <false>) (m&e (cdr l) p use-ellipses? last? #f)])
                            (if just-vars?
                                (values match-content #f #f)
                                (values
                                 (if interp-box
                                     (vector 'prefab key match-content)
                                     `(lambda (e)
                                        (if ,(if s-exp?
                                                 `(equal? ',key (prefab-struct-key e))
                                                 `(stx-prefab? ',key e))
                                            ,(app match-content '(cdr (vector->list (struct->vector (syntax-e e)))))
                                            #f)))
                                 did-var?
                                 #f)))))]
                     [else
                      (if just-vars?
                          (values null #f #f)
                          (values
                           (if interp-box
                               (vector 'quote (syntax-e p))
                               `(lambda (e)
                                  (if ,(let ([test `(equal? (quote ,(syntax-e p)) (syntax-e e))])
                                         (if id-is-rest? ; might get a syntax pair
                                             `(and (syntax? e) ,test)
                                             test))
                                      null
                                      #f)))
                           #f
                           #f))]))
           (let-values ([(r did-var? <false>) (m&e p p #t #t #f)])
             (if just-vars?
                 ;; Look for duplicate uses of variable names:
                 (let ([ht (make-hasheq)])
                   (let loop ([r r])
                     (cond
                      [(syntax? r)
                       (let ([l (hash-ref ht (syntax-e r) null)])
                         (when (ormap (lambda (i) (bound-identifier=? i r)) l)
                           (raise-syntax-error 
                            (syntax-e who)
                            "variable used twice in pattern"
                            top
                            r))
                         (hash-set! ht (syntax-e r) (cons r l)))]
                      [(pair? r)
                       (loop (car r))
                       (loop (cdr r))]
                      [else (void)]))
                   r)
                 ;; A common trivial case is just return the expression
                 (if (equal? r '(lambda (e) e))
                     (if phase-param?
                         '(lambda (e free-identifier=?) e)
                         '(lambda (e) e))
                     (if interp-box
                         r
                         `(lambda (e ,@(if phase-param?
                                           '(free-identifier=?) 
                                           null))
                            ,(app-e r)))))))

  (-define (make-match&env who top p k phase-param? s-exp?)
           (make-match&env/extract-vars who top p k #f phase-param? #f s-exp?))
  
  (-define (get-match-vars who top p k)
           (make-match&env/extract-vars who top p k #t #f #f #f))

  (-define (make-interp-match p keys interp-box s-exp?)
           (make-match&env/extract-vars (quote-syntax interp) 
                                        #f p 
                                        keys
                                        #f #f interp-box s-exp?))
  
  ;; Create an S-expression that applies
  ;; rest to `e'. Optimize ((lambda (e) E) e) to E.
  (-define (app-e rest)
           (if (and (pair? rest)
                    (eq? (car rest) 'lambda)
                    (equal? (cadr rest) '(e)))
               (caddr rest)
               `(,rest e)))

  ;; Create an S-expression that applies
  ;; rest to e.
  (-define (app rest e)
           (if (and (pair? rest)
                    (eq? (car rest) 'lambda)
                    (equal? (cadr rest) '(e)))
               (let ([r (caddr rest)])
                 ;; special (common) case: body is `e' or `(list e)'
                 (cond
                  [(eq? r 'e)
                   e]
                  [(and (pair? r)
                        (eq? (car r) 'list)
                        (pair? (cdr r))
                        (eq? (cadr r) 'e)
                        (null? (cddr r)))
                   `(list ,e)]
                  [else
                   `(,rest ,e)]))
               `(,rest ,e)))

  ;; Create an S-expression that appends
  ;; e1 and e2. Optimize...
  (-define (app-append e1 e2)
           (if (and (pair? e1)
                    (eq? (car e1) 'list)
                    (pair? (cdr e1))
                    (null? (cddr e1)))
               `(cons/#f ,(cadr e1) ,e2)
               `(append/#f ,e1 ,e2)))

  ;; ----------------------------------------------------------------------
  ;; Output generator

  ;; Takes a syntax pattern, an environment prototype, and
  ;; a keyword symbol list, and produces an expander
  ;; that takes an environment and produces syntax.
  ;;
  ;; If the environment prototype is #f, it produces a list of
  ;; variables used in the pattern, instead. This is useful for
  ;; determining what kind of environment (and prototype) to construct
  ;; for the pattern.
  ;;
  ;; An environment for an expander is a list*; see the note above,
  ;; under "Input Matcher", for details.
  ;;
  (-define (make-pexpand p proto-r k dest s-exp?)
           (-define top p)
           ;; Helper function: avoid generating completely new symbols
           ;;  for substitution. Instead, try to generate normal symbols
           ;;  with a standard prefix, so that the symbols can be shared.
           (-define sub-gensym (let ([cnt 0]
                                     [prefix (let pfx-loop ([pfx "_pat"])
                                               (if (let loop ([p p])
                                                     (cond
                                                      [(symbol? p)
                                                       (let ([s (symbol->string p)])
                                                         (and ((string-length s) . > . (string-length pfx))
                                                              (string=? pfx (substring s 0 (string-length pfx)))))]
                                                      [(syntax? p) (loop (syntax-e p))]
                                                      [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                                                      [(vector? p) (loop (vector->list p))]
                                                      [(box? p) (loop (unbox p))]
                                                      [(struct? p) (loop (struct->vector p))]
                                                      [else #f]))
                                                   (pfx-loop (string-append "_" pfx))
                                                   pfx))])
                                 (lambda ()
                                   (set! cnt (add1 cnt))
                                   (string->symbol (format "~a~a" prefix cnt)))))
           ;; The pattern expander:
           (-define (expander p proto-r local-top use-ellipses? use-tail-pos hash! need-list?)
                    (cond
                     [(and use-ellipses? (ellipsis? p))
                      (let*-values ([(p-head) (stx-car p)]
                                    [(el-count rest-p last-el)
                                     (let loop ([p (stx-cdr (stx-cdr p))][el-count 0][last-el (stx-car (stx-cdr p))])
                                       (if (and (stx-pair? p)
                                                (...? (stx-car p)))
                                           (loop (stx-cdr p) (add1 el-count) (stx-car p))
                                           (values el-count p last-el)))]
                                    [(p-head) (let loop ([el-count el-count])
                                                (if (zero? el-count)
                                                    p-head
                                                    (datum->syntax
                                                     #f
                                                     (list (loop (sub1 el-count)) (quote-syntax ...)))))]
                                    [(nestings) (and proto-r (get-ellipsis-nestings p-head k))])
                        (when (null? nestings)
                          (apply
                           raise-syntax-error 
                           'syntax
                           "no pattern variables before ellipsis in template"
                           (pick-specificity
                            top
                            last-el)))
                        (let* ([proto-rr+deep?s (and proto-r
                                                     (map (lambda (nesting)
                                                            (ellipsis-sub-env nesting proto-r top local-top))
                                                          nestings))]
                               [proto-rr-deep (and proto-r
                                                   ;; the ones that we had to unwrap:
                                                   (let loop ([l proto-rr+deep?s])
                                                     (cond
                                                      [(null? l) null]
                                                      [(cdar l) (loop (cdr l))]
                                                      [else (cons (caar l) (loop (cdr l)))])))]
                               [proto-rr-shallow (and proto-r
                                                      ;; the ones that we leave alone for these ellipsis:
                                                      (let loop ([l proto-rr+deep?s])
                                                        (cond
                                                         [(null? l) null]
                                                         [(cdar l) (cons (caar l) (loop (cdr l)))]
                                                         [else (loop (cdr l))])))]
                               [__ (unless (null? proto-rr-shallow)
                                     (when (null? proto-rr-deep)
                                       (apply
                                        raise-syntax-error 
                                        'syntax
                                        "too many ellipses in template"
                                        (pick-specificity
                                         top
                                         last-el))))]
                               [rest (expander rest-p proto-r local-top #t use-tail-pos hash! need-list?)]
                               [ehead (expander p-head (and proto-r (append proto-rr-shallow proto-rr-deep)) p-head #t #f hash! 
                                                (or need-list? (positive? el-count)))])
                          (if proto-r
                              `(lambda (r)
                                 ,(let ([pre (let ([deeps
                                                    (let ([valses
                                                           ;; Generate one binding per nested use. This will duplicate
                                                           ;; bindings if a pattern variable is used multiple times; that's
                                                           ;; good if the uses are in different nesting levels (which could be
                                                           ;; ok if there are extra ellipses around them), but it might also
                                                           ;; create redundant entries.
                                                           (map (lambda (var)
                                                                  (apply-list-ref 'r (stx-memq*-pos (list var) proto-r) use-tail-pos))
                                                                proto-rr-deep)])
                                                      (cond
                                                       [(and (= 1 (length valses))
                                                             (= 0 el-count)
                                                             (null? proto-rr-shallow)
                                                             (equal? ehead '(lambda (r) (car r))))
                                                        ;; Common case: one item in list, no map needed:
                                                        (car valses)]
                                                       [(and (= 2 (length valses))
                                                             (= 0 el-count)
                                                             (null? proto-rr-shallow)
                                                             (equal? ehead '(lambda (r) (list (car r) (cadr r)))))
                                                        ;; Another common case: a maintained pair
                                                        `(map 
                                                          (lambda (a b) (list a b))
                                                          ,@valses)]
                                                       [else
                                                        ;; General case:
                                                        (letrec ([wrap (lambda (expr el-count)
                                                                         (if (zero? el-count)
                                                                             expr
                                                                             (wrap `(apply append ,expr)
                                                                                   (sub1 el-count))))])
                                                          (wrap
                                                           `(map 
                                                             (lambda vals
                                                               (,ehead 
                                                                ,(if (null? proto-rr-shallow)
                                                                     'vals
                                                                     '(append shallows vals))))
                                                             ,@valses)
                                                           el-count))]))])
                                               (if (null? proto-rr-shallow)
                                                   deeps
                                                   `(let ([shallows
                                                           (list ,@(map (lambda (var)
                                                                          (apply-list-ref 'r (stx-memq*-pos var proto-r) use-tail-pos))
                                                                        proto-rr-shallow))])
                                                      ,deeps)))]
                                        [post (apply-to-r rest)])
                                    (let ([v (if (eq? post 'null)
                                                 pre
                                                 `(append ,pre ,post))])
                                      (if (and (not need-list?) (syntax? p) (not s-exp?))
                                          ;; Keep srcloc, properties, etc.:
                                          (let ([small-dest (datum->syntax p
                                                                           'dest
                                                                           p
                                                                           p)])
                                            `(datum->syntax/shape (quote-syntax ,small-dest) ,v))
                                          v))))
                              ;; variables were hashed
                              (void))))]
                     [(stx-pair? p)
                      (let ([hd (stx-car p)])
                        (if (and use-ellipses?
                                 (...? hd))
                            (if (and (stx-pair? (stx-cdr p))
                                     (stx-null? (stx-cdr (stx-cdr p))))
                                (let ([dp (stx-car (stx-cdr p))])
                                  (expander dp proto-r dp #f use-tail-pos hash! need-list?))
                                (raise-syntax-error 
                                 'syntax
                                 "misplaced ellipsis in template"
                                 top
                                 hd))
                            (let ([ehd (expander hd proto-r hd use-ellipses? use-tail-pos hash! #f)]
                                  [etl (expander (stx-cdr p) proto-r local-top use-ellipses? use-tail-pos hash! need-list?)])
                              (if proto-r
                                  `(lambda (r)
                                     ,(apply-cons p (apply-to-r ehd) (apply-to-r etl) p sub-gensym s-exp?))
                                  ;; variables were hashed
                                  (void)))))]
                     [(stx-vector? p #f)
                      (let ([e (expander (vector->list (syntax-e p)) proto-r p use-ellipses? use-tail-pos hash! #t)])
                        (if proto-r
                            `(lambda (r)
                               (list->vector (,(if s-exp? 'values 'stx->list) ,(apply-to-r e))))
                            ;; variables were hashed
                            (void)))]
                     [(stx-box? p)
                      (let ([e (expander (unbox (syntax-e p)) proto-r p use-ellipses? use-tail-pos hash! #t)])
                        (if proto-r
                            `(lambda (r)
                               (box (,(if s-exp? 'values 'syntax-e) ,(apply-to-r e))))
                            ;; variables were hashed
                            (void)))]
                     [(and (syntax? p)
                           (struct? (syntax-e p))
                           (prefab-struct-key (syntax-e p)))
                      (let ([e (expander (cdr (vector->list (struct->vector (syntax-e p)))) proto-r p use-ellipses? use-tail-pos hash! #t)])
                        (if proto-r
                            `(lambda (r)
                               (apply make-prefab-struct ',(prefab-struct-key (syntax-e p)) 
                                      (,(if s-exp? 'values 'stx->list) ,(apply-to-r e))))
                            ;; variables were hashed
                            (void)))]
                     [(identifier? p)
                      (if (stx-memq p k) 
                          (if proto-r 
                              `(lambda (r) (,(if s-exp? 'quote 'quote-syntax) ,p))
                              (void))
                          (if proto-r
                              (let ((x (stx-memq p proto-r)))
                                (if x 
                                    `(lambda (r) ,(apply-list-ref 'r (stx-memq-pos p proto-r) use-tail-pos))
                                    (begin
                                      (when (and use-ellipses?
                                                 (...? p))
                                        (raise-syntax-error 
                                         'syntax
                                         "misplaced ellipsis in template"
                                         top
                                         p))
                                      (check-not-pattern p proto-r)
                                      `(lambda (r) (,(if s-exp? 'quote 'quote-syntax) ,p)))))
                              (unless (and (...? p)
                                           use-ellipses?)
                                (hash! p))))]
                     [(null? p) 
                      ;; Not syntax, so avoid useless syntax info
                      (if proto-r 
                          `(lambda (r) null)
                          (void))]
                     [else (if proto-r 
                               `(lambda (r) (,(if s-exp? 'quote 'quote-syntax) ,p))
                               (void))]))
           (let* ([ht (if proto-r
                          #f
                          (make-hasheq))]
                  [in-order null] ; same content as ht, but in deterministic order
                  [l (expander p proto-r p #t
                               (and proto-r (sub1 (length proto-r)))
                               (if proto-r
                                   #f
                                   (lambda (r)
                                     (let ([l (hash-ref ht (syntax-e r) null)])
                                       (let ([pr (and (pair? l)
                                                      (ormap (lambda (i) 
                                                               (and (bound-identifier=? (mcar i) r) i))
                                                             l))])
                                         (if pr
                                             (set-mcdr! pr (cons r (mcdr pr)))
                                             (let ([pr (mcons r (list r))])
                                               (set! in-order (cons pr in-order))
                                               (hash-set! ht (syntax-e r) (cons pr l))))))))
                               #f)])
             (if proto-r
                 `(lambda (r)
                    ,(let ([main (let ([build (apply-to-r l)])
                                   (if (or s-exp?
                                           (and (pair? build)
                                                (eq? (car build) 'pattern-substitute)))
                                       build
                                       (let ([small-dest ;; In case dest has significant structure...
                                              (and dest (datum->syntax
                                                         dest
                                                         'dest
                                                         dest
                                                         dest))])
                                         `(datum->syntax/shape (quote-syntax ,small-dest)
							       ,build))))])
                       (if (multiple-ellipsis-vars? proto-r)
                           `(catch-ellipsis-error
                             (lambda () ,main)
                             (quote ,p)
                             ;; This is a trick to minimize the syntax structure we keep:
                             (quote-syntax ,(datum->syntax #f '... p)))
                           main)))
                 (let ([l in-order])
                   (values
                    ;; Get list of unique vars:
                    (map mcar l)
                    ;; All ids, including duplicates:
                    (map mcdr l))))))

  ;; apply-to-r creates an S-expression that applies
  ;; rest to `r', but it also optimizes ((lambda (r) E) r)
  ;; as simply E.
  (-define (apply-to-r rest)
           (if (and (pair? rest)
                    (eq? (car rest) 'lambda)
                    (equal? (cadr rest) '(r)))
               (caddr rest)
               `(,rest r)))

  ;; creates an S-expression that conses h and t,
  ;; with optimizations. If h and t are quoted
  ;; versions of the car and cdr of p, then return
  ;; a quoted as the "optimization" --- one that
  ;; is necessary to preserve the syntax wraps
  ;; associated with p.
  (-define (apply-cons stx h t p sub-gensym s-exp?)
           (cond
            [(and (pair? h)
                  (if s-exp?
                      (eq? (car h) 'quote)
                      (eq? (car h) 'quote-syntax))
                  (eq? (cadr h) (stx-car p))
                  (or (eq? t 'null)
                      (and
                       (pair? t)
                       (eq? (car t) (car h))
                       (eq? (cadr t) (stx-cdr p)))))
             `(,(if s-exp? 'quote 'quote-syntax) ,p)]
            [(and (pair? t)
                  (eq? (car t) 'pattern-substitute))
             ;; fold h into the existing pattern-substitute:
             (cond
              [(and (pair? h)
                    (or (eq? (car h) 'quote-syntax)
                        (eq? (car h) 'quote))
                    (eq? (cadr h) (stx-car p)))
               ;; Just extend constant part:
               `(pattern-substitute
                 (,(if s-exp? 'quote 'quote-syntax)
                  ,(let ([v (cons (cadr h) (cadadr t))])
                     ;; We exploit the fact that we're
                     ;;  building an S-expression to
                     ;;  preserve the source's distinction
                     ;;  between (x y) and (x . (y)).
                     (if (syntax? stx)
                         (datum->syntax stx
                                        v
                                        stx
                                        stx
                                        stx)
                         v)))
                 . ,(cddr t))]
              [(and (pair? h)
                    (eq? (car t) #| = 'pattern-substitute |# (car h)))
               ;; Combine two pattern substitutions:
               `(pattern-substitute
                 (,(if s-exp? 'quote 'quote-syntax)
                  ,(let ([v (cons (cadadr h) (cadadr t))])
                     (if (syntax? stx)
                         (datum->syntax stx
                                        v
                                        stx
                                        stx
                                        stx)
                         v)))
                 ,@(cddr h) ;; <-- WARNING: potential quadratic expansion
                 . ,(cddr t))]
              [else
               ;; General case: add a substitution:
               (let* ([id (sub-gensym)]
                      [expr (cons id (cadadr t))]
                      [expr (if (syntax? stx)
                                (datum->syntax stx
                                               expr
                                               stx
                                               stx
                                               stx)
                                expr)])
                 `(pattern-substitute
                   (,(if s-exp? 'quote 'quote-syntax) ,expr)
                   ,id ,h
                   . ,(cddr t)))])]
            [(not s-exp?)
             (cond
              [(eq? t 'null)
               (apply-cons stx h 
                           `(pattern-substitute (quote-syntax ()))
                           p
                           sub-gensym
                           s-exp?)]
              
              [(and (pair? t)
                    (eq? (car t) 'quote-syntax)
                    (stx-smaller-than? (cdr t) 10))
               ;; Shift into `pattern-substitute' mode with an intitial constant.
               ;; (Only do this for small constants, so we don't traverse
               ;; big constants when looking for substitutions.)
               (apply-cons stx h 
                           `(pattern-substitute ,t)
                           p
                           sub-gensym
                           s-exp?)]
              [else
               ;; Shift into `pattern-substitute' with an initial substitution:
               (apply-cons stx h
                           (let ([id (sub-gensym)])
                             `(pattern-substitute (quote-syntax ,id)
                                                  ,id ,t))
                           p
                           sub-gensym
                           s-exp?)])]
            [else
             ;; In S-expression mode, `cons' on, but collapse to `list'
             ;; or `list*' if possible:
             (cond
              [(eq? t 'null)
               (list 'list h)]
              [(and (pair? t)
                    (eq? (car t) 'list))
               (list* 'list h (cdr t))]
              [(and (pair? t)
                    (or (eq? (car t) 'list*)
                        (eq? (car t) 'cons)))
               (list* 'list* h (cdr t))]
              [else
               (list 'cons h t)])]))
            
  (-define (stx-smaller-than? stx sz)
           (sz . > . (stx-size stx (add1 sz))))

  (-define (stx-size stx up-to)
           (cond
            [(up-to . < . 1) 0]
            [(syntax? stx) (stx-size (syntax-e stx) up-to)]
            [(pair? stx) (let ([s1 (stx-size (car stx) up-to)])
                           (+ s1 (stx-size (cdr stx) (- up-to s1))))]
            [(vector? stx) (stx-size (vector->list stx) up-to)]
            [(struct? stx) (stx-size (struct->vector stx) up-to)]
            [(box? stx) (add1 (stx-size (unbox stx) (sub1 up-to)))]
            [else 1]))

  ;; Generates a list-ref expression; if use-tail-pos
  ;;  is not #f, then the argument list is really a list*
  ;;  (see the note under "Input Matcher") and in that case
  ;;  use-tail-pos is a number indicating the list-tail
  ;;  position of the last element
  (-define (apply-list-ref e p use-tail-pos)
           (cond
            [(and use-tail-pos (= p use-tail-pos))
             (cond
              [(eq? p 0) e]
              [(eq? p 1) `(cdr ,e)]
              [(eq? p 2) `(cddr ,e)]
              [(eq? p 3) `(cdddr ,e)]
              [(eq? p 4) `(cddddr ,e)]
              [else `(list-tail ,e ,p)])]
            [(eq? p 0) `(car ,e)]
            [(eq? p 1) `(cadr ,e)]
            [(eq? p 2) `(caddr ,e)]
            [(eq? p 3) `(cadddr ,e)]
            [else `(list-ref ,e ,p)]))

  ;; Returns a list that nests a pattern variable as deeply as it
  ;; is ellipsed. Escaping ellipses are detected.
  (-define get-ellipsis-nestings
           (lambda (p k)
             (let sub ([p p][use-ellipses? #t])
               (cond 
                [(and use-ellipses? (ellipsis? p))
                 (let-values ([(rest nest)
                               (let loop ([p (stx-cdr (stx-cdr p))][nest list])
                                 (if (and (stx-pair? p)
                                          (...? (stx-car p)))
                                     (loop (stx-cdr p) (lambda (x) (list (nest x))))
                                     (values p nest)))])
                   (let ([subs (sub (stx-car p) #t)])
                     (append (map nest subs)
                             (sub rest #t))))]
                [(stx-pair? p) 
                 (let ([hd (stx-car p)])
                   (if (and use-ellipses?
                            (identifier? hd)
                            (...? hd)
                            (stx-pair? (stx-cdr p)))
                       (sub (stx-car (stx-cdr p)) #f)
                       (append (sub (stx-car p) use-ellipses?) 
                               (sub (stx-cdr p) use-ellipses?))))]
                [(identifier? p)
                 (if (or (stx-memq p k) 
                         (wildcard? p))
                     '() 
                     (list p))]
                [(stx-vector? p #f)
                 (sub (vector->list (syntax-e p)) use-ellipses?)]
                [(stx-box? p)
                 (sub (unbox (syntax-e p)) use-ellipses?)]
                [(and (syntax? p)
                      (prefab-struct-key (syntax-e p)))
                 (sub (cdr (vector->list (struct->vector (syntax-e p)))) use-ellipses?)]
                [else '()]))))

  ;; Checks whether the given nesting matches a nesting in the
  ;; environment prototype, returning the prototype entry if it is
  ;; found, and signaling an error otherwise. If the prototype 
  ;; entry should be unwrapped by one, it is, and the resulting
  ;; prototype is paired with #f. Otherwise, the prototype is left
  ;; alone and paired with #t. There may be multiple matches; in that 
  ;; case, prefer unwrapping to not unwrapping (because the other one
  ;; must be for a different sub-template nuder a shared ellipsis).
  (-define ellipsis-sub-env
           (lambda (nesting proto-r src detail-src)
             (let ([vs (map (lambda (proto)
                              (let ([start (if (pair? proto)
                                               (car proto)
                                               proto)])
                                (let loop ([c start] [n nesting] [unwrap? (pair? proto)])
                                  (cond
                                   [(and (pair? c) (pair? n))
                                    (loop (car c) (car n) #t)]
                                   [(pair? n)
                                    (loop c (car n) #f)]
                                   [(and (syntax? c) (syntax? n))
                                    (if (bound-identifier=? c n)
                                        (cons (if unwrap? start proto)
                                              (not unwrap?))
                                        #f)]
                                   [else #f]))))
                            proto-r)])
               (unless (ormap values vs)
                 (apply
                  raise-syntax-error 
                  'syntax
                  "too few ellipses for pattern variable in template"
                  (pick-specificity
                   src
                   (let loop ([n nesting])
                     (if (syntax? n)
                         n
                         (loop (car n)))))))
               (or (ormap (lambda (v) (and v (not (cdr v)) v)) vs)
                   (ormap values vs)))))

  (-define (extract-vars proto-r)
           (map (lambda (i)
                  (let loop ([i i])
                    (if (syntax? i)
                        i
                        (loop (car i)))))
                proto-r))

  ;; Checks that a variable is not in the prototype
  ;; environment, and specifically not an ellipsed
  ;; variable.
  (-define (check-not-pattern ssym proto-r)
           (for-each (lambda (p)
                       (when (pair? p)
                         (let loop ([l (car p)])
                           (cond
                            [(syntax? l)
                             (when (bound-identifier=? l ssym)
                               (raise-syntax-error 
                                'syntax
                                "missing ellipsis with pattern variable in template"
                                ssym))]
                            [else (loop (car l))]))))
                     proto-r))

  ;; Tests if x is an ellipsing pattern of the form
  ;;   (blah ... . blah2)
  (-define (ellipsis? x)
           (and (stx-pair? x) 
                (let ([d (stx-cdr x)])
                  (and (stx-pair? d) 
                       (...? (stx-car d))
                       (not (...? (stx-car x)))))))

  ;; Takes an environment prototype and removes
  ;; the ellipsis-nesting information.
  (-define (flatten-nestings nestings filter?)
           (let loop ([nestings nestings])
             (if (null? nestings)
                 null
                 (if (filter? (car nestings))
                     (cons (let loop ([nesting (car nestings)])
                             (if (syntax? nesting)
                                 nesting
                                 (loop (car nesting))))
                           (loop (cdr nestings)))
                     (loop (cdr nestings))))))

  (-define (multiple-ellipsis-vars? proto-r)
           (let loop ([proto-r proto-r])
             (cond
              [(null? proto-r) #f]
              [(pair? (car proto-r))
               (let loop ([proto-r (cdr proto-r)])
                 (cond
                  [(null? proto-r) #f]
                  [(pair? (car proto-r))
                   #t]
                  [else (loop (cdr proto-r))]))]
              [else (loop (cdr proto-r))])))

  (-define (no-ellipses? stx)
           (cond
            [(stx-pair? stx)
             (and (no-ellipses? (stx-car stx))
                  (no-ellipses? (stx-cdr stx)))]
            [(identifier? stx)
             (not (...? stx))]
            [else #t]))

  (-define (raise-pattern-error self stx)
           (if (identifier? stx)
               (raise-syntax-error
                #f
                "pattern variable cannot be used outside of a template"
                stx)
               (raise-syntax-error
                #f
                "pattern variable cannot be used outside of a template"
                stx
                (if (free-identifier=? (quote-syntax set!) (stx-car stx))
                    (stx-car (stx-cdr stx))
                    (stx-car stx)))))

  ;; Structure for communicating first-order pattern variable information:
  (define-values (struct:syntax-mapping -make-syntax-mapping -syntax-mapping? syntax-mapping-ref syntax-mapping-set!)
    (make-struct-type 'syntax-mapping #f 2 0 #f null (current-inspector) raise-pattern-error))
  (-define (make-syntax-mapping depth valvar)
           (make-set!-transformer (-make-syntax-mapping depth valvar)))
  (-define (syntax-pattern-variable? v)
           (and (set!-transformer? v)
                (-syntax-mapping? (set!-transformer-procedure v))))
  (-define (syntax-mapping-depth v)
           (syntax-mapping-ref (set!-transformer-procedure v) 0))
  (-define (syntax-mapping-valvar v)
           (syntax-mapping-ref (set!-transformer-procedure v) 1))

  ;; Ditto for S-expression patterns:
  (define-values (struct:s-exp-mapping -make-s-exp-mapping -s-exp-mapping? s-exp-mapping-ref s-exp-mapping-set!)
    (make-struct-type 's-exp-mapping #f 2 0 #f null (current-inspector) raise-pattern-error))
  (-define (make-s-exp-mapping depth valvar)
           (make-set!-transformer (-make-s-exp-mapping depth valvar)))
  (-define (s-exp-pattern-variable? v)
           (and (set!-transformer? v)
                (-s-exp-mapping? (set!-transformer-procedure v))))
  (-define (s-exp-mapping-depth v)
           (s-exp-mapping-ref (set!-transformer-procedure v) 0))
  (-define (s-exp-mapping-valvar v)
           (s-exp-mapping-ref (set!-transformer-procedure v) 1))

  (#%provide (protect make-match&env get-match-vars make-interp-match 
                      make-pexpand
                      make-syntax-mapping syntax-pattern-variable?
                      syntax-mapping-depth syntax-mapping-valvar
                      make-s-exp-mapping s-exp-pattern-variable?
                      s-exp-mapping-depth s-exp-mapping-valvar
                      stx-memq-pos no-ellipses?)))
