#lang racket/base

  (require mzlib/list
           "underscore-allowed.rkt")
  (require "term.rkt"
           "term-fn.rkt"
           setup/path-to-relative
           (for-template
            mzscheme
            "term.rkt"
            "matcher.rkt"))
  
  (provide rewrite-side-conditions/check-errs
           extract-names
           (rename-out [binds? id-binds?])
           raise-ellipsis-depth-error
           make-language-id
           language-id-nts)
  
  (provide (struct-out id/depth))
  
  (define (rewrite-side-conditions/check-errs all-nts what bind-names? orig-stx)
    (define (expected-exact name n stx)
      (raise-syntax-error what (format "~a expected to have ~a arguments" 
                                       name
                                       n)
                          orig-stx 
                          stx))
    (define (expected-arguments name stx)
      (raise-syntax-error what (format "~a expected to have arguments" name) orig-stx stx))
    (define (expect-identifier src stx)
      (unless (identifier? stx)
        (raise-syntax-error what "expected an identifier" src stx)))
    
    ; union-find w/o balancing or path compression (at least for now)
    (define (union e f sets)
      (hash-set sets (find f sets) (find e sets)))
    (define (find e sets)
      (let recur ([chd e] [par (hash-ref sets e #f)])
        (if (and par (not (eq? chd par))) (recur par (hash-ref sets par #f)) chd)))
    
    (define last-contexts (make-hasheq))
    (define last-stx (make-hasheq)) ;; used for syntax error reporting
    (define assignments #hasheq())
    (define (record-binder pat-stx under)
      (define pat-sym (syntax->datum pat-stx))
      (set! assignments
            (if (null? under)
                assignments
                (let ([last (hash-ref last-contexts pat-sym #f)])
                  (hash-set! last-stx pat-sym (cons pat-stx (hash-ref last-stx pat-sym '())))
                  (cond
                    [last
                     (unless (equal? (length last) (length under))
                       (define stxs (hash-ref last-stx pat-sym))
                       (raise-syntax-error what
                                           (format "found ~a under ~a ellips~as in one place and ~a ellips~as in another"
                                                   pat-sym
                                                   (length last)
                                                   (if (= 1 (length last)) "i" "e")
                                                   (length under)
                                                   (if (= 1 (length under)) "i" "e"))
                                           orig-stx
                                           (car stxs)
                                           (cdr stxs)))
                     (foldl (λ (cur last asgns) (union cur last asgns)) assignments under last)]
                    [else
                     (hash-set! last-contexts pat-sym under)
                     assignments])))))

    (define ellipsis-number 0)
    
    (define-values (term names)
      (let loop ([term orig-stx]
                 [under '()])
        (syntax-case term (side-condition variable-except variable-prefix hole name in-hole hide-hole cross unquote and)
          [(side-condition pre-pat (and))
           ;; rewriting metafunctions (and possibly other things) that have no where, etc clauses
           ;; end up with side-conditions that are empty 'and' expressions, so we just toss them here.
           (loop #'pre-pat under)]
          [(side-condition pre-pat exp)
           (let ()
             (define-values (pre-term pre-vars) (loop #'pre-pat under))
             (define names/ellipses (map build-dots pre-vars))
             (with-syntax ([pre-term pre-term]
                           [((name name/ellipses) ...)
                            (filter
                             values
                             (map (λ (id name/ellipses)
                                    (if (id/depth-mismatch? id)
                                        #f
                                        (list (id/depth-id id)
                                              name/ellipses)))
                                  pre-vars
                                  names/ellipses))]
                           [src-loc 
                            (let ([stx #'exp])
                              (define src (syntax-source stx))
                              (define line (syntax-line stx))
                              (define col (syntax-column stx))
                              (format "~a:~a" 
                                      (if (path? src)
                                          (path->relative-string/library src)
                                          "?")
                                      (if (and line col)
                                          (format "~a:~a" line col)
                                          (if line
                                              (format "~a:?" line)
                                              (syntax-position stx)))))])
               (values (syntax/loc term
                         (side-condition
                          pre-term
                          ,(lambda (bindings)
                             (term-let
                              ([name/ellipses (lookup-binding bindings 'name)] ...)
                              exp))
                          ; For use in error messages.
                          src-loc))
                       pre-vars)))]
          [(side-condition a ...) (expected-exact 'side-condition 2 term)]
          [side-condition (expected-arguments 'side-condition term)]
          [(variable-except a ...)
           (begin
             (for ([a (in-list (syntax->list #'(a ...)))])
               (expect-identifier term a))
             (values term '()))]
          [variable-except (expected-arguments 'variable-except term)]
          [(variable-prefix a)
           (begin
             (expect-identifier term #'a)
             (values term '()))]
          [(variable-prefix a ...) (expected-exact 'variable-prefix 1 term)]
          [variable-prefix (expected-arguments 'variable-prefix term)]
          [hole (values term '())]
          [(name x y)
           (let ()
             (define-values (sub-term sub-vars) (loop #'y under))
             (record-binder #'x under)
             (values #`(name x #,sub-term)
                     (cons (make-id/depth #'x (length under) #f)
                           sub-vars)))]
          [(name x ...) (expected-exact 'name 2 term)]
          [name (expected-arguments 'name term)]
          [(in-hole a b)
           (let ()
             (define-values (a-term a-vars) (loop #'a under))
             (define-values (b-term b-vars) (loop #'b under))
             (values #`(in-hole #,a-term #,b-term)
                     (append a-vars b-vars)))]
          [(in-hole a ...) (expected-exact 'in-hole 2 term)]
          [in-hole (expected-arguments 'in-hole term)]
          [(hide-hole a) 
           (let ()
             (define-values (sub-term vars) (loop #'a under))
             (values #`(hide-hole #,sub-term) vars))]
          [(hide-hole a ...) (expected-exact 'hide-hole 1 term)]
          [hide-hole (expected-arguments 'hide-hole term)]
          [(cross a)
           (let ()
             (expect-identifier term #'a)
             (define a-str (symbol->string (syntax-e #'a)))
             (values #`(cross #,(string->symbol (format "~a-~a" a-str a-str)))
                     '()))]
          [(cross a ...) (expected-exact 'cross 1 term)]
          [cross (expected-arguments 'cross term)]
          [(unquote . _)
           (raise-syntax-error what "unquote disallowed in patterns" orig-stx term)]
          [_
           (identifier? term)
           (let ()
             (define m (regexp-match #rx"^([^_]*)_(.*)$" (symbol->string (syntax-e term))))
             (cond
               [m
                (define prefix (list-ref m 1))
                (define suffix (list-ref m 2))
                (define suffix-sym (string->symbol suffix))
                (define prefix-sym (string->symbol prefix))
                (define prefix-stx (datum->syntax term prefix-sym))
                (define mismatch? (regexp-match? #rx"^!_" suffix))
                (cond
                  [(eq? prefix-sym '...)
                   (raise-syntax-error 
                    what
                    "found an ellipsis outside of a sequence"
                    orig-stx
                    term)]
                  [(memq prefix-sym all-nts)
                   (record-binder term under)
                   (values (if mismatch?
                               `(mismatch-name ,term (nt ,prefix-stx))
                               `(name ,term (nt ,prefix-stx)))
                           (list (make-id/depth term (length under) mismatch?)))]
                  [(memq prefix-sym underscore-allowed)
                   (record-binder term under)
                   (values (if mismatch?
                               `(mismatch-name ,term ,prefix-stx)
                               `(name ,term ,prefix-stx))
                           (list (make-id/depth term (length under) mismatch?)))]
                  [else
                   (raise-syntax-error
                    what
                    (format "before underscore must be either a non-terminal or a built-in pattern, found ~a in ~s"
                            suffix-sym (syntax-e term))
                    orig-stx 
                    term)])]
               [(eq? (syntax-e term) '...)
                (raise-syntax-error 
                 what
                 "found an ellipsis outside of a sequence"
                 orig-stx
                 term)]
               [(memq (syntax-e term) all-nts)
                (cond
                  [bind-names?
                   (record-binder term under)
                   (values `(name ,term (nt ,term)) (list (make-id/depth term (length under) #f)))]
                  [else
                   (values `(nt ,term) '())])]
               [(memq (syntax-e term) underscore-allowed)
                (cond
                  [bind-names?
                   (record-binder #'term under)
                   (values `(name ,term ,term) (list (make-id/depth term (length under) #f)))]
                  [else
                   (values term '())])]
               [else
                (values term '())]))]
          [(terms ...)
           (let ()
             (define terms-lst (syntax->list #'(terms ...)))
             (define (is-ellipsis? term)
               (and (identifier? term)
                    (regexp-match? #rx"^[.][.][.]" (symbol->string (syntax-e term)))))
             (when (and (pair? terms-lst) (is-ellipsis? (car terms-lst)))
               (raise-syntax-error what
                                   "ellipsis should not appear in the first position of a sequence"
                                   orig-stx 
                                   term))
             (define-values (updated-terms vars)
               (let t-loop ([terms terms-lst])
                 (cond
                   [(null? terms) (values '() '())]
                   [(null? (cdr terms))
                    (define-values (term vars) (loop (car terms) under))
                    (values (list term) vars)]
                   [(is-ellipsis? (cadr terms))
                    (when (and (pair? (cddr terms))
                               (is-ellipsis? (caddr terms)))
                      (raise-syntax-error what
                                          "two ellipses should not appear in a row"
                                          orig-stx
                                          (cadr terms)
                                          (list (caddr terms))))
                    (define ellipsis-sym (syntax-e (cadr terms)))
                    (define ellipsis-pre-str (symbol->string ellipsis-sym))
                    (define mismatch? (regexp-match? #rx"^[.][.][.]_!_" ellipsis-pre-str))
                    (define ellipsis-str (cond
                                           [mismatch?
                                            (set! ellipsis-number (+ ellipsis-number 1))
                                            (format "..._r~a" ellipsis-number)]
                                           [(regexp-match? #rx"^[.][.][.]_r" ellipsis-pre-str)
                                            (string-append (substring ellipsis-str 0 4)
                                                           "r"
                                                           (substring ellipsis-str 
                                                                      4
                                                                      (string-length ellipsis-str)))]
                                           [(regexp-match? #rx"^[.][.][.]_" ellipsis-pre-str) 
                                            ellipsis-pre-str]
                                           [else 
                                            (set! ellipsis-number (+ ellipsis-number 1))
                                            (format "..._r~a" ellipsis-number)]))
                    (define ellipsis+name (datum->syntax
                                           (cadr terms)
                                           (string->symbol ellipsis-str)
                                           (cadr terms)))
                    (record-binder ellipsis+name under)
                    (define-values (fst-term fst-vars) 
                      (loop (car terms) (cons (syntax-e ellipsis+name) under)))
                    (define-values (rst-terms rst-vars) (t-loop (cddr terms)))
                    (values (cons `(repeat ,fst-term 
                                           ,ellipsis+name
                                           ,(if mismatch? (cadr terms) #f))
                                  rst-terms)
                            (append fst-vars rst-vars))]
                   [else
                    (define-values (fst-term fst-vars) (loop (car terms) under))
                    (define-values (rst-terms rst-vars) (t-loop (cdr terms)))
                    (values (cons fst-term rst-terms)
                            (append fst-vars rst-vars))])))
             (values `(list ,@updated-terms) vars))]
          [else
           (when (pair? (syntax-e term))
             (let loop ([term term])
               (cond
                 [(syntax? term) (loop (syntax-e term))]
                 [(pair? term) (loop (cdr term))]
                 [(null? term) (void)]
                 [#t
                  (raise-syntax-error what "dotted pairs not supported in patterns" orig-stx term)])))
           (values term '())])))
    
    (define closed-table
      (make-immutable-hasheq (hash-map assignments (λ (cls _) (cons cls (find cls assignments))))))
    
    (define repeat-id-counts (make-hash))

    (define ellipsis-normalized 
      (let loop ([pat term])
        (syntax-case pat (repeat)
          [(repeat sub-pat name mismatch-name)
           (let ()
             (define mapped-name (hash-ref closed-table (syntax-e #'name) #f))
             (define new-name (if mapped-name
                                  mapped-name
                                  (syntax-e #'name)))
             (hash-set! repeat-id-counts new-name (+ 1 (hash-ref repeat-id-counts new-name 0)))
             (let ([id (syntax-e #'mismatch-name)])
               (when id
                 (hash-set! repeat-id-counts id (+ 1 (hash-ref repeat-id-counts id 0)))))
             #`(repeat #,(loop #'sub-pat) #,new-name mismatch-name))]
          [(a ...)
           (let ()
             (define new (map loop (syntax->list #'(a ...))))
             (if (syntax? pat)
                 (datum->syntax pat new pat)
                 new))]
          [_ pat])))

    ;(printf "term ~s\n" (syntax->datum (datum->syntax #'here term)))
    ;(printf "norm ~s\n" (syntax->datum (datum->syntax #'here ellipsis-normalized)))
    ;(printf "repeat-id-counts ~s\n" repeat-id-counts)
    
    (define ellipsis-normalized/simplified 
      (let loop ([pat ellipsis-normalized])
        (syntax-case pat (repeat)
          [(repeat sub-pat name mismatch-name)
           (let ()
             #`(repeat #,(loop #'sub-pat) 
                       #,(if (= 1 (hash-ref repeat-id-counts (syntax-e #'name)))
                             #f
                             #'name)
                       #,(if (and (syntax-e #'mismatch-name)
                                  (= 1 (hash-ref repeat-id-counts (syntax-e #'mismatch-name))))
                             #f
                             #'mismatch-name)))]
          [(a ...)
           (let ()
             (define new (map loop (syntax->list #'(a ...))))
             (if (syntax? pat)
                 (datum->syntax pat new pat)
                 new))]
          [_ pat])))
    
    (filter-duplicates what orig-stx names)
    (let ([without-mismatch-names (filter (λ (x) (not (id/depth-mismatch? x))) names)])
      (with-syntax ([(name/ellipses ...) (map build-dots without-mismatch-names)]
                    [(name ...) (map id/depth-id without-mismatch-names)]
                    [term ellipsis-normalized/simplified])
        #'(term (name ...) (name/ellipses ...)))))
  
  (define-struct id/depth (id depth mismatch?))
  
  ;; extract-names : syntax syntax -> (values (listof syntax) (listof syntax[x | (x ...) | ((x ...) ...) | ...]))
  ;; this function is obsolete and uses of it are suspect. Things should be using 
  ;; rewrite-side-conditions/check-errs instead
  (define (extract-names all-nts what bind-names? orig-stx [mode 'rhs-only])
    (let* ([dups
            (let loop ([stx orig-stx]
                       [names null]
                       [depth 0])
              (syntax-case stx (name in-hole side-condition cross nt)
                [(name sym pat)
                 (identifier? (syntax sym))
                 (loop (syntax pat) 
                       (cons (make-id/depth (syntax sym) depth #f) names)
                       depth)]
                [(in-hole pat1 pat2)
                 (loop (syntax pat1)
                       (loop (syntax pat2) names depth)
                       depth)]
                [(side-condition pat . rest)
                 (loop (syntax pat) names depth)]
                [(cross _) names]
                [(pat ...)
                 (let i-loop ([pats (syntax->list (syntax (pat ...)))]
                              [names names])
                   (cond
                     [(null? pats) names]
                     [else 
                      (if (or (null? (cdr pats))
                              (not (identifier? (cadr pats)))
                              (not (or (free-identifier=? (quote-syntax ...)
                                                          (cadr pats))
                                       (let ([inside (syntax-e (cadr pats))])
                                         (regexp-match #rx"^\\.\\.\\._" (symbol->string inside))))))
                          (i-loop (cdr pats)
                                  (loop (car pats) names depth))
                          (i-loop (cdr pats)
                                  (loop (car pats) names (+ depth 1))))]))]
                [x
                 (and (identifier? (syntax x))
                      ((case mode
                         [(rhs-only) binds-in-right-hand-side?]
                         [(binds-anywhere) binds?])
                       all-nts bind-names? (syntax x)))
                 (cons (make-id/depth (syntax x) depth #f) names)]
                [else names]))]
           [no-dups (filter-duplicates what orig-stx dups)])
      (values (map id/depth-id no-dups)
              (map build-dots no-dups))))
  
  ;; build-dots : id/depth -> syntax[x | (x ...) | ((x ...) ...) | ...]
  (define (build-dots id/depth)
    (let loop ([depth (id/depth-depth id/depth)])
      (cond
        [(zero? depth) (id/depth-id id/depth)]
        [else (with-syntax ([rest (loop (- depth 1))]
                            [dots (quote-syntax ...)])
                (syntax (rest dots)))])))
  
  (define (binds? nts bind-names? x)
    (or (and bind-names? (memq (syntax-e x) nts))
        (and bind-names? (memq (syntax-e x) underscore-allowed))
        (regexp-match #rx"_" (symbol->string (syntax-e x)))))
  
  (define (binds-in-right-hand-side? nts bind-names? x)
    (and (binds? nts bind-names? x)
         (let ([str (symbol->string (syntax-e x))])
           (and (not (regexp-match #rx"^\\.\\.\\._" str))
                (not (regexp-match #rx"_!_" str))))))
  
  (define (raise-ellipsis-depth-error what one-binder one-depth another-binder another-depth [orig-stx #f])
    (raise-syntax-error
     what
     (format "found the same binder, ~s, at different depths, ~a and ~a"
             (syntax->datum one-binder)
             one-depth
             another-depth)
     orig-stx
     another-binder
     (list one-binder)))
  
  (define (filter-duplicates what orig-stx dups)
    (let loop ([dups dups])
      (cond
        [(null? dups) null]
        [else 
         (cons
          (car dups)
          (filter (lambda (x) 
                    (let ([same-id? (free-identifier=? (id/depth-id x)
                                                       (id/depth-id (car dups)))])
                      (when same-id?
                        (unless (equal? (id/depth-depth x)
                                        (id/depth-depth (car dups)))
                          (raise-ellipsis-depth-error
                           what 
                           (id/depth-id x) (id/depth-depth x)
                           (id/depth-id (car dups)) (id/depth-depth (car dups))
                           orig-stx)))
                      (not same-id?)))
                  (loop (cdr dups))))])))

