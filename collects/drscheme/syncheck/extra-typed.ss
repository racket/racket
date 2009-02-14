#lang typed-scheme

(require (except-in scheme/list remove-duplicates)
         "id-sets.ss")

(define-type-alias (MaybeList a) (Rec x (U a '() (Pair a x))))

(provide (all-defined-out))

;; remove-duplicates : (listof syntax[original]) -> (listof syntax[original])
;; removes duplicates, based on the source locations of the identifiers
;; assumes the list is ordered by source location
(: remove-duplicates ((Listof Syntax) -> (Listof Syntax)))         
(define (remove-duplicates ids)
  (cond
    [(null? ids) null]
    [else (let: loop : (Listof Syntax)
                ([fst : Syntax (car ids)]
                 [rst : (Listof Syntax) (cdr ids)])
            (error 'foo) #;(cond
              [(null? rst) (list fst)]
              [else (if (and (eq? (syntax-source fst)
                                  (syntax-source (car rst)))                             
                             ;; CHANGE - used eqv? instead of =, since these might be #f
                             (eqv? (syntax-position fst)
                                   (syntax-position (car rst))))
                        (loop fst (cdr rst))
                        (cons fst (loop (car rst) (cdr rst))))]))]))


;; name-duplication? : (listof syntax) (listof id-set) symbol -> boolean
;; returns #t if the name chosen would be the same as another name in this scope.
(: name-duplication? ((Listof Identifier) (Listof Id-Set) String -> Any))
(define (name-duplication? to-be-renamed id-sets new-str)
  (let ([new-ids (map (λ: ([id : Identifier]) (datum->syntax id (string->symbol new-str)))
                      to-be-renamed)])
    (ormap (λ: ([id-set : Id-Set])
             (ormap (λ: ([new-id : Identifier]) (get-ids id-set new-id)) 
                    new-ids))
           id-sets)))


;; annotate-raw-keyword : syntax id-map -> void
;; annotates keywords when they were never expanded. eg.
;; if someone just types `(λ (x) x)' it has no 'origin
;; field, but there still are keywords.
(: annotate-raw-keyword (Syntax Id-Set -> Any))
(define (annotate-raw-keyword stx id-map)
  (let ([lst (syntax-e stx)])
    (when (pair? lst)
      (let ([f-stx (car lst)])
        (when (and (syntax-original? f-stx)
                   (identifier? f-stx))
          (add-id id-map f-stx))))))


;; add-binders : syntax id-set -> void
;; transforms an argument list into a bunch of symbols/symbols
;; and puts them into the id-set
;; effect: colors the identifiers
(: add-binders (Syntax Id-Set -> Void))
(define (add-binders stx id-set)
  (let: loop : Void ([stx : (MaybeList Syntax) stx])
    (let ([e (if (syntax? stx) (syntax-e stx) stx)])
      (cond
        [(cons? e)
         (let ([fst (car e)]
               [rst (cdr e)])
           (if (identifier? fst) ;; CHANGE - was (syntax? fst)
               (begin
                 (when (syntax-original? fst)
                   (add-id id-set fst))
                 (loop rst))
               (loop rst)))]
        [(null? e) (void)]
        [(identifier? stx) ;; CHANGE -- used to be else
         (when (syntax-original? stx)
           (add-id id-set stx))]))))

(define-type-alias TailHT (HashTable Syntax (Listof Syntax)))

;; annotate-tail-position/last : (listof syntax) -> void
(: annotate-tail-position/last (Syntax (Listof Syntax) TailHT -> Void))
(define (annotate-tail-position/last orig-stx stxs tail-ht)
  (unless (null? stxs)
    (annotate-tail-position orig-stx (car (#{last-pair @ Syntax} stxs)) tail-ht)))

;; annotate-tail-position : syntax -> void
;; colors the parens (if any) around the argument
;; to indicate this is a tail call.
(: annotate-tail-position (Syntax Syntax TailHT -> Void))
(define (annotate-tail-position orig-stx tail-stx tail-ht)
  (hash-set!
   tail-ht 
   orig-stx 
   (cons
    tail-stx
    (hash-ref 
     tail-ht
     orig-stx
     (λ () null)))))

;; add-disappeared-uses : syntax id-set -> void
(: add-disappeared-uses (Syntax Id-Set -> Void))
(define (add-disappeared-uses stx id-set)
  (let ([prop (syntax-property stx 'disappeared-use)])
    (when prop
      (let loop ([prop prop])
        (cond
          [(pair? prop)
           (loop (car prop))
           (loop (cdr prop))]
          [(identifier? prop)
           (add-id id-set prop)])))))

;; add-require-spec : hash-table[sexp[require-spec] -o> (listof syntax)]
;;                 -> require-spec
;;                    syntax
;;                 -> void
(: add-require-spec ((HashTable Any (Listof Syntax)) -> (Syntax Syntax -> Void)))
(define (add-require-spec require-ht)
  (λ (raw-spec syntax)
    (when (syntax-original? syntax)
      (let ([key (syntax->datum raw-spec)])
        (hash-set! require-ht
                   key
                   (cons syntax
                         (hash-ref require-ht
                                   key
                                   (λ () '()))))))))

;; possible-suffixes : (listof string)
;; these are the suffixes that are checked for the reverse 
;; module-path mapping.
(: possible-suffixes (Listof String))
(define possible-suffixes '(".ss" ".scm" ""))

;; add-origins : sexp id-set -> void
(: add-origins (Syntax Id-Set -> Void))
(define (add-origins sexp id-set)
  (let ([origin (syntax-property sexp 'origin)])
    (when (syntax? origin) ;; CHANGE - was (when origin ...)
      (let loop ([ct origin])
        (cond
          [(pair? ct) 
           (loop (car ct))
           (loop (cdr ct))]
          [(identifier? ct) ;; CHANGE - was (syntax? ct)
           (when (syntax-original? ct)
             (add-id id-set ct))]
          [else (void)])))))


;; add-disappeared-bindings : syntax id-set -> void
(: add-disappeared-bindings (Syntax Id-Set Id-Set -> Void))
(define (add-disappeared-bindings stx binders disappaeared-uses)
  (let ([prop (syntax-property stx 'disappeared-binding)])
    (when prop
      (let loop ([prop prop])
        (cond
          [(pair? prop)
           (loop (car prop))
           (loop (cdr prop))]
          [(identifier? prop)
           (add-origins prop disappaeared-uses)
           (add-id binders prop)])))))

;; module-name-sym->filename : symbol -> (union #f string)
(: module-name-sym->filename (Symbol -> (Option Path)))
(define (module-name-sym->filename sym)
  (let ([str (symbol->string sym)])
    (and ((string-length str) . > . 1)
         (char=? (string-ref str 0) #\,)
         (let ([fn (substring str 1 (string-length str))])
           (ormap (λ: ([x : String])
                    (let ([test (string->path (string-append fn x))])
                      (and (file-exists? test)
                           test)))
                  possible-suffixes)))))

(: symbolic-compare? (Syntax Syntax -> Boolean))
(define (symbolic-compare? x y) (eq? (syntax-e x) (syntax-e y)))

;; type req/tag = (make-req/tag syntax sexp boolean)
(define-typed-struct req/tag ([req-stx : Syntax] [req-sexp : Any] [used? : Boolean]))

;; add-var : hash-table -> syntax -> void
;; adds the variable to the hash table.
(: add-var ((HashTable Any (Listof Any)) -> (Syntax -> Void)))
(define (add-var ht)
  (λ (var)
    (let* ([key (syntax-e var)]
           [prev (hash-ref ht #{key :: Any} (λ () #{null :: (Listof Any)}))])
      (hash-set! ht #{key :: Any} #{(cons var prev) :: (Listof Any)}))))

#|
;; annotate-basic : syntax 
;;                  namespace
;;                  string[directory]
;;                  syntax[id]
;;                  id-set (six of them)
;;                  hash-table[require-spec -> syntax] (three of them)
;;               -> void
(: annotate-basic (Syntax 
                   Any String Syntax
                   Id-Set Id-Set Id-Set Id-Set Id-Set Id-Set 
                   Any
                   (HashTable Syntax Syntax) (HashTable Syntax Syntax) (HashTable Syntax Syntax) (HashTable Syntax Syntax)
                   -> Void))
(define (annotate-basic sexp user-namespace user-directory jump-to-id
                        low-binders high-binders 
                        low-varrefs high-varrefs 
                        low-tops high-tops
                        templrefs
                        requires require-for-syntaxes require-for-templates require-for-labels)
  (let ([tail-ht (make-hash-table)]
        [maybe-jump
         (λ: ([vars : Syntax])
           (when jump-to-id
             (for-each (λ: ([id : Identifier])
                         (let ([binding (identifier-binding id)])
                           (when (pair? binding)
                             (let ([nominal-source-id (list-ref binding 3)])
                               (when (eq? nominal-source-id jump-to-id)
                                 (jump-to id))))))
                       (syntax->list vars))))])
    
    (let level-loop ([sexp sexp]
                     [high-level? #f])
      (let* ([loop (λ (sexp) (level-loop sexp high-level?))]
             [varrefs (if high-level? high-varrefs low-varrefs)]
             [binders (if high-level? high-binders low-binders)]
             [tops (if high-level? high-tops low-tops)]
             [collect-general-info
              (λ (stx)
                (add-origins stx varrefs)
                (add-disappeared-bindings stx binders varrefs)
                (add-disappeared-uses stx varrefs))])
        (collect-general-info sexp)
        (syntax-case* sexp (#%plain-lambda case-lambda if begin begin0 let-values letrec-values set!
                                           quote quote-syntax with-continuation-mark 
                                           #%plain-app #%top #%plain-module-begin
                                           define-values define-syntaxes define-values-for-syntax module
                                           #%require #%provide #%expression)
          (if high-level? free-transformer-identifier=? free-identifier=?)
          [(#%plain-lambda args bodies ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
             (add-binders (syntax args) binders)
             (for-each loop (syntax->list (syntax (bodies ...)))))]
          [(case-lambda [argss bodiess ...]...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (for-each (λ (bodies/stx) (annotate-tail-position/last sexp 
                                                                    (syntax->list bodies/stx)
                                                                    tail-ht))
                       (syntax->list (syntax ((bodiess ...) ...))))
             (for-each
              (λ (args bodies)
                (add-binders args binders)
                (for-each loop (syntax->list bodies)))
              (syntax->list (syntax (argss ...)))
              (syntax->list (syntax ((bodiess ...) ...)))))]
          [(if test then else)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (annotate-tail-position sexp (syntax then) tail-ht)
             (annotate-tail-position sexp (syntax else) tail-ht)
             (loop (syntax test))
             (loop (syntax else))
             (loop (syntax then)))]
          [(begin bodies ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
             (for-each loop (syntax->list (syntax (bodies ...)))))]
          
          ;; treat a single body expression specially, since this has
          ;; different tail behavior.
          [(begin0 body)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (annotate-tail-position sexp (syntax body) tail-ht)
             (loop (syntax body)))]
          
          [(begin0 bodies ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (for-each loop (syntax->list (syntax (bodies ...)))))]
          
          [(let-values (bindings ...) bs ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (for-each collect-general-info (syntax->list (syntax (bindings ...))))
             (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
             (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
               (for-each (λ (x) (add-binders x binders))
                         (syntax->list (syntax ((xss ...) ...))))
               (for-each loop (syntax->list (syntax (es ...))))
               (for-each loop (syntax->list (syntax (bs ...))))))]
          [(letrec-values (bindings ...) bs ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (for-each collect-general-info (syntax->list (syntax (bindings ...))))
             (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
             (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
               (for-each (λ (x) (add-binders x binders))
                         (syntax->list (syntax ((xss ...) ...))))
               (for-each loop (syntax->list (syntax (es ...))))
               (for-each loop (syntax->list (syntax (bs ...))))))]
          [(set! var e)
           (begin
             (annotate-raw-keyword sexp varrefs)
             
             ;; tops are used here because a binding free use of a set!'d variable
             ;; is treated just the same as (#%top . x).
             (when (syntax-original? (syntax var))
               (if (identifier-binding (syntax var))
                   (add-id varrefs (syntax var))
                   (add-id tops (syntax var))))
             
             (loop (syntax e)))]
          [(quote datum)
           ;(color-internal-structure (syntax datum) constant-style-name)
           (annotate-raw-keyword sexp varrefs)]
          [(quote-syntax datum)
           ;(color-internal-structure (syntax datum) constant-style-name)
           (annotate-raw-keyword sexp varrefs)
           (let loop ([stx #'datum])
             (cond [(identifier? stx)
                    (when (syntax-original? stx)
                      (add-id templrefs stx))]
                   [(syntax? stx)
                    (loop (syntax-e stx))]
                   [(pair? stx)
                    (loop (car stx))
                    (loop (cdr stx))]
                   [(vector? stx)
                    (for-each loop (vector->list stx))]
                   [(box? stx)
                    (loop (unbox stx))]
                   [else (void)]))]
          [(with-continuation-mark a b c)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (annotate-tail-position sexp (syntax c) tail-ht)
             (loop (syntax a))
             (loop (syntax b))
             (loop (syntax c)))]
          [(#%plain-app pieces ...)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (for-each loop (syntax->list (syntax (pieces ...)))))]
          [(#%top . var)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (when (syntax-original? (syntax var))
               (add-id tops (syntax var))))]
          [(define-values vars b)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (add-binders (syntax vars) binders)
             (maybe-jump (syntax vars))
             (loop (syntax b)))]
          [(define-syntaxes names exp)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (add-binders (syntax names) binders)
             (maybe-jump (syntax names))
             (level-loop (syntax exp) #t))]
          [(define-values-for-syntax names exp)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (add-binders (syntax names) high-binders)
             (maybe-jump (syntax names))
             (level-loop (syntax exp) #t))]
          [(module m-name lang (#%plain-module-begin bodies ...))
           (begin
             (annotate-raw-keyword sexp varrefs)
             ((annotate-require-open user-namespace user-directory) (syntax lang))
             
             ;; temporarily removed until Matthew fixes whatever.
             #;
             (hash-table-put! requires 
                              (syntax->datum (syntax lang))
                              (cons (syntax lang)
                                    (hash-table-get requires 
                                                    (syntax->datum (syntax lang))
                                                    (λ () '()))))
             (for-each loop (syntax->list (syntax (bodies ...)))))]
          
          ; top level or module top level only:
          [(#%require require-specs ...)
           (let ([at-phase
                  (lambda (stx requires)
                    (syntax-case stx ()
                      [(_ require-specs ...)
                       (let ([new-specs (map trim-require-prefix
                                             (syntax->list (syntax (require-specs ...))))])
                         (annotate-raw-keyword sexp varrefs)
                         (for-each (annotate-require-open user-namespace user-directory) new-specs)
                         (for-each (add-require-spec requires)
                                   new-specs
                                   (syntax->list (syntax (require-specs ...)))))]))])
             (for-each (lambda (spec)
                         (syntax-case* spec (for-syntax for-template for-label) (lambda (a b)
                                                                                  (eq? (syntax-e a) (syntax-e b)))
                           [(for-syntax specs ...)
                            (at-phase spec require-for-syntaxes)]
                           [(for-template specs ...)
                            (at-phase spec require-for-templates)]
                           [(for-label specs ...)
                            (at-phase spec require-for-labels)]
                           [else
                            (at-phase (list #f spec) requires)]))
                       (syntax->list #'(require-specs ...))))]
          
          ; module top level only:
          [(#%provide provide-specs ...)
           (let ([provided-varss (map extract-provided-vars
                                      (syntax->list (syntax (provide-specs ...))))])
             (annotate-raw-keyword sexp varrefs)
             (for-each (λ (provided-vars)
                         (for-each
                          (λ (provided-var)
                            (when (syntax-original? provided-var)
                              (add-id varrefs provided-var)))
                          provided-vars))
                       provided-varss))]
          
          [(#%expression arg)
           (begin
             (annotate-raw-keyword sexp varrefs)
             (loop #'arg))]
          [id
           (identifier? (syntax id))
           (when (syntax-original? sexp)
             (add-id varrefs sexp))]
          [_
           (begin
             #;
             (printf "unknown stx: ~e datum: ~e source: ~e\n"
                     sexp
                     (and (syntax? sexp)
                          (syntax->datum sexp))
                     (and (syntax? sexp)
                          (syntax-source sexp)))
             (void))])))
    (add-tail-ht-links tail-ht)))
|#
