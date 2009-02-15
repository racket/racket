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
                (cond
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

;; these are the suffixes that are checked for the reverse 
;; module-path mapping.
(: possible-suffixes (Listof String))
(define possible-suffixes '(".ss" ".scm" ""))

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

