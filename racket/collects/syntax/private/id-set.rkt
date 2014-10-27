#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/stx racket/syntax)
         racket/set racket/dict racket/sequence racket/stream racket/syntax
         syntax/id-table)
  
;; TODO
;; - generalize and impl bound-id sets
;; - add contracts
;; - support generic set interface
;; - support generic sequence interface(?)
;; - better error msgs?

;; fns with standard free-id-set- or bound-id-set- prefix
(define-for-syntax ID-SET-PREFIXED-FNS
  #'(empty? count member? first rest >stream >list copy copy-clear map for-each))
;; fns with both mutable and immutable versions
(define-for-syntax ID-SET-IMMUT/MUT-FNS
  #'(add remove clear union intersect subtract symmetric-difference))
;; other functions that have a different prefix (eg free-id-)
(define-for-syntax ID-SET-ALT-PREFIXED-FNS #'(set? set=? subset? proper-subset?))

;; fns with standard free-id-table- or bound-id-table- prefix
(define-for-syntax ID-TABLE-FNS
  #'(ref set set! remove remove! map for-each count first next iterate-key iterate-first))
        
(define-for-syntax ID-SET-FNS #'(empty?))

(define-for-syntax (mk-pred id) (format-id id "~a?" id))

(define-syntax (define-and-provide-id-set-fns stx)
  (syntax-parse stx
    [(_ #:type type)
     #:with id-set (format-id #'type "~a-id-set" #'type)
     #:with mutable-id-set (format-id #'id-set "mutable-~a" #'id-set)
     #:with mk-mutable-id-set (format-id #'mutable-id-set "make-~a" #'mutable-id-set)
     #:with immutable-id-set (format-id #'id-set "immutable-~a" #'id-set)
     #:with mk-immutable-id-set (format-id #'immutable-id-set "make-~a" #'immutable-id-set)
     #:with id-set? (mk-pred #'id-set)
     #:with mutable-id-set? (mk-pred #'mutable-id-set)
     #:with immutable-id-set? (mk-pred #'immutable-id-set)
;     #:with in-id-set-fn-name (format-id #'type "in-id-set") 
;     #:with in-id-set-fn (format-id #'type "in-~a-id-set" #'type) ; in-set
     #:with id-set-table (format-id #'type "~a-id-set-table" #'type)
     #:with id-set-empty? (format-id #'type "~a-id-set-empty?" #'type)
     #:with id-set-count (format-id #'type "~a-id-set-count" #'type)
     #:with id-set-member? (format-id #'type "~a-id-set-member?" #'type)
     #:with id-set=? (format-id #'type "~a-id-set=?" #'type)
     #:with id-set-add (format-id #'type "~a-id-set-add" #'type)
     #:with id-set-add! (format-id #'type "~a-id-set-add!" #'type)
     #:with id-set-remove (format-id #'type "~a-id-set-remove" #'type)
     #:with id-set-remove! (format-id #'type "~a-id-set-remove!" #'type)
     #:with id-set-first (format-id #'type "~a-id-set-first" #'type)
     #:with id-set-rest (format-id #'type "~a-id-set-rest" #'type)
     #:with in-id-set (format-id #'type "in-~a-id-set" #'type)
     #:with id-set->stream (format-id #'type "~a-id-set->stream" #'type)
     #:with id-set->list (format-id #'type "~a-id-set->list" #'type)
     #:with id-set-copy (format-id #'type "~a-id-set-copy" #'type)
     #:with id-set-copy-clear (format-id #'type "~a-id-set-copy-clear" #'type)
     #:with id-set-clear (format-id #'type "~a-id-set-clear" #'type)
     #:with id-set-clear! (format-id #'type "~a-id-set-clear!" #'type)
     #:with id-set-union (format-id #'type "~a-id-set-union" #'type)
     #:with id-set-union! (format-id #'type "~a-id-set-union!" #'type)
     #:with id-set-intersect (format-id #'type "~a-id-set-intersect" #'type)
     #:with id-set-intersect! (format-id #'type "~a-id-set-intersect!" #'type)
     #:with id-set-subtract (format-id #'type "~a-id-set-subtract" #'type)
     #:with id-set-subtract! (format-id #'type "~a-id-set-subtract!" #'type)
     #:with id-set-symmetric-difference (format-id #'type "~a-id-set-symmetric-difference" #'type)
     #:with id-set-symmetric-difference! (format-id #'type "~a-id-set-symmetric-difference!" #'type)
     #:with id-subset? (format-id #'type "~a-id-subset?" #'type)
     #:with id-proper-subset? (format-id #'type "~a-id-proper-subset?" #'type)
     #:with id-set-map (format-id #'type "~a-id-set-map" #'type)
     #:with id-set-for-each (format-id #'type "~a-id-set-for-each" #'type)
;     #:with (id-set-fn-name ...)
;            (append 
;             (stx-map (λ (f) (format-id #'type "id-set-~a" f)) ID-SET-FNS)
;             #;(stx-map (λ (f) (format-id #'type "id-set-~a" f)) ID-SET-PREFIXED-FNS)
;             #;(stx-map (λ (f) (format-id #'type "id-set-~a" f)) ID-SET-IMMUT/MUT-FNS)
;             #;(stx-map (λ (f) (format-id #'type "id-set-~a!" f)) ID-SET-IMMUT/MUT-FNS)
;             #;(stx-map (λ (f) (format-id #'type "id-~a" f)) ID-SET-ALT-PREFIXED-FNS))
;     #:with (id-set-fn ...) 
;            (stx-map 
;             (λ (f) (format-id #'type "~a-~a" #'type f)) 
;             #'(id-set-fn-name ...))
     #:with make-mutable-id-table (format-id #'type "make-~a-id-table" #'type)
     #:with make-immutable-id-table (format-id #'type "make-immutable-~a-id-table" #'type)
     #:with id-table-ref (format-id #'type "~a-id-table-ref" #'type)
     #:with id-table-set (format-id #'type "~a-id-table-set" #'type)
     #:with id-table-set! (format-id #'type "~a-id-table-set!" #'type)
     #:with id-table-remove (format-id #'type "~a-id-table-remove" #'type)
     #:with id-table-remove! (format-id #'type "~a-id-table-remove!" #'type)
     #:with id-table-map (format-id #'type "~a-id-table-map" #'type)
     #:with id-table-for-each (format-id #'type "~a-id-table-for-each" #'type)
     #:with id-table-count (format-id #'type "~a-id-table-count" #'type)
     #:with id-table-first (format-id #'type "~a-id-table-first" #'type)
     #:with id-table-next (format-id #'type "~a-id-table-next" #'type)
     #:with id-table-iterate-key  (format-id #'type "~a-id-table-iterate-key" #'type)
     #:with id-table-iterate-first (format-id #'type "~a-id-table-iterate-first" #'type)
;     #:with (id-table-fn-name ...)
;            (append 
;              (stx-map (λ (f) (format-id #'type "id-table-~a" f)) ID-TABLE-FNS))
;     #:with (id-table-fn ...) 
;            (stx-map 
;             (λ (f) (format-id f "~a-~a" #'type f)) 
;             #'(id-table-fn-name ...))
;     #:with (in-id-set-fn-name id-set-fn-name ...) (in-id-set-fn id-set-fn ...)
;     #:when (stx-map (λ (f) (printf "~a\n" f)) #'(id-table-fn-name ...))
;     #:when (stx-map (λ (f) (printf "~a\n" f)) #'(id-table-fn ...))
     #'(begin
;         (define/with-syntax (id-set-fn-name ...) #'(id-set-fn ...))
;        (define-syntaxes (id-set-fn-name ...)
;          (values (make-rename-transformer (syntax id-set-fn)) ...))
         (provide 
          (rename-out [mk-mutable-id-set mutable-id-set]
                      [mk-immutable-id-set immutable-id-set])
;          in-id-set-fn id-set-fn ...)
          id-set?
          mutable-id-set?
          immutable-id-set?
          id-set-empty?
          id-set-count
          id-set-member?
          id-set=?
          id-set-add
          id-set-add!
          id-set-remove
          id-set-remove!
          id-set-first
          id-set-rest
          in-id-set
          id-set->stream
          id-set->list
          id-set-copy
          id-set-copy-clear
          id-set-clear
          id-set-clear!
          id-set-union
          id-set-union!
          id-set-intersect
          id-set-intersect!
          id-set-subtract
          id-set-subtract!
          id-set-symmetric-difference
          id-set-symmetric-difference!
          id-subset?
          id-proper-subset?
          id-set-map
          id-set-for-each)

         ;; set predicates
         (define (id-set-member? s x) 
           (id-table-ref (id-set-table s) x #f))

         (define (id-set=? s1 s2)
           (define table1 (id-set-table s1))
           (define table2 (id-set-table s2))
           (and (for/and ([id (in-dict-keys table1)]) (id-table-ref table2 id #f))
                (for/and ([id (in-dict-keys table2)]) (id-table-ref table1 id #f))))

         ;; add/remove
         (define (id-set-add s x)
           (immutable-id-set (id-table-set (id-set-table s) x #t)))
         (define (id-set-add! s x)
           (id-table-set! (id-set-table s) x #t))
         
         (define (id-set-remove s x)
           (immutable-id-set (id-table-remove (id-set-table s) x)))
         (define (id-set-remove! s x)
           (id-table-remove! (id-set-table s) x))

         (define (id-set-count s)
           (id-table-count (id-set-table s)))
         
         (define (id-set-empty? s)
           (zero? (id-set-count s))) 

         ;; Can't just copy id-table because there's no copy function or dict-copy
         (define (id-set-copy s)
           (if (mutable-id-set? s)
               (mk-mutable-id-set (id-set->list s))
               (mk-immutable-id-set (id-set->list s))))
         
         (define (id-set-copy-clear s)
           (if (mutable-id-set? s)
               (mk-mutable-id-set null)
               (mk-immutable-id-set null)))
         
         (define (id-set-clear s)
           (immutable-id-set (dict-clear (id-set-table s))))
         (define (id-set-clear! s)
           (define table (id-set-table s))
           (dict-clear! table)
           (mutable-id-set table))
        
         ;; set traversals
         (define (id-set-first s)
           (define table (id-set-table s))
           (id-table-iterate-key table (id-table-iterate-first table)))
         
         ;; id-set-rest is undefined for mutable sets
         ;; and thus always returns a mutable set
         (define (id-set-rest s)
           (define table (id-set-table s))
           (define i (id-table-iterate-first table))
           (immutable-id-set (id-table-remove table (id-table-iterate-key table i))))
         
         (define (in-id-set s)
           (in-dict-keys (id-set-table s)))
         (define (id-set->stream s)
           (sequence->stream (in-id-set s)))
         (define (id-set->list s)
           (for/fold ([ids '()]) ([id (in-dict-keys (id-set-table s))])
             (cons id ids)))
         
         ;; -------------------------------------------------------------------
         ;; set operations
         
         (define (choose-immutable cmp set0 other-sets-lst)
           (for/fold ([largest set0]) ([s (in-list other-sets-lst)])
             (if (and (immutable-id-set? s)
                      (cmp (id-set-count s)
                           (id-set-count largest)))
                 s
                 largest)))
         (define (choose-largest-immutable set0 other-sets-lst)
           (choose-immutable > set0 other-sets-lst))
         (define (choose-smallest-immutable set0 other-sets-lst)
           (choose-immutable < set0 other-sets-lst))
         
         (define (id-set-union set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-union "expected immutable id set in: ~a" set0))
           (define largest-immutable (choose-largest-immutable set0 ss))
           (immutable-id-set
            (for/fold
                ([table (id-set-table largest-immutable)])
              ([s (in-list (cons set0 ss))]
               #:unless (eq? s largest-immutable))
              (for/fold ([table table]) ([id (in-dict-keys (id-set-table s))])
                (id-table-set table id #t)))))
         (define (id-set-union! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-union! "expected mutable id set in: ~a" set0))
           (define table (id-set-table set0))
           (for ([s (in-list ss)])
             (for ([id (in-dict-keys (id-set-table s))])
               (id-table-set! table id #t))))
         
         (define (id-set-intersect set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-intersect "expected immutable id set in: ~a" set0))
           (define smallest-immutable (choose-smallest-immutable set0 ss))
           (define smallest-table (id-set-table smallest-immutable))
           (define all-tables-seq (in-list (map id-set-table (cons set0 ss))))
           (define (keep? id)
             (for/and ([table all-tables-seq] #:unless (eq? table smallest-table))
               (id-table-ref table id #f)))
           (immutable-id-set
            (for/fold ([table smallest-table])
              ([id (in-dict-keys smallest-table)] #:unless (keep? id))
              (id-table-remove table id))))     
         (define (id-set-intersect! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-intersect! "expected mutable id set in: ~a" set0))
           (define tables-seq (in-list (map id-set-table ss)))
           (define (keep? id)
             (for/and ([table tables-seq])
               (printf "keep? ~a = ~a\n" id (id-table-ref table id #f))
               (id-table-ref table id #f)))
           (define table0 (id-set-table set0))
           ;; use dict-keys (instead of in-dict-keys) to grab all keys ahead of time
           ;; bc we will possibly be removing some of them
           (for ([id (dict-keys table0)] #:unless (keep? id))
             (id-table-remove! table0 id)))
         
         (define (id-set-subtract set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-subtract "expected immutable id set in: ~a" set0))
           (define tables-seq (in-list (map id-set-table ss)))
           (define (remove? id)
             (for/or ([table tables-seq])
               (id-table-ref table id #f)))
           (define table0 (id-set-table set0))
           (immutable-id-set
            (for/fold ([table table0])
              ([id (in-dict-keys table0)] #:when (remove? id))
              (id-table-remove table id))))
         (define (id-set-subtract! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-subtract! "expected mutable id set in: ~a" set0))
           (define tables-seq (in-list (map id-set-table ss)))
           (define (remove? id)
             (for/or ([table tables-seq])
               (id-table-ref table id #f)))
           (define table0 (id-set-table set0))
           ;; use dict-keys (instead of in-dict-keys) to grab all keys ahead of time
           ;; bc we will possibly be removing some of them
           (for ([id (dict-keys table0)] #:when (remove? id))
             (id-table-remove! table0 id)))
         
         (define (id-set-symmetric-difference set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-symmetric-difference 
                    "expected immutable id set in: ~a" set0))
           (define largest-immutable (choose-largest-immutable set0 ss))
           (immutable-id-set
            (for/fold ([table (id-set-table largest-immutable)])
              ([s (in-list (cons set0 ss))] #:unless (eq? s largest-immutable))
              (for/fold ([table table]) ([id (in-dict-keys (id-set-table s))])
                (if (id-table-ref table id #f)
                    (id-table-remove table id)
                    (id-table-set table id #t))))))
         (define (id-set-symmetric-difference! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-symmetric-difference!
                    "expected mutable id set in: ~a" set0))
           (define table (id-set-table set0))
           (for ([s (in-list ss)])
             (for ([id (in-dict-keys (id-set-table s))])
               (if (id-table-ref table id #f)
                   (id-table-remove! table id)
                   (id-table-set! table id #t)))))
         
         (define (id-subset? s1 s2)
           (define table1 (id-set-table s1))
           (define table2 (id-set-table s2))
           (for/and ([id (in-dict-keys table1)])
             (id-table-ref table2 id #f)))
         
         (define (id-proper-subset? s1 s2)
           (define table1 (id-set-table s1))
           (define table2 (id-set-table s2))
           (and (for/and ([id (in-dict-keys table1)])
                  (id-table-ref table2 id #f))
                (for/or ([id (in-dict-keys table2)])
                  (not (id-table-ref table1 id #f)))))
         
         (define (id-set-map s f)
           (for/fold ([ids null]) ([id (in-dict-keys (id-set-table s))])
             (cons (f id) ids)))
         
         (define (id-set-for-each s f)
           (for ([id (in-dict-keys (id-set-table s))]) (f id)))
         
         ;; -------------------------------------------------------------------
         ;; struct defs
         
         (define id-set-hash-constant
           (equal-hash-code "hash code for free id sets"))
         (define id-set-hash-constant2
           (equal-hash-code "another hash code for free id sets"))
         ;; table: the internal id table
         (struct id-set (table)
           #:property prop:sequence in-id-set
           #:methods gen:equal+hash
           [(define (equal-proc s1 s2 rec)
              (rec (id-set-table s1)
                (id-set-table s2)))
            (define (hash-proc s rec)
              (+ (rec (id-set-table s))
                 id-set-hash-constant))
            (define (hash2-proc s rec)
              (+ (rec (id-set-table s))
                 id-set-hash-constant2))])
         (struct mutable-id-set id-set ()
           #:methods gen:set
           [(define set-empty? id-set-empty?)
            (define set-member? id-set-member?)
            (define set-count id-set-count)
            (define set=? id-set=?)
            (define subset? id-subset?)
            (define proper-subset? id-proper-subset?)
            (define set-map id-set-map)
            (define set-for-each id-set-for-each)
            (define set-copy id-set-copy)
            (define set-copy-clear id-set-copy-clear)
            (define set->list id-set->list)
            (define set->stream id-set->stream)
            (define in-set in-id-set)
            (define set-first id-set-first)
            (define set-add! id-set-add!)
            (define set-remove! id-set-remove!)
            (define set-clear! id-set-clear!)
            (define set-union! id-set-union!)
            (define set-intersect! id-set-intersect!)
            (define set-subtract! id-set-subtract!)
            (define set-symmetric-difference! id-set-symmetric-difference!)])
         (struct immutable-id-set id-set ()
           #:methods gen:stream
           [(define stream-empty? id-set-empty?)
            (define stream-first id-set-first)
            (define stream-rest id-set-rest)]
           #:methods gen:set
           [(define set-empty? id-set-empty?)
            (define set-member? id-set-member?)
            (define set-count id-set-count)
            (define set=? id-set=?)
            (define subset? id-subset?)
            (define proper-subset? id-proper-subset?)
            (define set-map id-set-map)
            (define set-for-each id-set-for-each)
            (define set-copy id-set-copy)
            (define set-copy-clear id-set-copy-clear)
            (define set->list id-set->list)
            (define set->stream id-set->stream)
            (define in-set in-id-set)
            (define set-first id-set-first)
            (define set-rest id-set-rest)
            (define set-add id-set-add)
            (define set-remove id-set-remove)
            (define set-clear id-set-clear)
            (define set-union id-set-union)
            (define set-intersect id-set-intersect)
            (define set-subtract id-set-subtract)
            (define set-symmetric-difference id-set-symmetric-difference)])
         
         (define (mk-mutable-id-set 
                  [init-set null] 
                  #:phase [phase (syntax-local-phase-level)])
           (mutable-id-set 
            (make-mutable-id-table
             (for/hash ([x (in-set init-set)]) (values x #t))
             #:phase phase)))
         
         (define (mk-immutable-id-set
                  [init-set null]
                  #:phase [phase (syntax-local-phase-level)])
           (immutable-id-set 
            (make-immutable-id-table 
             (for/hash ([x (in-set init-set)]) (values x #t))
             #:phase phase)))
         
         )]))

(define-and-provide-id-set-fns #:type free)
(define-and-provide-id-set-fns #:type bound)