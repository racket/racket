#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/stx racket/syntax)
         racket/set racket/dict racket/sequence racket/stream racket/contract
         syntax/id-table)
  
;; id formatting helper fns ---------------------------------------------------
;; mk-pred-name : identifier -> identifier
(define-for-syntax (fmt-pred-name id) (format-id id "~a?" id))
;; fmt-set-id-fn-name : identifier string -> identifier
;; (fmt-set-id-fn-name #'free "empty?") => #'free-id-set-empty?
(define-for-syntax (fmt-set-id-fn-name set-id-type set-fn-name)
  (format-id set-id-type (string-append "~a-id-set-" set-fn-name) set-id-type))
;; fmt-table-id-fn-name : identifier string -> identifier
;; (fmt-table-id-fn-name #'free "ref") => #'free-id-table-ref
(define-for-syntax (fmt-tbl-id-fn-name tbl-id-type tbl-fn-name)
  (format-id tbl-id-type (string-append "~a-id-table-" tbl-fn-name) tbl-id-type))
;; fmt-id : string identifier -> identifier
;; format id where id is used as both the ctx and the single str escape
(define-for-syntax (fmt-id str-pat id) (format-id id str-pat id))
                
(provide (for-syntax fmt-id fmt-set-id-fn-name fmt-pred-name))

;; defines and provides functions for an identifier set,
;; where type = free or bound
(define-syntax (define-and-provide-id-set stx)
  (syntax-parse stx
    [(_ #:type type)
     ;; names for id-set fns --------------------------------------------------
     #:with id-set (fmt-id "~a-id-set" #'type)
     #:with mutable-id-set (fmt-id "mutable-~a" #'id-set)
     #:with mk-mutable-id-set (fmt-id"make-~a" #'mutable-id-set)
     #:with immutable-id-set (fmt-id "immutable-~a" #'id-set)
     #:with mk-immutable-id-set (fmt-id "make-~a" #'immutable-id-set)
     #:with id-set? (fmt-pred-name #'id-set)
     #:with mutable-id-set? (fmt-pred-name #'mutable-id-set)
     #:with immutable-id-set? (fmt-pred-name #'immutable-id-set)
     #:with chaperone-mutable-id-set (fmt-id "chaperone-~a" #'mutable-id-set)
     #:with chaperone-immutable-id-set (fmt-id "chaperone-~a" #'immutable-id-set)
     #:with id-set-get-table (fmt-set-id-fn-name #'type "table") ; internal table accessor
     #:with id-set-empty? (fmt-set-id-fn-name #'type "empty?")
     #:with id-set-count (fmt-set-id-fn-name #'type "count")
     #:with id-set-member? (fmt-set-id-fn-name #'type "member?")
     #:with id-set-add (fmt-set-id-fn-name #'type "add")
     #:with id-set-add! (fmt-set-id-fn-name #'type "add!")
     #:with id-set-remove (fmt-set-id-fn-name #'type "remove")
     #:with id-set-remove! (fmt-set-id-fn-name #'type "remove!")
     #:with id-set-first (fmt-set-id-fn-name #'type "first")
     #:with id-set-rest (fmt-set-id-fn-name #'type "rest")
     #:with id-set->stream (fmt-set-id-fn-name #'type ">stream")
     #:with id-set->list (fmt-set-id-fn-name #'type ">list")
     #:with id-set-copy (fmt-set-id-fn-name #'type "copy")
     #:with id-set-copy-clear (fmt-set-id-fn-name #'type "copy-clear")
     #:with id-set-clear (fmt-set-id-fn-name #'type "clear")
     #:with id-set-clear! (fmt-set-id-fn-name #'type "clear!")
     #:with id-set-union (fmt-set-id-fn-name #'type "union")
     #:with id-set-union! (fmt-set-id-fn-name #'type "union!")
     #:with id-set-intersect (fmt-set-id-fn-name #'type "intersect")
     #:with id-set-intersect! (fmt-set-id-fn-name #'type "intersect!")
     #:with id-set-subtract (fmt-set-id-fn-name #'type "subtract")
     #:with id-set-subtract! (fmt-set-id-fn-name #'type "subtract!")
     #:with id-set-symmetric-difference (fmt-set-id-fn-name #'type "symmetric-difference")
     #:with id-set-symmetric-difference! (fmt-set-id-fn-name #'type "symmetric-difference!")
     #:with id-set-map (fmt-set-id-fn-name #'type "map")
     #:with id-set-for-each (fmt-set-id-fn-name #'type "for-each")
     ;; these fns don't have the conventional (eg "free-id-set-") prefix
     #:with id-subset? (fmt-id "~a-id-subset?" #'type)
     #:with id-proper-subset? (fmt-id "~a-id-proper-subset?" #'type)
     #:with in-id-set (fmt-id "in-~a-id-set" #'type)
     #:with id-set=? (fmt-id "~a-id-set=?" #'type)
     ;; names for id-table fns ------------------------------------------------
     #:with make-mutable-id-table (fmt-id "make-~a-id-table" #'type)
     #:with make-immutable-id-table (fmt-id "make-immutable-~a-id-table" #'type)
     #:with id-table-ref (fmt-tbl-id-fn-name #'type "ref")
     #:with id-table-set (fmt-tbl-id-fn-name #'type "set")
     #:with id-table-set! (fmt-tbl-id-fn-name #'type "set!")
     #:with id-table-remove (fmt-tbl-id-fn-name #'type "remove")
     #:with id-table-remove! (fmt-tbl-id-fn-name #'type "remove!")
     #:with id-table-map (fmt-tbl-id-fn-name #'type "map")
     #:with id-table-for-each (fmt-tbl-id-fn-name #'type "for-each")
     #:with id-table-count (fmt-tbl-id-fn-name #'type "count")
     #:with id-table-first (fmt-tbl-id-fn-name #'type "first")
     #:with id-table-next (fmt-tbl-id-fn-name #'type "next")
     #:with id-table-iterate-key  (fmt-tbl-id-fn-name #'type "iterate-key")
     #:with id-table-iterate-first (fmt-tbl-id-fn-name #'type "iterate-first")
     #'(begin
         (provide 
          (rename-out [mk-mutable-id-set mutable-id-set]
                      [mk-immutable-id-set immutable-id-set])
          chaperone-mutable-id-set
          chaperone-immutable-id-set
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

         ;; implementations here are copied from racket/private/set-types.rkt
         
         ;; set predicates
         (define (id-set-member? s x) 
           (id-table-ref (id-set-get-table s) x #f))

         (define (id-set=? s1 s2)
           (define table1 (id-set-get-table s1))
           (define table2 (id-set-get-table s2))
           (and (for/and ([id (in-dict-keys table1)]) (id-table-ref table2 id #f))
                (for/and ([id (in-dict-keys table2)]) (id-table-ref table1 id #f))))

         (define (id-set-count s)
           (id-table-count (id-set-get-table s)))
         
         (define (id-set-empty? s)
           (zero? (id-set-count s))) 

         ;; add/remove/copy
         (define (id-set-add s x)
           (immutable-id-set (id-table-set (id-set-get-table s) x #t)))
         (define (id-set-add! s x)
           (id-table-set! (id-set-get-table s) x #t))
         
         (define (id-set-remove s x)
           (immutable-id-set (id-table-remove (id-set-get-table s) x)))
         (define (id-set-remove! s x)
           (id-table-remove! (id-set-get-table s) x))

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
           (immutable-id-set (dict-clear (id-set-get-table s))))
         (define (id-set-clear! s)
           (define table (id-set-get-table s))
           (dict-clear! table)
           (mutable-id-set table))
        
         ;; set traversals
         (define (id-set-first s)
           (define table (id-set-get-table s))
           (id-table-iterate-key table (id-table-iterate-first table)))
         
         ;; id-set-rest is undefined for mutable sets
         ;; and thus always returns a mutable set
         (define (id-set-rest s)
           (define table (id-set-get-table s))
           (define i (id-table-iterate-first table))
           (immutable-id-set (id-table-remove table (id-table-iterate-key table i))))
         
         (define (in-id-set s) (in-dict-keys (id-set-get-table s)))
         (define (id-set->stream s) (sequence->stream (in-id-set s)))
         (define (id-set->list s) (dict-keys (id-set-get-table s)))
         
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
             ([table (id-set-get-table largest-immutable)])
             ([s (in-list (cons set0 ss))]
              #:unless (eq? s largest-immutable))
              (for/fold ([table table])
                        ([id (in-dict-keys (id-set-get-table s))])
                (id-table-set table id #t)))))
         (define (id-set-union! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-union! "expected mutable id set in: ~a" set0))
           (define table (id-set-get-table set0))
           (for ([s (in-list ss)])
             (for ([id (in-dict-keys (id-set-get-table s))])
               (id-table-set! table id #t))))
         
         (define (id-set-intersect set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-intersect "expected immutable id set in: ~a" set0))
           (define smallest-immutable (choose-smallest-immutable set0 ss))
           (define smallest-table (id-set-get-table smallest-immutable))
           (define all-tables-seq (in-list (map id-set-get-table (cons set0 ss))))
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
           (define tables-seq (in-list (map id-set-get-table ss)))
           (define (keep? id)
             (for/and ([table tables-seq]) (id-table-ref table id #f)))
           (define table0 (id-set-get-table set0))
           ;; use dict-keys (instead of in-dict-keys) to grab all keys ahead of time
           ;; bc we will possibly be removing some of them
           (for ([id (dict-keys table0)] #:unless (keep? id))
             (id-table-remove! table0 id)))
         
         (define (id-set-subtract set0 . ss)
           (unless (immutable-id-set? set0)
             (error 'id-set-subtract "expected immutable id set in: ~a" set0))
           (define tables-seq (in-list (map id-set-get-table ss)))
           (define (remove? id)
             (for/or ([table tables-seq])
               (id-table-ref table id #f)))
           (define table0 (id-set-get-table set0))
           (immutable-id-set
            (for/fold ([table table0])
                      ([id (in-dict-keys table0)] #:when (remove? id))
              (id-table-remove table id))))
         (define (id-set-subtract! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-subtract! "expected mutable id set in: ~a" set0))
           (define tables-seq (in-list (map id-set-get-table ss)))
           (define (remove? id)
             (for/or ([table tables-seq])
               (id-table-ref table id #f)))
           (define table0 (id-set-get-table set0))
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
            (for/fold 
             ([table (id-set-get-table largest-immutable)])
             ([s (in-list (cons set0 ss))] #:unless (eq? s largest-immutable))
              (for/fold ([table table]) 
                        ([id (in-dict-keys (id-set-get-table s))])
                (if (id-table-ref table id #f)
                    (id-table-remove table id)
                    (id-table-set table id #t))))))
         (define (id-set-symmetric-difference! set0 . ss)
           (unless (mutable-id-set? set0)
             (error 'id-set-symmetric-difference!
                    "expected mutable id set in: ~a" set0))
           (define table (id-set-get-table set0))
           (for ([s (in-list ss)])
             (for ([id (in-dict-keys (id-set-get-table s))])
               (if (id-table-ref table id #f)
                   (id-table-remove! table id)
                   (id-table-set! table id #t)))))
         
         (define (id-subset? s1 s2)
           (define table1 (id-set-get-table s1))
           (define table2 (id-set-get-table s2))
           (for/and ([id (in-dict-keys table1)])
             (id-table-ref table2 id #f)))
         
         (define (id-proper-subset? s1 s2)
           (define table1 (id-set-get-table s1))
           (define table2 (id-set-get-table s2))
           (and (for/and ([id (in-dict-keys table1)])
                  (id-table-ref table2 id #f))
                (for/or ([id (in-dict-keys table2)])
                  (not (id-table-ref table1 id #f)))))
         
         (define (id-set-map s f)
           (for/fold ([ids null]) ([id (in-dict-keys (id-set-get-table s))])
             (cons (f id) ids)))
         
         (define (id-set-for-each s f)
           (for ([id (in-dict-keys (id-set-get-table s))]) (f id)))
         
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
              (rec (id-set-get-table s1)
                (id-set-get-table s2)))
            (define (hash-proc s rec)
              (+ (rec (id-set-get-table s))
                 id-set-hash-constant))
            (define (hash2-proc s rec)
              (+ (rec (id-set-get-table s))
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
         
         ;; consumes a contract for the internal table
         (define (chaperone-immutable-id-set s table/c)
           (define/contract table/ctc table/c (id-set-get-table s))
           (immutable-id-set table/ctc))
         (define (chaperone-mutable-id-set s table/c)
           (define/contract table/ctc table/c (id-set-get-table s))
           (mutable-id-set table/ctc))
            
         (define (mk-mutable-id-set 
                  [init-set null] 
                  #:phase [phase (syntax-local-phase-level)])
           (mutable-id-set 
            (make-mutable-id-table
             (for/hash ([x (in-set init-set)]) 
               (unless (identifier? x)
                 (raise-type-error (object-name mutable-id-set)
                                   "set with identifier keys" init-set))
               (values x #t))
             #:phase phase)))
         
         (define (mk-immutable-id-set
                  [init-set null]
                  #:phase [phase (syntax-local-phase-level)])
           (immutable-id-set 
            (make-immutable-id-table 
             (for/hash ([x (in-set init-set)]) 
               (unless (identifier? x)
                 (raise-type-error (object-name immutable-id-set)
                                   "set with identifier keys" init-set))
               (values x #t))
             #:phase phase))))]))

(define-and-provide-id-set #:type free)
(define-and-provide-id-set #:type bound)