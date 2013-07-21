#lang racket/base

(require racket/private/dict
         (only-in racket/private/hash paired-fold)
         (only-in racket/syntax format-symbol)
         (for-syntax racket/base racket/syntax))

(define (custom-hash-ref d key [default (key-failure 'dict-ref d key)])
  (dprintf "custom-hash-ref\n")
  (hash-check-key 'dict-ref d key)
  (hash-ref (custom-hash-table d)
            (hash-wrap-key d key)
            default))

(define (custom-hash-set! d key val)
  (dprintf "custom-hash-set!\n")
  (hash-check-key 'dict-set! d key)
  (hash-set! (custom-hash-table d)
             (hash-wrap-key d key)
             val))

(define (custom-hash-set d key val)
  (dprintf "custom-hash-set\n")
  (hash-check-key 'dict-set d key)
  (update-custom-hash-table
   d
   (hash-set (custom-hash-table d)
             (hash-wrap-key d key)
             val)))

(define (custom-hash-remove! d key)
  (dprintf "custom-hash-remove!\n")
  (hash-check-key 'dict-remove! d key)
  (hash-remove! (custom-hash-table d)
                (hash-wrap-key d key)))

(define (custom-hash-remove d key)
  (dprintf "custom-hash-remove\n")
  (hash-check-key 'dict-remove d key)
  (update-custom-hash-table
   d
   (hash-remove (custom-hash-table d)
                (hash-wrap-key d key))))

(define (custom-hash-count d)
  (dprintf "custom-hash-count\n")
  (hash-count (custom-hash-table d)))

(define (custom-hash-iterate-first d)
  (dprintf "custom-hash-iterate-first\n")
  (hash-iterate-first (custom-hash-table d)))

(define (custom-hash-iterate-next d pos)
  (dprintf "custom-hash-iterate-next\n")
  (hash-iterate-next (custom-hash-table d) pos))

(define (custom-hash-iterate-key d pos)
  (dprintf "custom-hash-iterate-key\n")
  (custom-key-contents (hash-iterate-key (custom-hash-table d) pos)))

(define (custom-hash-iterate-value d pos)
  (dprintf "custom-hash-iterate-value\n")
  (hash-iterate-value (custom-hash-table d) pos))

(define (custom-hash-has-key? d key)
  (dprintf "custom-hash-has-key?\n")
  (hash-check-key 'dict-has-key? d key)
  (hash-has-key? (custom-hash-table d)
                 (hash-wrap-key d key)))

(define (custom-hash-ref! d key default)
  (dprintf "custom-hash-ref!\n")
  (hash-check-key 'dict-ref! d key)
  (hash-ref! (custom-hash-table d)
             (hash-wrap-key d key)
             default))

(define (custom-hash-set*! d . pairs)
  (dprintf "custom-hash-set*!\n")
  (define table (custom-hash-table d))
  (paired-fold 'dict-set*! pairs (void)
              (lambda (x k v)
                (hash-check-key 'dict-set*! d k)
                (hash-set! table (hash-wrap-key d k) v))))

(define (custom-hash-set* d . pairs)
  (dprintf "custom-hash-set*\n")
  (update-custom-hash-table
   d
   (paired-fold 'dict-set* pairs (custom-hash-table d)
               (lambda (table k v)
                 (hash-check-key 'dict-set* d k)
                 (hash-set table (hash-wrap-key d k) v)))))

(define (custom-hash-update! d key proc
                             [default (key-failure 'dict-update! d key)])
  (dprintf "custom-hash-update!\n")
  (hash-check-key 'dict-update! d key)
  (hash-update! (custom-hash-table d)
                (hash-wrap-key d key)
                proc
                default))

(define (custom-hash-update d key proc
                            [default (key-failure 'dict-update d key)])
  (dprintf "custom-hash-update\n")
  (hash-check-key 'dict-update d key)
  (update-custom-hash-table
   d
   (hash-update (custom-hash-table d)
                (hash-wrap-key d key)
                proc
                default)))

(define (custom-hash-map d proc)
  (dprintf "custom-hash-map\n")
  (hash-map (custom-hash-table d)
            (lambda (k v)
              (proc (custom-key-contents k) v))))

(define (custom-hash-for-each d proc)
  (dprintf "custom-hash-for-each\n")
  (hash-for-each (custom-hash-table d)
                 (lambda (k v)
                   (proc (custom-key-contents k) v))))

;; custom-hash-keys, -values, and -list:
;; for/fold instead of for/list because order doesn't matter
;; and it saves the time and allocation of calling reverse

(define (custom-hash-keys d)
  (dprintf "custom-hash-keys\n")
  (for/fold ([keys '()]) ([k (in-hash-keys (custom-hash-table d))])
    (cons (custom-key-contents k) keys)))

(define (custom-hash-values d)
  (dprintf "custom-hash-values\n")
  (for/fold ([vals '()]) ([v (in-hash-values (custom-hash-table d))])
    (cons v vals)))

(define (custom-hash-copy d)
  (dprintf "custom-hash-copy\n")
  (update-custom-hash-table d (hash-copy (custom-hash-table d))))

(define (custom-hash->list d)
  (dprintf "custom-hash->list\n")
  (for/fold ([pairs '()]) ([(k v) (in-hash (custom-hash-table d))])
    (cons (cons (custom-key-contents k) v) pairs)))

(define (custom-hash-empty? d)
  (dprintf "custom-hash-empty?\n")
  (hash-empty? (custom-hash-table d)))

(define (custom-hash-clear d)
  (dprintf "custom-hash-clear\n")
  (update-custom-hash-table
   d
   (hash-clear (custom-hash-table d))))

(define (custom-hash-clear! d)
  (dprintf "custom-hash-clear!\n")
  (hash-clear! (custom-hash-table d)))

(define (hash-wrap-key d key)
  (define spec (custom-hash-spec d))
  (wrap-key spec key))

(define (wrap-key spec key)
  (define wrap (custom-spec-wrap spec))
  (define intern (custom-spec-intern spec))
  (ephemeron-value
   (hash-ref! intern key
     (lambda ()
       (make-ephemeron key (wrap key))))))

(define (hash-check-key who d key)
  (define spec (custom-hash-spec d))
  (check-key who spec key))

(define (check-key who spec key)
  (define key? (custom-spec-key? spec))
  (unless (key? key)
    (raise-argument-error who (format "~a" key?) key)))

(define (update-custom-hash-table d table)
  (cond
    [(immutable? table) (immutable-custom-hash (custom-hash-spec d) table)]
    [(hash-weak? table) (weak-custom-hash (custom-hash-spec d) table)]
    [else (mutable-custom-hash (custom-hash-spec d) table)]))

(define (key-failure who d key)
  (lambda ()
    (raise-arguments-error who
                           "no value found for key"
                           "key" key
                           "all keys" (dict-keys d))))

(struct custom-key [contents])

(struct custom-spec [key? wrap intern])

(struct custom-hash [spec table]
  #:methods gen:equal+hash
  [(define (equal-proc x y rec)
     (and (eq? (custom-hash-spec x)
               (custom-hash-spec y))
          (rec (custom-hash-table x)
               (custom-hash-table y))))
   (define (hash-proc x rec)
     (+ (eq-hash-code (custom-hash-spec x))
        (rec (custom-hash-table x))
        custom-hash-constant))
   (define (hash2-proc x rec)
     (rec (custom-hash-table x)))])

(define custom-hash-constant
  (equal-hash-code "hash code for a custom hash table"))

(struct immutable-custom-hash custom-hash []
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set custom-hash-set)
   (define dict-remove custom-hash-remove)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)
   (define dict-has-key? custom-hash-has-key?)
   (define dict-set* custom-hash-set*)
   (define dict-update custom-hash-update)
   (define dict-map custom-hash-map)
   (define dict-for-each custom-hash-for-each)
   (define dict-keys custom-hash-keys)
   (define dict-values custom-hash-values)
   (define dict->list custom-hash->list)
   (define dict-empty? custom-hash-empty?)
   (define dict-clear custom-hash-clear)])

(struct imperative-custom-hash custom-hash []
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set! custom-hash-set!)
   (define dict-remove! custom-hash-remove!)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)
   (define dict-has-key? custom-hash-has-key?)
   (define dict-ref! custom-hash-ref!)
   (define dict-set*! custom-hash-set*!)
   (define dict-update! custom-hash-update!)
   (define dict-map custom-hash-map)
   (define dict-for-each custom-hash-for-each)
   (define dict-keys custom-hash-keys)
   (define dict-values custom-hash-values)
   (define dict-copy custom-hash-copy)
   (define dict->list custom-hash->list)
   (define dict-empty? custom-hash-empty?)
   (define dict-clear custom-hash-clear)
   (define dict-clear! custom-hash-clear!)])

(struct weak-custom-hash imperative-custom-hash [])

(struct mutable-custom-hash imperative-custom-hash [])

(define-syntax (define-custom-hash-types stx)
  (parameterize ([current-syntax-context stx])
    (define-values (base-id args-stx)
      (syntax-case stx ()
        [(_ name #:key? key? =? hc1 hc2)
         (values #'name #'(#:key? key? =? hc1 hc2))]
        [(_ name #:key? key? =? hc1)
         (values #'name #'(#:key? key? =? hc1))]
        [(_ name #:key? key? =?)
         (values #'name #'(#:key? key? =?))]
        [(_ name =? hc1 hc2)
         (values #'name #'(=? hc1 hc2))]
        [(_ name =? hc1)
         (values #'name #'(=? hc1))]
        [(_ name =?)
         (values #'name #'(=?))]))
    (unless (identifier? base-id)
      (wrong-syntax base-id "expected an identifier"))
    (define (id fmt) (format-id base-id fmt base-id))
    (define/with-syntax (name
                         name?
                         immutable-name?
                         mutable-name?
                         weak-name?
                         make-immutable-name
                         make-mutable-name
                         make-weak-name)
      (list (id "~a")
            (id "~a?")
            (id "immutable-~a?")
            (id "mutable-~a?")
            (id "weak-~a?")
            (id "make-immutable-~a")
            (id "make-mutable-~a")
            (id "make-weak-~a")))
    (define/with-syntax args args-stx)
    #'(define-values (name?
                      immutable-name?
                      mutable-name?
                      weak-name?
                      make-immutable-name
                      make-mutable-name
                      make-weak-name)
        (make-custom-hash-types #:for 'define-custom-hash-types
                                #:name 'name
                                . args))))

(define (make-custom-hash-types =? [hc1 default-hc] [hc2 default-hc]
                                #:key? [key? default-pred]
                                #:for [who 'make-custom-hash-types]
                                #:name [name 'custom-hash])
  (define spec (make-custom-spec who key? =? hc1 hc2))
  (define (sym fmt) (format-symbol fmt name))
  (values (custom-hash-predicate spec (sym "~a?"))
          (immutable-custom-hash-predicate spec (sym "immutable-~a?"))
          (mutable-custom-hash-predicate spec (sym "mutable-~a?"))
          (weak-custom-hash-predicate spec (sym "weak-~a?"))
          (immutable-custom-hash-maker spec (sym "make-immutable-~a"))
          (mutable-custom-hash-maker spec (sym "make-mutable-~a"))
          (weak-custom-hash-maker spec (sym "make-weak-~a"))))

(define (make-custom-hash =? [hc1 default-hc] [hc2 default-hc]
                          #:key? [key? default-pred])
  (define spec (make-custom-spec 'make-custom-hash key? =? hc1 hc2))
  (define make (mutable-custom-hash-maker spec 'make))
  (make))

(define (make-weak-custom-hash =? [hc1 default-hc] [hc2 default-hc]
                          #:key? [key? default-pred])
  (define spec (make-custom-spec 'make-custom-hash key? =? hc1 hc2))
  (define make (weak-custom-hash-maker spec 'make))
  (make))

(define (make-immutable-custom-hash =? [hc1 default-hc] [hc2 default-hc]
                          #:key? [key? default-pred])
  (define spec (make-custom-spec 'make-custom-hash key? =? hc1 hc2))
  (define make (immutable-custom-hash-maker spec 'make))
  (make))

(define (make-custom-spec who key? =? hc1 hc2)
  (check-arities who =? 2 3)
  (check-arities who hc1 1 2)
  (check-arities who hc2 1 2)
  (check-arity who key? 1)
  (struct wrapped-key custom-key []
    #:methods gen:equal+hash
    [(define equal-proc
       (if (procedure-arity-includes? =? 2)
           (lambda (a b f)
             (=? (custom-key-contents a)
                 (custom-key-contents b)))
           (lambda (a b f)
             (=? (custom-key-contents a)
                 (custom-key-contents b)
                 f))))
     (define hash-proc
       (if (procedure-arity-includes? hc1 1)
           (lambda (a f)
             (hc1 (custom-key-contents a)))
           (lambda (a f)
             (hc1 (custom-key-contents a) f))))
     (define hash2-proc
       (if (procedure-arity-includes? hc2 1)
           (lambda (a f)
             (hc2 (custom-key-contents a)))
           (lambda (a f)
             (hc2 (custom-key-contents a) f))))])
  (custom-spec key? wrapped-key (make-weak-hasheq)))

(define (default-hc x f) 1)
(define (default-pred x) #t)

(define (check-arities who f a b)
  (unless (and (procedure? f)
               (or (procedure-arity-includes? f a)
                   (procedure-arity-includes? f b)))
    (raise-argument-error who (arities-string a b) f)))

(define (check-arity who f a)
  (unless (and (procedure? f)
               (procedure-arity-includes? f a))
    (raise-argument-error who (arity-string a) f)))

(define (arities-string a b)
  (format "(or/c ~a ~a)" (arity-string a) (arity-string b)))

(define (arity-string a)
  (format "(procedure-arity-includes/c ~a)" a))

(define (custom-hash-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (custom-hash? x)
         (eq? (custom-hash-spec x) spec)))
  (procedure-rename proc name))

(define (weak-custom-hash-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (weak-custom-hash? x)
         (eq? (custom-hash-spec x) spec)))
  (procedure-rename proc name))

(define (mutable-custom-hash-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (mutable-custom-hash? x)
         (eq? (custom-hash-spec x) spec)))
  (procedure-rename proc name))

(define (immutable-custom-hash-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (immutable-custom-hash? x)
         (eq? (custom-hash-spec x) spec)))
  (procedure-rename proc name))

(define (immutable-custom-hash-maker spec name)
  (define (proc [dict '()])
    (dprintf "~a\n" name)
    (define table
      (for/fold ([table (make-immutable-hash)]) ([(k v) (in-dict dict)])
        (check-key name spec k)
        (hash-set table (wrap-key spec k) v)))
    (immutable-custom-hash spec table))
  (procedure-rename proc name))

(define (imperative-custom-hash-maker spec name make-table make-dict)
  (define (proc [dict '()])
    (dprintf "~a\n" name)
    (define table (make-table))
    (for ([(k v) (in-dict dict)])
      (check-key name spec k)
      (hash-set! table (wrap-key spec k) v))
    (make-dict spec table))
  (procedure-rename proc name))

(define (mutable-custom-hash-maker spec name)
  (imperative-custom-hash-maker spec name make-hash mutable-custom-hash))

(define (weak-custom-hash-maker spec name)
  (imperative-custom-hash-maker spec name make-weak-hash weak-custom-hash))

(define dprintf void)

(provide make-custom-hash
         make-immutable-custom-hash
         make-weak-custom-hash
         make-custom-hash-types
         define-custom-hash-types)
