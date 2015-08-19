#lang racket/base
(require syntax/parse/private/residual-ct ;; keep abs. path
         racket/contract/base
         syntax/private/id-table
         racket/syntax
         "make.rkt")

#|
An IAttr is (make-attr identifier number boolean)
An SAttr is (make-attr symbol number boolean)

The number is the ellipsis nesting depth. The boolean is true iff the
attr is guaranteed to be bound to a value which is a syntax object (or
a list^depth of syntax objects).
|#

#|
SAttr lists are always stored in sorted order, to make comparison
of signatures easier for reified syntax-classes.
|#

(define (iattr? a)
  (and (attr? a) (identifier? (attr-name a))))

(define (sattr? a)
  (and (attr? a) (symbol? (attr-name a))))

;; increase-depth : Attr -> Attr
(define (increase-depth x)
  (make attr (attr-name x) (add1 (attr-depth x)) (attr-syntax? x)))

(provide/contract
 [iattr? (any/c . -> . boolean?)]
 [sattr? (any/c . -> . boolean?)]

 [increase-depth
  (-> attr? attr?)]
 [attr-make-uncertain
  (-> attr? attr?)]

 ;; IAttr operations
 [append-iattrs
  (-> (listof (listof iattr?))
      (listof iattr?))]
 [union-iattrs
  (-> (listof (listof iattr?))
      (listof iattr?))]
 [reorder-iattrs
  (-> (listof sattr?) (listof iattr?)
      (listof iattr?))]
 [rename-attr
  (-> iattr? identifier?
      iattr?)]

 ;; SAttr operations
 [iattr->sattr
  (-> iattr?
      sattr?)]
 [iattrs->sattrs
  (-> (listof iattr?)
      (listof sattr?))]
 [sort-sattrs
  (-> (listof sattr?)
      (listof sattr?))]

 [intersect-sattrss
  (-> (listof (listof sattr?))
      (listof sattr?))]

 [check-iattrs-subset
  (-> (listof iattr?)
      (listof iattr?)
      (or/c syntax? false/c)
      any)])

;; IAttr operations

;; append-iattrs : (listof (listof IAttr)) -> (listof IAttr)
(define (append-iattrs attrss)
  (let* ([all (apply append attrss)]
         [names (map attr-name all)]
         [dup (check-duplicate-identifier names)])
    (when dup
      (wrong-syntax dup "duplicate attribute"))
    all))

;; union-iattrs : (listof (listof IAttr)) -> (listof IAttr)
(define (union-iattrs attrss)
  (define count-t (make-bound-id-table))
  (define attr-t (make-bound-id-table))
  (define list-count (length attrss))
  (define attr-keys null)
  (for* ([attrs (in-list attrss)] [attr (in-list attrs)])
    (define name (attr-name attr))
    (define prev (bound-id-table-ref attr-t name #f))
    (unless prev (set! attr-keys (cons name attr-keys)))
    (bound-id-table-set! attr-t name (join-attrs attr prev))
    (let ([pc (bound-id-table-ref count-t name 0)])
      (bound-id-table-set! count-t name (add1 pc))))
  (for/list ([k (in-list attr-keys)])
    (define a (bound-id-table-ref attr-t k))
    (if (= (bound-id-table-ref count-t (attr-name a)) list-count)
        a
        (attr-make-uncertain a))))

;; join-attrs : Attr Attr/#f -> Attr
;; Works with both IAttrs and SAttrs.
;; Assumes attrs have same name.
(define (join-attrs a b)
  (if (and a b)
      (proper-join-attrs a b)
      (or a b)))

(define (proper-join-attrs a b)
  (let ([aname (attr-name a)])
    (unless (equal? (attr-depth a) (attr-depth b))
      (wrong-syntax (and (syntax? aname) aname)
                    "attribute '~a' occurs with different nesting depth"
                    (if (syntax? aname) (syntax-e aname) aname)))
    (make attr aname (attr-depth a) (and (attr-syntax? a) (attr-syntax? b)))))

(define (attr-make-uncertain a)
  (make attr (attr-name a) (attr-depth a) #f))

(define (iattr->sattr a)
  (let ([name (attr-name a)]
        [depth (attr-depth a)]
        [syntax? (attr-syntax? a)])
     (make attr (syntax-e name) depth syntax?)))

(define (iattrs->sattrs as)
  (sort-sattrs (map iattr->sattr as)))

(define (sort-sattrs as)
  (sort as string<?
        #:key (lambda (a) (symbol->string (attr-name a)))
        #:cache-keys? #t))

(define (rename-attr a name)
  (make attr name (attr-depth a) (attr-syntax? a)))

;; intersect-sattrss : (listof (listof SAttr)) -> (listof SAttr)
;; FIXME: rely on sorted inputs, simplify algorithm and avoid second sort?
(define (intersect-sattrss attrss)
  (cond [(null? attrss) null]
        [else
         (let* ([namess (map (lambda (attrs) (map attr-name attrs)) attrss)]
                [names (filter (lambda (s)
                                 (andmap (lambda (names) (memq s names))
                                         (cdr namess)))
                               (car namess))]
                [ht (make-hasheq)]
                [put (lambda (attr) (hash-set! ht (attr-name attr) attr))]
                [fetch-like (lambda (attr) (hash-ref ht (attr-name attr) #f))])
           (for* ([attrs (in-list attrss)]
                  [attr (in-list attrs)]
                  #:when (memq (attr-name attr) names))
             (put (join-attrs attr (fetch-like attr))))
           (sort-sattrs (hash-map ht (lambda (k v) v))))]))

;; reorder-iattrs : (listof SAttr) (listof IAttr) -> (listof IAttr)
;; Reorders iattrs (and restricts) based on relsattrs
;; If a relsattr is not found, or if depth or contents mismatches, raises error.
(define (reorder-iattrs relsattrs iattrs)
  (let ([ht (make-hasheq)])
    (for ([iattr (in-list iattrs)])
      (let ([remap-name (syntax-e (attr-name iattr))])
        (hash-set! ht remap-name iattr)))
    (let loop ([relsattrs relsattrs])
      (if (null? relsattrs)
          null
          (let ([sattr (car relsattrs)]
                [rest (cdr relsattrs)])
            (let ([iattr (hash-ref ht (attr-name sattr) #f)])
              (check-iattr-satisfies-sattr iattr sattr)
              (cons iattr (loop rest))))))))

(define (check-iattr-satisfies-sattr iattr sattr)
  (unless iattr
    (wrong-syntax #f "required attribute is not defined: ~s" (attr-name sattr)))
  (unless (= (attr-depth iattr) (attr-depth sattr))
    (wrong-syntax (attr-name iattr)
                  "attribute has wrong depth (expected ~s, found ~s)"
                  (attr-depth sattr) (attr-depth iattr)))
  (when (and (attr-syntax? sattr) (not (attr-syntax? iattr)))
    (wrong-syntax (attr-name iattr)
                  "attribute may not be bound to syntax: ~s"
                  (attr-name sattr))))

;; check-iattrs-subset : (listof IAttr) (listof IAttr) stx -> void
(define (check-iattrs-subset little big ctx)
  (define big-t (make-bound-id-table))
  (for ([a (in-list big)])
    (bound-id-table-set! big-t (attr-name a) #t))
  (for ([a (in-list little)])
    (unless (bound-id-table-ref big-t (attr-name a) #f)
      (raise-syntax-error #f
                          "attribute bound in defaults but not in pattern"
                          ctx
                          (attr-name a)))))
