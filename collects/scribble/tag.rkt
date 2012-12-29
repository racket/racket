#lang racket/base
(require racket/contract/base
         syntax/modcollapse
         setup/main-collects
         scribble/core
         ;; Needed to normalize planet version numbers:
         (only-in planet/resolver get-planet-module-path/pkg)
         (only-in planet/private/data pkg-maj pkg-min))

(provide
 (contract-out
  [make-section-tag ((string?) 
                     (#:doc (or/c #f module-path?)
                      #:tag-prefixes (or/c #f (listof string?)))
                     . ->* .
                     tag?)]
  [make-module-language-tag (-> symbol? tag?)]
  [taglet? (any/c . -> . boolean?)]
  [module-path-prefix->string (module-path? . -> . string?)]
  [module-path-index->taglet (module-path-index? . -> . taglet?)]
  [intern-taglet (any/c . -> . any/c)]
  [doc-prefix (case->
               ((or/c #f module-path?) taglet? . -> . taglet?)
               ((or/c #f module-path?) (or/c #f (listof string?)) taglet? . -> . taglet?))]))

(define (make-section-tag s #:doc [doc #f] #:tag-prefixes [prefix #f])
  `(part ,(doc-prefix doc prefix s)))

(define (make-module-language-tag langname)
  `(mod-path ,(symbol->string langname)))

(define (taglet? v)
  (and (not (generated-tag? v))
       (tag? (list 'something (list v)))))

(define interned (make-weak-hash))
  
(define (intern-taglet v)
  (let ([v (if (list? v)
               (map intern-taglet v)
               (datum-intern-literal v))])
    (if (or (string? v)
            (bytes? v)
            (list? v))
        (let ([b (hash-ref interned v #f)])
          (if b
              (or (weak-box-value b)
                  ;; just in case the value is GCed before we extract it:
                  (intern-taglet v))
              (begin
                (hash-set! interned v (make-weak-box v))
                v)))
        v)))

(define (do-module-path-index->taglet mod)
  ;; Derive the name from the module path:
  (let ([p (collapse-module-path-index
            mod
            (lambda () (build-path (current-directory) "dummy")))])
    (if (path? p)
        ;; If we got a path back anyway, then it's best to use the resolved
        ;; name; if the current directory has changed since we 
        ;; the path-index was resolved, then p might not be right. Also,
        ;; the resolved path might be a symbol instead of a path.
        (let ([rp (resolved-module-path-name 
                   (module-path-index-resolve mod))])
          (if (path? rp)
              (intern-taglet 
               (path->main-collects-relative rp))
              rp))
        (let ([p (if (and (pair? p)
                          (eq? (car p) 'planet))
                     ;; Normalize planet verion number based on current
                     ;; linking:
                     (let-values ([(path pkg)
                                   (get-planet-module-path/pkg p #f #f)])
                       (list* 'planet
                              (cadr p)
                              (list (car (caddr p))
                                    (cadr (caddr p))
                                    (pkg-maj pkg)
                                    (pkg-min pkg))
                              (cdddr p)))
                     ;; Otherwise the path is fully normalized:
                     p)])
          (intern-taglet p)))))

(define collapsed (make-weak-hasheq))
(define (module-path-index->taglet mod)
  (or (hash-ref collapsed mod #f)
      (let ([v (do-module-path-index->taglet mod)])
        (hash-set! collapsed mod v)
        v)))

(define (module-path-prefix->string p)
  (datum-intern-literal
   (format "~a" (module-path-index->taglet (module-path-index-join p #f)))))

(define doc-prefix
  (case-lambda
   [(doc s)
    (if doc
        (if (list? s)
            (cons (module-path-prefix->string doc) s)
            (list (module-path-prefix->string doc) s))
        s)]
   [(doc prefix s)
    (doc-prefix doc (if prefix
                        (append prefix (if (list? s)
                                           s
                                           (list s)))
                        s))]))

