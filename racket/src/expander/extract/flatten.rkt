#lang racket/base
(require "../common/set.rkt"
         "../run/status.rkt"
         "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt"
         "variable.rkt"
         "symbol.rkt"
         "primitive-table.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt"))

(provide flatten!)

(define (flatten! start-link
                  #:linklets linklets
                  #:linklets-in-order linklets-in-order
                  #:needed needed
                  #:exports exports
                  #:instance-knot-ties instance-knot-ties
                  #:primitive-table-directs primitive-table-directs
                  #:check-later-vars check-later-vars)
  (log-status "Flattening to a single linklet...")
  (define needed-linklets-in-order
    (for/list ([lnk (in-list (unbox linklets-in-order))]
               #:when (hash-ref needed lnk #f))
      lnk))

  ;; variable -> symbol
  (define variable-names (pick-variable-names
                          #:linklets linklets
                          #:needed-linklets-in-order needed-linklets-in-order
                          #:instance-knot-ties instance-knot-ties))

  (for ([var (in-hash-keys variable-names)]
        #:when (symbol? (link-name (variable-link var)))
        #:unless (hash-ref check-later-vars var #f))
    (error 'flatten "found a dependency on a non-primitive: ~s from ~s"
           (variable-name var)
           (link-name (variable-link var))))

  (values
   variable-names
   `(linklet
     ;; imports
     ()
     ;; exports
     ,(for/list ([ex-sym (in-list (sort (hash-keys exports) symbol<?))])
        (define var (hash-ref exports ex-sym))
        (define int-sym (hash-ref variable-names var #f))
        (unless int-sym
          (error 'flatten "export does not map to an instance variable: ~s" ex-sym))
        `[,int-sym ,ex-sym])
     ;; body
     ,@(apply
        append
        (for/list ([lnk (in-list (reverse needed-linklets-in-order))])
          (define body
            (body-with-substituted-variable-names lnk
                                                  (hash-ref linklets lnk)
                                                  variable-names
                                                  #:linklets linklets
                                                  #:instance-knot-ties instance-knot-ties))
          (substitute-primitive-table-access body primitive-table-directs))))))

(define (pick-variable-names #:linklets linklets
                             #:needed-linklets-in-order needed-linklets-in-order
                             #:instance-knot-ties instance-knot-ties)
  ;; We need to pick a name for each needed linklet's definitions plus
  ;; each primitive import. Start by checking which names are
  ;; currently used.
  (define variable-locals (make-hash)) ; variable -> set-of-symbol
  (define all-variables null) ; domain of `variable-locals` in an order
  (define otherwise-used-symbols (seteq))
  
  (for ([lnk (in-list needed-linklets-in-order)])
    (define li (hash-ref linklets lnk))
    (define linklet (linklet-info-linklet li))
    (define importss+localss
      (skip-abi-imports (bootstrap:s-expr-linklet-importss+localss linklet)))
    (define exports+locals
      (bootstrap:s-expr-linklet-exports+locals linklet))
    (define all-mentioned-symbols
      (all-used-symbols (bootstrap:s-expr-linklet-body linklet)))
    
    (define (record! lnk external+local knot-ties)
      (cond
       [(find-knot-tying-alternate knot-ties lnk (car external+local) linklets)
        => (lambda (alt-lnk)
             (unless (eq? alt-lnk 'ignore)
               (record! alt-lnk external+local knot-ties)))]
       [else
        (define var (variable lnk (car external+local)))
        (unless (hash-ref variable-locals var #f)
          (set! all-variables (cons var all-variables)))
        (hash-update! variable-locals
                      var
                      (lambda (s) (set-add s (cdr external+local)))
                      (seteq))]))
    
    (for ([imports+locals (in-list importss+localss)]
          [i-lnk (in-list (linklet-info-imports li))])
      (for ([import+local (in-list imports+locals)])
        (record! i-lnk import+local instance-knot-ties)))
    
    (for ([export+local (in-list exports+locals)])
      (record! lnk export+local #hasheq()))
                   
    (define all-import-export-locals
      (list->set
       (apply append
              (map cdr exports+locals)
              (for/list ([imports+locals (in-list importss+localss)])
                (map cdr imports+locals)))))
    (set! otherwise-used-symbols
          (set-union otherwise-used-symbols
                     (set-subtract all-mentioned-symbols
                                   all-import-export-locals))))
  
  ;; For each variable name, use the obvious symbol if it won't
  ;; collide, otherwise pick a symbol that's not mentioned anywhere.
  ;; (If a variable was given an alternative name for all imports or
  ;; exports, probably using the obvious symbol would cause a
  ;; collision.)
  (for/hash ([var (in-list (reverse all-variables))])
    (define current-syms (hash-ref variable-locals var))
    (define sym
      (cond
       [(and (= 1 (set-count current-syms))
             (not (set-member? otherwise-used-symbols (set-first current-syms))))
        (set-first current-syms)]
       [(and (set-member? current-syms (variable-name var))
             (not (set-member? otherwise-used-symbols (variable-name var))))
        (variable-name var)]
       [else (distinct-symbol (variable-name var) otherwise-used-symbols)]))
    (set! otherwise-used-symbols (set-add otherwise-used-symbols sym))
    (values var sym)))

(define (body-with-substituted-variable-names lnk li variable-names
                                              #:linklets linklets
                                              #:instance-knot-ties instance-knot-ties)
  (define linklet (linklet-info-linklet li))
  (define importss+localss
    (skip-abi-imports (bootstrap:s-expr-linklet-importss+localss linklet)))
  (define exports+locals
    (bootstrap:s-expr-linklet-exports+locals linklet))

  (define substs (make-hasheq))
  
  (define (add-subst! lnk external+local knot-ties)
    (cond
     [(find-knot-tying-alternate knot-ties lnk (car external+local) linklets)
      => (lambda (alt-lnk)
           (if (eq? alt-lnk 'ignore)
               ;; Map to original name:
               (hash-set! substs (cdr external+local) (car external+local))
               ;; Map to alt-link:
               (add-subst! alt-lnk external+local knot-ties)))]
     [else
      (hash-set! substs
                 (cdr external+local)
                 (hash-ref variable-names (variable lnk (car external+local))))]))
  
  (for ([imports+locals (in-list importss+localss)]
        [i-lnk (in-list (linklet-info-imports li))])
    (for ([import+local (in-list imports+locals)])
      (add-subst! i-lnk import+local instance-knot-ties)))
  
  (for ([export+local (in-list exports+locals)])
    (add-subst! lnk export+local #hasheq()))
  
  (define orig-s (bootstrap:s-expr-linklet-body (linklet-info-linklet li)))
  
  (substitute-symbols orig-s substs))


(define (find-knot-tying-alternate knot-ties lnk external linklets)
  (cond
   [(hash-ref knot-ties (link-name lnk) #f)
    => (lambda (alt-paths)
         (or (for/or ([alt-path (in-list alt-paths)])
               (cond
                [(eq? alt-path 'ignore)
                 'ignore]
                [else
                 (define alt-lnk (link alt-path 0))
                 (define li (hash-ref linklets alt-lnk
                                      (lambda ()
                                        (error 'flatten
                                               (string-append "module for knot tying is not part"
                                                              " of the flattened module's implementation\n"
                                                              "  module: ~a\n"
                                                              "  attempted redirect for: ~a")
                                               (link-name alt-lnk)
                                               (link-name lnk)))))
                 (define exports+locals (bootstrap:s-expr-linklet-exports+locals (linklet-info-linklet li)))
                 (for/or ([export+local (in-list exports+locals)])
                   (and (eq? external (car export+local))
                        alt-lnk))]))
             (error 'flatten "could not find alternative export: ~s from ~s"
                    external
                    lnk)))]
   [else #f]))
