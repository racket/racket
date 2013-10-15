#lang racket/base

(require racket/list
         racket/match
         racket/contract
         compiler/zo-parse
         "util.rkt"
         "mpi.rkt"
         racket/set)

(define current-excluded-modules (make-parameter (set)))

(define ZOS (make-parameter #f))
(define MODULE-IDX-MAP (make-parameter #f))
(define PHASE*MODULE-CACHE (make-parameter #f))

(define (nodep-file file-to-batch)
  (define idx-map (make-hash))
  (parameterize ([ZOS (make-hash)]
                 [MODULE-IDX-MAP idx-map]
                 [PHASE*MODULE-CACHE (make-hash)])
    (define (get-modvar-rewrite modidx)
      (define pth (mpi->path* modidx))
      (hash-ref idx-map pth
                (lambda ()
                  (error 'get-modvar-rewrite "Cannot locate modvar rewrite for ~S" pth))))
    (match (get-nodep-module-code/path file-to-batch 0)
      [(struct @phase (_ (struct module-code (modvar-rewrite lang-info ctop))))
       (values ctop lang-info (modvar-rewrite-modidx modvar-rewrite) get-modvar-rewrite)])))

(define (path->comp-top pth submod)
  (hash-ref! (ZOS) (cons pth submod)
             (λ ()
                (define zo (call-with-input-file pth zo-parse))
                (if submod
                    (extract-submod zo submod)
                    zo))))

(define (extract-submod zo submod)
  (define m (compilation-top-code zo))
  (struct-copy compilation-top
               zo
               [code (let loop ([m m])
                       (if (and (pair? (mod-name m))
                                (equal? submod (cdr (mod-name m))))
                           m
                           (or (ormap loop (mod-pre-submodules m))
                               (ormap loop (mod-post-submodules m)))))]))

(define (excluded? pth)
  (and (path? pth)
       (set-member? (current-excluded-modules) (path->string pth))))

(define (get-nodep-module-code/index mpi phase)
  (define pth (mpi->path! mpi))
  (cond
    [(symbol? pth)
     (hash-set! (MODULE-IDX-MAP) pth pth)
     pth]
    [(excluded? pth)
     (hash-set! (MODULE-IDX-MAP) pth mpi)
     mpi]
    [else
     (get-nodep-module-code/path pth phase)]))

(define-struct @phase (phase code))
(define-struct modvar-rewrite (modidx provide->toplevel))
(define-struct module-code (modvar-rewrite lang-info ctop))
(define @phase-ctop (compose module-code-ctop @phase-code))

(define (get-nodep-module-code/path pth phase)
  (define MODULE-CACHE
    (hash-ref! (PHASE*MODULE-CACHE) phase make-hash))
  (if (hash-ref MODULE-CACHE pth #f)
      #f
      (hash-ref! 
       MODULE-CACHE pth 
       (lambda ()
         (define-values (base file dir?) (split-path (if (path-string? pth)
                                                         pth
                                                         (cadr pth))))
         (define base-directory
           (if (path? base) 
               (path->complete-path base (current-directory))
               (current-directory)))
         (define-values (modvar-rewrite lang-info ctop)
           (begin
             (log-debug (format "Load ~S @ ~S" pth phase))
             (nodep/dir
              (parameterize ([current-load-relative-directory base-directory])
                (path->comp-top
                 (build-compiled-path 
                  base 
                  (path-add-suffix file #".zo"))
                 (and (pair? pth) (cddr pth))))
              pth
              phase)))
         (when (and phase (zero? phase))
           (hash-set! (MODULE-IDX-MAP) pth modvar-rewrite))
         (make-@phase 
          phase
          (make-module-code modvar-rewrite lang-info ctop))))))

(define (nodep/dir top pth phase)
  (parameterize ([current-module-path pth])
    (nodep top phase)))

(define (nodep top phase)
  (match top
    [(struct compilation-top (max-let-depth prefix form))
     (define-values (modvar-rewrite lang-info new-form) (nodep-form form phase))
     (values modvar-rewrite lang-info (make-compilation-top max-let-depth prefix new-form))]
    [else (error 'nodep "unrecognized: ~e" top)]))

(define (nodep-form form phase)
  (if (mod? form)
      (let-values ([(modvar-rewrite lang-info mods)
                    (nodep-module form phase)])
        (values modvar-rewrite lang-info (make-splice mods)))
      (error 'nodep-form "Doesn't support non mod forms")))

; XXX interning is hack to fix test/add04.ss and provide/contract renaming
(define (intern s) (string->symbol (symbol->string s)))
(define (construct-provide->toplevel prefix provides)
  (define provide-ht (make-hasheq))
  (for ([tl (prefix-toplevels prefix)]
        [i (in-naturals)])
      (when (symbol? tl)
        (hash-set! provide-ht (intern tl) i)))  
  (lambda (sym pos)
    (define isym (intern sym))
    (log-debug (format "Looking up ~S@~a [~S] in ~S" sym pos isym prefix))
    (define res
      (hash-ref provide-ht isym
              (lambda ()
                (error 'provide->toplevel "Cannot find ~S in ~S" sym prefix))))
    (log-debug (format "Looked up ~S@~a and got ~v" sym pos res))
    res))

(define (nodep-module mod-form phase)
  (match mod-form
    [(struct mod (name srcname self-modidx prefix provides requires body syntax-bodies
                       unexported max-let-depth dummy lang-info internal-context
                       flags pre-submodules post-submodules))
     (define new-prefix prefix)
     ;; Cache all the mpi paths
     (for-each (match-lambda
                 [(and mv (struct module-variable (modidx sym pos phase constantness)))
                  (mpi->path! modidx)]
                 [tl
                  (void)])
               (prefix-toplevels new-prefix))
     (define mvs (filter module-variable? (prefix-toplevels new-prefix)))
     (log-debug (format "[~S] module-variables: ~S - ~S" name (length mvs) mvs))
     (values (make-modvar-rewrite self-modidx (construct-provide->toplevel new-prefix provides))
             lang-info
             (append (requires->modlist requires phase)
                     (if (and phase (zero? phase))
                         (begin (log-debug (format "[~S] lang-info : ~S" name lang-info)) ; XXX Seems to always be #f now
                                (list (make-mod name srcname self-modidx new-prefix provides requires body empty
                                                unexported max-let-depth dummy lang-info internal-context
                                                empty empty empty)))
                         (begin (log-debug (format "[~S] Dropping module @ ~S" name phase))
                                empty))))]              
    [else (error 'nodep-module "huh?: ~e" mod-form)]))

(define (+* l r)
  (if (and l r) (+ l r) #f))

(define (requires->modlist requires current-phase)
  (apply append
         (map
          (match-lambda
            [(list-rest req-phase mpis)
             (define phase (+* current-phase req-phase))
             (apply append
                    (map (compose extract-modules (lambda (mpi) (get-nodep-module-code/index mpi phase))) mpis))])
          requires)))

(define (all-but-last l)
  (reverse (rest (reverse l))))

(define REQUIRED (make-hasheq))
(define (extract-modules ct)
  (cond
    [(compilation-top? ct)
     (match (compilation-top-code ct)
       [(and m (? mod?))
        (list m)]
       [(struct splice (mods))
        mods])]
    [(symbol? ct)
     (if (hash-has-key? REQUIRED ct)
         empty
         (begin
           (hash-set! REQUIRED ct #t)
           (list (make-req (make-stx (make-wrapped ct empty 'clean)) (make-toplevel 0 0 #f #f)))))]
    [(module-path-index? ct)
     (if (hash-has-key? REQUIRED ct)
         empty
         (begin
           (hash-set! REQUIRED ct #t)
           (list (make-req (make-stx (make-wrapped ct empty 'clean)) (make-toplevel 0 0 #f #f)))))]
    [(not ct)
     empty]
    [(@phase? ct)
     (extract-modules (@phase-ctop ct))]
    [else
     (error 'extract-modules "Unknown extraction: ~S" ct)]))

(define get-modvar-rewrite/c 
  (module-path-index? . -> . (or/c symbol? modvar-rewrite? module-path-index?)))
(provide/contract
 [struct modvar-rewrite 
         ([modidx module-path-index?]
          [provide->toplevel (symbol? exact-nonnegative-integer? . -> . exact-nonnegative-integer?)])]
 [get-modvar-rewrite/c contract?]
 [current-excluded-modules (parameter/c generic-set?)]
 [nodep-file (-> path-string?
                 (values compilation-top? lang-info/c module-path-index? get-modvar-rewrite/c))])
