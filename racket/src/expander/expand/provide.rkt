#lang racket/base
(require "../common/struct-star.rkt"
         "../syntax/syntax.rkt"
         "../syntax/taint.rkt"
         "../syntax/track.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "require+provide.rkt"
         "context.rkt"
         "protect.rkt"
         "module-path.rkt"
         "binding-for-transformer.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "free-id-set.rkt"
         "main.rkt")

(provide parse-and-expand-provides!)

(define layers '(raw phaseless id))

(define provide-form-name 'provide) ; complain as `provide` instead of `#%provide`

(define (parse-and-expand-provides! specs orig-s
                                    rp self
                                    phase ctx)
  ;; returns a list of expanded specs while registering provides in `rp`
  (define ns (expand-context-namespace ctx))
  (let loop ([specs specs]
             [at-phase phase]
             [protected? #f]
             [layer 'raw])
    (define-values (track-stxess exp-specss)
      (for/lists (track-stxes exp-specs) ([spec (in-list specs)])
        (define disarmed-spec (syntax-disarm spec))
        (define fm (and (pair? (syntax-e disarmed-spec))
                        (identifier? (car (syntax-e disarmed-spec)))
                        (syntax-e (car (syntax-e disarmed-spec)))))
        (define (check-nested want-layer)
          (unless (member want-layer (member layer layers))
            (raise-syntax-error provide-form-name (format "nested `~a' not allowed" fm) orig-s spec)))
        (case fm
          [(for-meta)
           (check-nested 'raw)
           (define-match m disarmed-spec '(for-meta phase-level spec ...))
           (define p (syntax-e (m 'phase-level)))
           (unless (phase? p)
             (raise-syntax-error provide-form-name "bad `for-meta' phase" orig-s spec))
           (define-values (track-stxes exp-specs)
             (loop (m 'spec)
                   (phase+ p at-phase)
                   protected?
                   'phaseless))
           (values null
                   (list
                    (syntax-track-origin*
                     track-stxes
                     (rebuild
                      spec
                      `(,(m 'for-meta) ,(m 'phase-level) ,@exp-specs)))))]
          [(for-syntax)
           (check-nested 'raw)
           (define-match m disarmed-spec '(for-syntax spec ...))
           (define-values (track-stxes exp-specs)
             (loop (m 'spec)
                   (phase+ 1 at-phase)
                   protected?
                   'phaseless))
           (values null
                   (list
                    (syntax-track-origin*
                     track-stxes
                     (rebuild
                      spec
                      `(,(m 'for-syntax) ,@exp-specs)))))]
          [(for-label)
           (check-nested 'raw)
           (define-match m disarmed-spec '(for-label spec ...))
           (define-values (track-stxes exp-specs)
             (loop (m 'spec)
                   #f
                   protected?
                   'phaseless))
           (values null
                   (list
                    (syntax-track-origin*
                     track-stxes
                     (rebuild
                      spec
                      `(,(m 'for-label) ,@exp-specs)))))]
          [(protect)
           (check-nested 'phaseless)
           (when protected?
             (raise-syntax-error provide-form-name "nested `protect' not allowed" orig-s spec))
           (define-match m disarmed-spec '(protect p-spec ...))
           (define-values (track-stxes exp-specs)
             (loop (m 'p-spec)
                   at-phase
                   #t
                   layer))
           (values null
                   (list
                    (syntax-track-origin*
                     track-stxes
                     (rebuild
                      spec
                      `(,(m 'protect) ,@exp-specs)))))]
          [(rename)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(rename id:from id:to))
           (parse-identifier! (m 'id:from) orig-s (syntax-e (m 'id:to)) at-phase ns rp protected?)
           (values null (list spec))]
          [(struct)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(struct id:struct (id:field ...)))
           (parse-struct! (m 'id:struct) orig-s (m 'id:field) at-phase ns rp protected?)
           (values null (list spec))]
          [(all-from)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(all-from mod-path))
           (parse-all-from (m 'mod-path) orig-s self null at-phase ns rp protected? ctx)
           (values null (list spec))]
          [(all-from-except)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(all-from-except mod-path id ...))
           (parse-all-from (m 'mod-path) orig-s self (m 'id) at-phase ns rp protected? ctx)
           (values null (list spec))]
          [(all-defined)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(all-defined))
           (parse-all-from-module self spec orig-s null #f at-phase ns rp protected?)
           (values null (list spec))]
          [(all-defined-except)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(all-defined-except id ...))
           (parse-all-from-module self spec orig-s (m 'id) #f at-phase ns rp protected?)
           (values null (list spec))]
          [(prefix-all-defined)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(prefix-all-defined id:prefix))
           (parse-all-from-module self spec orig-s null (syntax-e (m 'id:prefix)) at-phase ns rp protected?)
           (values null (list spec))]
          [(prefix-all-defined-except)
           (check-nested 'phaseless)
           (define-match m disarmed-spec '(prefix-all-defined-except id:prefix id ...))
           (parse-all-from-module self spec orig-s (m 'id) (syntax-e (m 'id:prefix)) at-phase ns rp protected?)
           (values null (list spec))]
          [(expand)
           (define-match ex-m disarmed-spec '(expand (id . datum))) ; just check syntax
           (define-match m disarmed-spec '(expand form)) ; get form to expand
           (define exp-spec (expand (m 'form) (struct*-copy expand-context ctx
                                                            [stops (free-id-set at-phase (list (core-id 'begin at-phase)))]
                                                            ;; Discarding definition-context scopes is ok,
                                                            ;; because the scopes won't be captured by
                                                            ;; any `quote-syntax`:
                                                            [def-ctx-scopes #f])))
           (unless (and (pair? (syntax-e exp-spec))
                        (identifier? (car (syntax-e exp-spec)))
                        (eq? 'begin (core-form-sym exp-spec at-phase)))
             (raise-syntax-error provide-form-name "expansion was not a `begin' sequence" orig-s spec))
           (define-match e-m exp-spec '(begin spec ...))
           (define-values (track-stxes exp-specs)
             (loop (e-m 'spec)
                   at-phase
                   protected?
                   layer))
           (values (list* spec exp-spec track-stxes)
                   exp-specs)]
          [else
           (cond
            [(identifier? spec)
             (parse-identifier! spec orig-s (syntax-e spec) at-phase ns rp protected?)
             (values null (list spec))]
            [else
             (raise-syntax-error provide-form-name "bad syntax" orig-s spec)])])))
    (values (apply append track-stxess)
            (apply append exp-specss))))

;; ----------------------------------------

(define (parse-identifier! spec orig-s sym at-phase ns rp protected?)
  (define b (resolve+shift/extra-inspector spec at-phase ns))
  (unless b
    (raise-syntax-error provide-form-name "provided identifier is not defined or required" orig-s spec))
  (define as-transformer? (binding-for-transformer? b spec at-phase ns))
  (define immed-b (resolve+shift spec at-phase #:immediate? #t))
  (add-provide! rp sym at-phase b immed-b spec orig-s
                #:as-protected? protected?
                #:as-transformer? as-transformer?))

(define (parse-struct! id:struct orig-s fields at-phase ns rp protected?)
  (define (mk fmt)
    (define sym (string->symbol (format fmt (syntax-e id:struct))))
    (datum->syntax id:struct sym id:struct))
  (define (mk2 fmt field-id)
    (define sym (string->symbol (format fmt
                                        (syntax-e id:struct)
                                        (syntax-e field-id))))
    (datum->syntax id:struct sym id:struct))
  (for ([fmt (in-list (list "~a"
                            "make-~a"
                            "struct:~a"
                            "~a?"))])
    (define id (mk fmt))
    (parse-identifier! id orig-s (syntax-e id) at-phase ns rp protected?))
  (for ([field (in-list fields)])
    (define get-id (mk2 "~a-~a" field))
    (define set-id (mk2 "set-~a-~a!" field))
    (parse-identifier! get-id orig-s (syntax-e get-id) at-phase ns rp protected?)
    (parse-identifier! set-id orig-s (syntax-e set-id) at-phase ns rp protected?)))
  
(define (parse-all-from mod-path-stx orig-s self except-ids at-phase ns rp protected? ctx)
  (define mod-path (syntax->datum mod-path-stx))
  (unless (module-path? mod-path)
    (raise-syntax-error provide-form-name "not a module path" orig-s mod-path-stx))
  (define mpi (module-path->mpi/context mod-path ctx))
  (parse-all-from-module mpi #f orig-s except-ids #f at-phase ns rp protected?))
  
(define (parse-all-from-module mpi matching-stx orig-s except-ids prefix-sym at-phase ns rp protected?)
  (define requireds (extract-module-requires rp mpi at-phase))

  (define (phase-desc) (cond
                        [(zero-phase? at-phase) ""]
                        [(label-phase? at-phase) " for-label"]
                        [else (format " for phase ~a" at-phase)]))
  (unless requireds
    (raise-syntax-error provide-form-name
                        (format "cannot provide from a module without a matching require~a"
                                (phase-desc))
                        orig-s matching-stx))

  (define (add-prefix sym)
    (if prefix-sym
        (string->symbol (string-append (symbol->string prefix-sym) (symbol->string sym)))
        sym))

  (define found (make-hasheq))
  
  ;; Register all except excluded bindings:
  (for ([i (in-list requireds)])
    (define id (required-id i))
    (define phase (required-phase i))
    (unless (or (and matching-stx
                     ;; For `(all-defined-out)`, phase and binding context must match:
                     (not (and (eqv? phase at-phase)
                               (free-identifier=? id
                                                  (datum->syntax matching-stx (syntax-e id))
                                                  phase
                                                  phase))))
                (for/or ([except-id (in-list except-ids)])
                  (and (free-identifier=? id except-id phase phase)
                       (hash-set! found except-id #t))))
      (define b (resolve+shift/extra-inspector id phase ns))
      (define immed-b (resolve+shift id phase #:immediate? #t))
      (add-provide! rp (add-prefix (syntax-e id)) phase b immed-b id orig-s 
                    #:as-protected? protected?
                    #:as-transformer? (required-as-transformer? i))))
  
  ;; Check that all exclusions matched something to exclude:
  (unless (= (hash-count found) (length except-ids))
    (for ([except-id (in-list except-ids)])
      (unless (or (hash-ref found except-id #f)
                  (for/or ([i (in-list requireds)])
                    (define id (required-id i))
                    (define phase (required-phase i))
                    (free-identifier=? id except-id phase phase)))
        (raise-syntax-error provide-form-name
                            (format (if matching-stx
                                        "excluded identifier was not defined or required in the module~a"
                                        "excluded identifier was not required from the specified module~a")
                                    (phase-desc))
                            orig-s
                            except-id)))))
