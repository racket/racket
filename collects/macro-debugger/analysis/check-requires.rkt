#lang racket/base
(require racket/contract/base
         racket/cmdline
         racket/match
         syntax/modcollapse
         "private/get-references.rkt"
         "private/nom-use-alg.rkt"
         "private/util.rkt")
(provide/contract
 [check-requires
  (->* (module-path?)
       (#:show-keep? any/c
        #:show-bypass? any/c
        #:show-drop? any/c
        #:show-uses? any/c)
       void?)]
 [show-requires (-> module-path? list?)]
 [mpi->key (-> module-path-index? any/c)])

#|
==========

Notes

  Ignore recommendations to DROP or BYPASS modules with side
  effects. Read the section below (How it works) and also see
  util/moduledb.rkt for whitelisting side-effectful modules.

  The script is not intelligent about the language, which causes
  certain spurious recommendations to appear frequently. For example,

    DROP scheme/mzscheme at 1

  means that the module's language is mzscheme, which automatically
  inserts (require-for-syntax scheme/mzscheme). It's infeasible to
  remove it except by rewriting the module in scheme/base or
  racket/base.

==========

How it works

Determining whether a require is actually useless is impossible: a
module may be required for compile-time side effect only, and there's
no way to monitor that.

Here are some approximations that are feasible to calculate:

NOM-USES = A require R is "used" by a module M if, during the
compilation of M, a reference is resolved to a binding exported by R.

DEF-USES = A require R is "used" by a module M if, during the
compilation of M, a reference is resolved to a binding defined in R.

The limitations:
 - misses side-effects
 - misses identifiers recognized via 'free-identifier=?'
   (But those should be recorded as 'disappeared-use anyway.)

==========

TODO

Handle for-label.

Let user provide database of modules that should never be dropped, eg
because they have side effects.
  - wouldn't it be awesome if this db could be a datalog program?
  - start simpler, though

Ambitious mode could analyze module and recommend ways to split module
into independent submodules.
|#

;; ========================================

#|
A recommendation is one of
  (list 'keep   module-path-index phase Refs)
  (list 'bypass module-path-index phase RefineTable)
  (list 'drop   module-path-index phase)
|#

;; analyze-requires : module-path -> (listof recommendation)
(define (analyze-requires mod-path)
  (let-values ([(compiled deriv) (get-module-code/trace mod-path)])
    (nom-use-alg (deriv->refs deriv) compiled)))

;; ========================================

#|
A displayed-recommendation is one of
  (list 'keep   module-path phase)
  (list 'bypass module-path phase (listof (list module-path phase)))
  (list 'drop   module-path phase)

A displayed-recommendation is similar to a recommendation, but
converts the module-path-indexes to module paths, omits the use-lists,
and simplifies the replacements lists.
|#

;; show-requires: module-path -> (listof displayed-recommendation)
(define (show-requires mod-path)
  (for/list ([entry (in-list (analyze-requires mod-path))])
    (match entry
      [(list 'keep mpi phase uses)
       (list 'keep (mpi->key mpi) phase)]
      [(list 'bypass mpi phase bypass)
       (list 'bypass (mpi->key mpi) phase
             (let ([bypass (flatten-bypass bypass)])
               (for/list ([(modpath+reqphase inner) (in-hash bypass)])
                 (list (car modpath+reqphase)
                       (cdr modpath+reqphase)
                       (any-renames? (imps->use-table inner))))))]
      [(list 'drop mpi phase)
       (list 'drop (mpi->key mpi) phase)])))

;; ========================================

(define (check-requires mod
                        #:show-keep? [show-keep? #t]
                        #:show-bypass? [show-bypass? #t]
                        #:show-drop? [show-drop? #t]
                        #:show-uses? [show-uses? #f])

  (define (show-bypass mpi bypass)
    (for ([(modname+reqphase inner) (flatten-bypass bypass)])
      (let ([modname (car modname+reqphase)]
            [reqphase (cdr modname+reqphase)]
            [use-table (imps->use-table inner)])
        (printf "  TO ~s at ~s~a\n" modname reqphase
                (cond [(any-renames? use-table)
                       " WITH RENAMING"]
                      [else ""]))
        (when show-uses?
          (show-uses use-table 4)))))

  (let ([recs (analyze-requires mod)])
    (for ([rec (in-list recs)])
      (match rec
        [(list 'keep mpi phase uses)
         (when show-keep?
           (printf "KEEP ~s at ~s\n"
                   (mpi->key mpi) phase)
           (when show-uses?
             (show-uses (imps->use-table uses) 2)))]
        [(list 'bypass mpi phase bypass)
         (when show-bypass?
           (printf "BYPASS ~s at ~s\n" (mpi->key mpi) phase)
           (show-bypass mpi bypass))]
        [(list 'drop mpi phase)
         (when show-drop?
           (printf "DROP ~s at ~s\n" (mpi->key mpi) phase))]))))

;; ----

;; flatten-bypass : RefineTable -> hash[(cons module-path int) => Imps]
(define (flatten-bypass table)
  (let ([flat-table (make-hash)]) ;; hash[(cons module-path int) => Imps]
    (let loop ([table table] [mpi-ctx null])
      (for ([(mod+reqphase inner) (in-hash table)])
        (let* ([mod (car mod+reqphase)]
               [reqphase (cdr mod+reqphase)]
               [mpis (cons mod mpi-ctx)])
          (cond [(hash? inner)
                 (loop inner mpis)]
                [else
                 ;; key may already exist, eg with import diamonds; so append
                 (let* ([modpath (mpi-list->module-path mpis)]
                        [key (cons modpath reqphase)])
                   (hash-set! flat-table key
                              (append inner (hash-ref flat-table key null))))]))))
    flat-table))

(define (ref->symbol r)
  (match r
    [(ref phase id mode (list dm ds nm ns dp ips np))
     (cond [id (syntax-e id)]
           [else ns])]))

;; imps->use-table : Imps -> hash[(list phase prov-sym ref-sym) => (listof mode)]
(define (imps->use-table imps)
  (let ([table (make-hash)])
    (for ([i (in-list imps)])
      (match i
        [(imp _m _p prov-sym _prov-phase r)
         (let* ([phase (ref-phase r)]
                [ref-sym (ref->symbol r)]
                [mode (ref-mode r)]
                [key (list phase prov-sym ref-sym)]
                [modes (hash-ref table key null)])
           (unless (memq mode modes)
             (hash-set! table key (cons mode modes))))]))
    table))

;; any-renames? : use-table -> boolean
(define (any-renames? use-table)
  (for/or ([key (in-hash-keys use-table)])
    (match key
      [(list phase prov-sym ref-sym)
       (not (eq? prov-sym ref-sym))])))

;; show-uses : use-table nat -> void
(define (show-uses use-table indent)
  (let* ([unsorted
          (for/list ([(key modes) (in-hash use-table)])
            (cons key (sort modes < #:key mode->nat)))]
         [sorted
          (sort unsorted
                (lambda (A B)
                  (let ([pA (car A)]
                        [pB (car B)])
                    (or (< pA pB)
                        (and (= pA pB)
                             (let ([strA (symbol->string (cadr A))]
                                   [strB (symbol->string (cadr B))])
                               (string<? strA strB))))))
                #:key car)]
         [spacer (make-string indent #\space)])
    (for ([elem (in-list sorted)])
      (match elem
        [(cons (list phase prov-sym ref-sym) modes)
         (printf "~a~a at ~a ~a~a\n"
                 spacer prov-sym phase modes
                 (cond [(eq? ref-sym prov-sym) ""]
                       [else (format " RENAMED TO ~a" ref-sym)]))]))))

;; ========================================

(require racket/cmdline
         raco/command-name)
(provide main)

#|
Example (from racket root directory):

  racket -lm macro-debugger/analysis/check-requires \
    collects/syntax/*.rkt

  racket -lm macro-debugger/analysis/check-requires -- -bu \
    collects/syntax/*.rkt

|#

(define (main . args)

  ;; show-keep? : boolean
  ;; Show KEEP messages in output.
  (define show-keep? #f)

  ;; show-bypass? : boolean
  ;; Show BYPASS messages in output.
  (define show-bypass? #f)

  ;; show-uses? : boolean
  (define show-uses? #f)

  ;; ========

  (define (go mod)
    (printf "~s:\n" mod)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (printf "ERROR in ~s\n" mod)
                       ((error-display-handler) (exn-message exn) exn))])
      (check-requires mod
                      #:show-keep? show-keep?
                      #:show-bypass? show-bypass?
                      #:show-uses? show-uses?))
    (newline))

  ;; Command-line args are interpreted as files if the file exists,
  ;; module names otherwise.
  (command-line
   #:program (short-program+command-name)
   #:argv args
   #:once-each
   [("-k" "--show-keep")
    "Show KEEP recommendations"
    (set! show-keep? #t)]
   [("-b" "--show-bypass")
    "Show BYPASS recommendations"
    (set! show-bypass? #t)]
   [("-u" "--show-uses")
    "Show uses for each module"
    (set! show-uses? #t)]
   #:args args
   (for ([arg (in-list args)])
     (cond [(file-exists? arg)
            (go `(file ,arg))]
           [else
            (let* ([inport (open-input-string arg)]
                   [mod (read inport)])
              (unless (eof-object? (peek-char inport))
                (error "bad module name:" arg))
              (go mod))]))))

(module* main #f
  (apply main (vector->list (current-command-line-arguments))))
