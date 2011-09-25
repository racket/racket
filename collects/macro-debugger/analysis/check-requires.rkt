#lang racket/base
(require racket/contract/base
         racket/cmdline
         racket/match
         syntax/modcollapse
         unstable/struct
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

Indicate when renaming is necessary.

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
  (list 'keep   module-path-index phase list)
  (list 'bypass module-path-index phase list)
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
      [(list 'bypass mpi phase replacements)
       (list 'bypass (mpi->key mpi) phase
             (for/list ([r (in-list replacements)])
               (match r
                 [(list rmpis rphase uses)
                  (list (mpi-list->module-path rmpis) rphase)])))]
      [(list 'drop mpi phase)
       (list 'drop (mpi->key mpi) phase)])))

;; ========================================

(define (check-requires mod
                        #:show-keep? [show-keep? #t]
                        #:show-bypass? [show-bypass? #t]
                        #:show-drop? [show-drop? #t]
                        #:show-uses? [show-uses? #f])

  (define (show-bypass mpi replacements)
    (for ([replacement (in-list replacements)])
      (match replacement
        [(list repl-mod-list phase uses)
         (printf "  TO ~s at ~a\n"
                 (mpi-list->module-path (append repl-mod-list (list mpi)))
                 phase)
         (show-uses uses 4)])))

  (define (show-uses uses indent)
    (when show-uses?
      (for ([use (in-list uses)])
        (match use
          [(list sym phase modes)
           (printf "~a~a ~a ~a\n" (make-string indent #\space) sym phase modes)]))))

  (let ([recs (analyze-requires mod)])
    (for ([rec (in-list recs)])
      (match rec
        [(list 'keep mpi phase uses)
         (when show-keep?
           (printf "KEEP ~s at ~a\n"
                   (mpi->key mpi) phase)
           (show-uses uses 2))]
        [(list 'bypass mpi phase replacements)
         (when show-bypass?
           (printf "BYPASS ~s at ~a\n" (mpi->key mpi) phase)
           (show-bypass mpi replacements))]
        [(list 'drop mpi phase)
         (when show-drop?
           (printf "DROP ~s at ~a\n" (mpi->key mpi) phase))]))))

(define (mpi-list->module-path mpi-list)
  (let* ([mpi*
          (let loop ([mpi #f] [mpi-list mpi-list])
            (cond [mpi
                   (let-values ([(mod base) (module-path-index-split mpi)])
                     (cond [mod (module-path-index-join mod (loop base mpi-list))]
                           [else (loop #f mpi-list)]))]
                  [(pair? mpi-list)
                   (loop (car mpi-list) (cdr mpi-list))]
                  [else #f]))]
         [collapsed
          (let loop ([mpi mpi*])
            (cond [mpi 
                   (let-values ([(mod base) (module-path-index-split mpi)])
                     (cond [mod
                            (collapse-module-path mod (lambda () (loop base)))]
                           [else (build-path 'same)]))]
                  [else (build-path 'same)]))])
    (match collapsed
      [(list 'lib str)
       (cond [(regexp-match? #rx"\\.rkt$" str)
              (let* ([no-suffix (path->string (path-replace-suffix str ""))]
                     [no-main
                      (cond [(regexp-match #rx"^([^/]+)/main$" no-suffix)
                             => cadr]
                            [else no-suffix])])
                (string->symbol no-main))]
             [else collapsed])]
      [(? path?)
       (path->string (simplify-path collapsed #f))] ;; to get rid of "./" at beginning
      [_ collapsed])))

;; ========================================

(require racket/cmdline)
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
