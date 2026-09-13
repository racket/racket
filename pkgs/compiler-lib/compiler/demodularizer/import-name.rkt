#lang racket/base
(require compiler/zo-structs
         racket/match
         "one-mod.rkt"
         "run.rkt"
         "name.rkt"
         "import.rkt"
         "remap.rkt"
         "linklet.rkt"
         "log.rkt"
         "at-phase-level.rkt")

(provide add-import-maps)

(define (add-import-maps phase-runs find-or-add-name! names
                         one-mods excluded-module-mpis)
  (for/hasheqv ([(root-phase runs) (in-hash phase-runs)])
    (values
     root-phase
     (for/list ([r (in-list runs)])
       (define linkl (run-linkl r))
       (define import-map
         (for/hasheq ([ext-names (in-list (if linkl (linklet*-importss linkl) null))]
                      [int-names (in-list (if linkl (linklet*-internal-importss linkl) null))]
                      [use (in-list (run-uses r))]
                      #:when (not (memq (car use) '(#%syntax-literals #%transformer-register)))
                      [ext-name (in-list ext-names)]
                      [int-name (in-list int-names)])
           (define import-path/submod (car use))
           (define phase-level (cdr use))
           (define one-m (and (not (symbol? import-path/submod))
                              (hash-ref one-mods import-path/submod)))
           (define new-name/import
             (cond
               [(or (not one-m) ; => symbol-named primitive module
                    (one-mod-excluded? one-m))
                ;; We're not demodularizing this module, so it's exports
                ;; keep the same name; we need to pick a new name for all
                ;; modules using the same reference
                (define new-name (find-or-add-name! names use ext-name))
                (import new-name
                        use
                        ext-name)]
               [else
                ;; This module can be demodularized, and the export name may be
                ;; different than the original name.  Start by getting the
                ;; internal name in the original source.
                (define src-int-name (hash-ref (hash-ref (one-mod-exports one-m)
                                                         phase-level)
                                               ext-name))
                (define src-new-name (find-name names use src-int-name))
                (cond
                  [(or (hash-ref excluded-module-mpis import-path/submod #f)
                       (hash-ref excluded-module-mpis (at-phase-level import-path/submod phase-level) #f))
                   ;; Not merged here, but demodularized form will export using
                   ;; the new internal name; the import locally can use
                   ;; the internal name, which is the same as the linklet export name
                   (import src-new-name use src-new-name)]
                  [else
                   ;; Merged here, so directly use the new source name
                   src-new-name])]))
           (values int-name new-name/import)))

       (log-demodularizer-debug " Import map for ~a ~a:" (run-path/submod r) (run-phase r))
       (for* ([(name i) (in-hash import-map)])
         (if (import? i)
             (log-demodularizer-debug "    ~a -> ~a = ~a ~a ~a" name
                                      (import-name i)
                                      (import-src-ext-name i)
                                      (car (import-path/submod+phase i))
                                      (cdr (import-path/submod+phase i)))
             (log-demodularizer-debug "    ~a -> ~a" name i)))

       (struct-copy run r
                    [import-map import-map])))))
