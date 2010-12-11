#lang racket/base

#|
Show imports (symbols that come from requires) and exports (symbols that are provided)

1. How can I avoid showing imported symbols from the lang line? It would be nice to
ignore all the symbols from racket/base if a file starts with
#lang racket/base

|#

(require racket/match
         unstable/generics
         racket/pretty
         syntax/parse
         (for-syntax racket/struct-info
                     racket/base
                     syntax/parse
                     racket/match))

(provide get-exports
         (struct-out provided))

(define module-name
  (compose resolved-module-path-name module-path-index-resolve))

(define-syntax (import-struct stx)
 (syntax-parse stx
   [(_ ([struct-name:identifier instance:identifier] more ...) body ...)
    (define (get-fields struct instance)
      ;; (printf "Import struct for ~a\n" #'struct-name)
      (let ([info (syntax-local-value struct (lambda () #f))])
        (match (extract-struct-info info)
               [(list name init-field-count auto-field-count accessor-proc
                      mutator-proc immutable-k-list)
                (begin
                  ;; messing around with strings is bad, whats a better solution?
                  (define (make-local-field field-stx)
                    (let* ([field (substring (symbol->string (syntax->datum field-stx))
                                             (- (string-length (string-append (symbol->string (syntax->datum name)) "-"))
                                                (string-length "struct:")))]
                           [final (string->symbol (string-append (symbol->string
                                                                   (syntax->datum instance))
                                                                 "."
                                                                 field))])
                      (datum->syntax instance final instance instance)))
                  #;
                  (apply printf "name: ~a init-field-count: ~a auto-field-count: ~a accessor-proc: ~a mutator-proc: ~a immutable-k-list: ~a\n"
                         (list name init-field-count auto-field-count (map syntax->datum accessor-proc)
                               mutator-proc immutable-k-list))
                  (with-syntax ([(field ...)
                                 (map make-local-field accessor-proc)]
                                [(setter! ...) mutator-proc]
                                [instance instance]
                                [(accessor ...) accessor-proc])
                    #|
                    (printf "bind: ~a\n" (map syntax->datum (syntax->list #'(field ...))))
                    (printf "setter: ~a\n" (map syntax->datum (syntax->list #'(setter! ...))))
                    |#
                    (begin
                     #;syntax-local-introduce
                      #;
                      #'(let ([my-accessor])
                          let-syntax ([field (make-rename-transformer my-accessor)] ...)
                          body)

                      #;
                      #'(let ([field (make-rename-transformer #'field
                                                              (accessor instance))]
                              ...)
                          body)

                      #'([field (make-set!-transformer
                                  (lambda (stx)
                                    (syntax-case stx (set!)
                                      [(set! id v) (if #'setter!
                                                     #'(setter! instance v)
                                                     #'(error 'with-struct "field ~a is not mutable so no set! is available" 'field))]
                                      [id #'(accessor instance)])))]
                         ...)

                      #;
                      #'(let-syntax ([field (make-set!-transformer
                                              (lambda (stx)
                                                (syntax-case stx (set!)
                                                  [(set! id v) (if #'setter!
                                                                 #'(setter! instance v)
                                                                 #'(error 'with-struct "field ~a is not mutable so no set! is available" 'field))]
                                                  [id #'(accessor instance)])))]
                                     ...)
                          body ...)

                      #;
                      #'(let-syntax ([field (lambda (stx)
                                              #'(accessor instance))]
                                     ...)
                          body1 body ...))))])))
    (with-syntax ([(field ...) (get-fields #'struct-name #'instance)])
      ;; (printf "Final let syntax is ~a\n" (syntax->datum #'(let-syntax (field ...) body ...)))
      #'(let-syntax (field ...)
          (import-struct (more ...) body ...)))]
   [(_ () body ...)
    #'(begin body ...)]))

(generics module-symbol
          (print module-symbol)
          (get-symbol module-symbol))

(provide print)

(define-syntax-rule (define-module-symbol name (fields ...) rest ...)
                    (define-struct name (fields ...)
                                   #:property prop:module-symbol
                                   rest ...))

(define-module-symbol symbol:normal (name)
                      (define-methods module-symbol
                                      (define (get-symbol self) (symbol:normal-name self))
                                      (define (print self)
                                        (import-struct ([symbol:normal self])
                                                       (format "~a" self.name)))))

(define-module-symbol symbol:normal/contract (name contract)
               (define-methods module-symbol
                               (define (get-symbol self) (symbol:normal-name self))
                               (define (print self)
                                 (import-struct ([symbol:normal/contract self])
                                                (format "~a contract ~a" self.name self.contract)))))

(define-module-symbol symbol:renamed (provided defined)
               (define-methods module-symbol
                               (define (get-symbol self) (symbol:renamed-provided self))
                               (define (print self)
                                 (import-struct ([symbol:renamed self])
                                                (format "~a as ~a" self.defined self.provided)))))

(define-module-symbol symbol:module-exported (where)
               (define-methods module-symbol
                               (define/generic symbol-print print)
                               (define (get-symbol self)
                                 (raise 'get-symbol "Not defined"))
                               (define (print self)
                                 (format "from ~a"
                                         (module-name
                                             (symbol:module-exported-where self))
                                         ))))

(define-module-symbol symbol:module-exported-from (original where)
               (define-methods module-symbol
                               (define/generic symbol-print print)
                               (define (get-symbol self)
                                 (raise 'get-symbol "Not defined"))
                               (define (print self)
                                 (import-struct ([symbol:module-exported-from self])
                                                (format "from ~a ~a"
                                                        (module-name self.where)
                                                        (symbol-print self.original))))))

(define-module-symbol symbol:module-exported-as
               (where phase-shift imported-name import-shift)
               (define-methods module-symbol
                               (define/generic symbol-print print)
                               (define (get-symbol self)
                                 (symbol:module-exported-as-imported-name self))
                               (define (print self)
                                 (import-struct ([symbol:module-exported-as self])
                                 (format "from ~a as ~a"
                                         (module-name self.where)
                                         self.imported-name)))))

(define-module-symbol symbol:multiple-modules (symbol modules)
               (define-methods module-symbol
                               (define/generic symbol-print print)
                               (define/generic symbol-get-symbol get-symbol)
                               (define (get-symbol self)
                                 (symbol-get-symbol
                                   (symbol:multiple-modules-symbol self)))
                               (define (print self)
                                 (import-struct ([symbol:multiple-modules self])
                                                (format "~a ~a"
                                                        (symbol-print self.symbol)
                                                        (let ([modules self.modules])
                                                          (if (null? modules)
                                                            ""
                                                            (for/fold ([start (symbol-print (car modules))])
                                                                      ([next (cdr modules)])
                                                                      (format "~a and ~a" start (symbol-print next))))))))))

(struct provided (phase variables syntaxes))

(define get-namespace
  (let ([namespaces (make-hash)])
    (lambda (file)
      (hash-ref namespaces file (lambda ()
                                  (let ([new (make-base-namespace)])
                                    (hash-set! namespaces file new)
                                    new))))))

(define (read-file file)
  (parameterize ([read-accept-reader #t])
    (with-input-from-file file (lambda () (read)))))

;; extract the symbol from the module and call `contract-name' on its contract
(define (get-contract symbol file)
  (parameterize ([current-namespace
                   (get-namespace file)
                   #;
                   (make-base-namespace)])
    ;; FIXME! it would be nice if we could pull multiple symbols out
    ;; in the same `dynamic-require' call
    (define has-contract? (dynamic-require 'racket/contract 'has-contract?))
    (define value-contract (dynamic-require 'racket/contract 'value-contract))
    (define contract-name (dynamic-require 'racket/contract 'contract-name))
    ;; syntax expansion might fail, just ignore it
    (with-handlers ([exn:fail:syntax? (lambda (e) #f)])
      (let ([result (dynamic-require file symbol (lambda () #f))])
        #;
        (printf "Result is ~a\n" result)
        #;
        (printf "v is ~a\n" v)
        #;
        (printf "v has contract? ~a\n" (has-contract? v))
        (if (has-contract? result)
          (contract-name (value-contract result))
          #f)))))

(define (make-symbol something file get-contract?)
  (define (populate-symbol symbol)
    (if (not get-contract?)
      (symbol:normal symbol)
      (let ([contract (get-contract symbol file)])
        (if contract
          (symbol:normal/contract symbol contract)
          (symbol:normal symbol)))))
  (define (extract-module path)
    (match path
           [(and (? module-path-index?) module)
            (symbol:module-exported module)]
           [(list path phase-shift imported-name imported-phase)
            (symbol:module-exported-as path
                                       phase-shift
                                       imported-name
                                       imported-phase)]))
  (match something
         [(list exported (list paths ...))
          (symbol:multiple-modules (populate-symbol exported)
                                   (map extract-module paths))]))

(define (extract-base-module module-code)
  (syntax-parse module-code
    [(module name base . rest) (syntax->datum #'base)]))

(define (module=? module1 module2)
  (define (resolve module)
    (cond
      [(symbol? module) (module-path-index-resolve (module-path-index-join module #f))]
      [(resolved-module-path? module) module]
      ;; [(module-path-index? module) (
      [(module-path-index? module)
       (module-path-index-resolve module)
       #;
       (let-values ([(path base) (module-path-index-split module)])
         (printf "Split module path ~a base ~a\n" path base)
         ((current-module-name-resolver) path))]
      [else (error 'module=? "Dont understand ~a" module)]))
  (define (raw-exports module)
    (parameterize ([current-namespace
                     (get-namespace (resolved-module-path-name module))
                     #;
                     (make-base-namespace)])
      (dynamic-require (resolved-module-path-name module) #f)
      (call-with-values (lambda () (module->exports (resolved-module-path-name module)))
                        (lambda v v))))
  #;
  (printf "~a resolved ~a. ~a resolved ~a\n" module1 (resolve module1)
          module2 (resolve module2))
  (eq? (resolve module1) (resolve module2))
  #;
  (equal? (raw-exports (resolve module1))
          (raw-exports (resolve module2)))
  #;
  (equal? (resolve module1) (resolve module2)))

(define (get-imports file all?)
  (let ([imports (parameterize ([current-namespace
                                  (get-namespace file)
                                  #;
                                  (make-base-namespace)])
                               (dynamic-require file #f)
                               (module->imports file))])
    (define (combine-provides provides)
      ;; provides is guaranteed to have at least one thing or we wouldn't get here
      (for/fold ([all (car provides)])
                ([provide (cdr provides)])
        (provided (provided-phase all)
                  (append (provided-variables all)
                          (provided-variables provide))
                  (append (provided-syntaxes all)
                          (provided-syntaxes provide)))))
    (define phase-imports (make-hash))
    (define base-module (extract-base-module (read-file file)))
    (define (fixup-paths path exports)
      (for/list ([export exports])
                (match export
                  [(symbol:multiple-modules symbol modules)
                   (symbol:multiple-modules symbol
                                            (if (null? modules)
                                              (list (symbol:module-exported path))
                                              (map (lambda (module)
                                                     (symbol:module-exported-from
                                                       module path))
                                                   modules)))])))
    (define (add-provide phase provide)
      (hash-set! phase-imports
                 phase
                 (cons provide (hash-ref phase-imports phase (lambda () (list))))))
    ;; (printf "Base module is ~a ~a\n" base-module (make-resolved-module-path base-module))
    (for ([import imports])
         (match import
           [(list phase-shift paths ...)
            ;; (printf "Import at phase shift ~a\n" phase-shift)
            (for ([path paths])
                 ;; (printf " Module ~a\n" (module-name path))
                 (define module-path (let-values ([(module-path rest) (module-path-index-split path)])
                                               ;; (printf "Module path is ~a. Rest is ~a\n" module-path rest)
                                               module-path))
                 ; (define resolved-module-path (module-path-index-resolve path))
                 ;; (define resolved-module-path (make-resolved-module-path module-path))
                 ;; (printf "base ~a = resolved ~a is ~a\n" base-module path (module=? base-module path))
                 (when (or all? (not (module=? path base-module)))
                   (let ([exports (get-exports module-path #f)])
                     (for ([export exports])
                          (match export
                                 [(provided phase variables syntaxes)
                                  (add-provide (+ phase phase-shift)
                                               (provided (+ phase phase-shift)
                                                         (fixup-paths path variables)
                                                         (fixup-paths path syntaxes)))])))))]))
    (hash-map phase-imports (lambda (phase provides)
                              (combine-provides provides)))))

(define (get-exports file get-contracts?)
  (define (sort-symbols symbols)
    (sort symbols (lambda (a b)
                    (define (get-symbol what)
                      (match what
                        [(list name rest ...) (symbol->string name)]))
                    (string<? (get-symbol a)
                              (get-symbol b)))))
  (define (make-symbol* export)
    (make-symbol export file get-contracts?))
  (let-values ([(exported-variables
                  exported-syntaxes)
                (parameterize ([current-namespace
                                 (get-namespace file)
                                 #;
                                 (make-base-namespace)])
                              (dynamic-require file #f)
                              (module->exports file))])
    #;
    (pretty-print (syntax->datum 
                              (parameterize ([current-namespace (make-base-namespace)])
                                            (expand (read-file file)))))


    ; (printf "Expanded is ~a\n" expanded)
    ; (printf "Variables ~a\n" (syntax-property expanded 'module-variable-provides))
    ; (printf "Syntaxes ~a\n" (syntax-property expanded 'module-syntax-provides))
    (define exports (make-hash))
    (for ([export exported-variables])
     (match export
            [(list (and (? number?) phase) symbols ...)
             (hash-set! exports phase (provided phase
                                                (map make-symbol* (sort-symbols symbols))
                                                '()))]))
    (for ([export exported-syntaxes])
     (match export
            [(list (and (? number?) phase) symbols ...)
             (hash-set! exports phase
                        (let ([existing (hash-ref exports phase (lambda () (provided phase '() '())))])
                          (provided phase
                                    (provided-variables existing)
                                    (map make-symbol* (sort-symbols symbols)))))]))
    (hash-map exports (lambda (a b) b))))


(define (phase-name phase)
  (case phase
    [(0) " (runtime)"]
    [(1) " (syntax)"]
    [(-1) " (template)"]
    [else ""]))

#|
(define (find-file provides category search)
  (struct levenshtein (name distance phase))
  ;; find the levenshtein distance between the searched-for term and the name
  (define (fuzzy-search search name)
    (local-require (prefix-in neil: (planet neil/levenshtein:1:3/levenshtein)))
    ;; (printf "Name is ~a\n" name)
    (define real-name (symbol->string (get-symbol name)))
    (let ([search-in real-name])
      (define distance (neil:string-levenshtein search search-in))
      (levenshtein real-name distance 0)))

  (define (compare-levenshtein object1 object2)
    (< (levenshtein-distance object1)
       (levenshtein-distance object2)))
  (define (do-search export)
    (match export
        [(provided phase variables syntaxes)
         ;; replace the phase from the fuzzy-search with the phase from the export
         (define (update-phase stuff)
           (for/list ([object stuff])
             (match object
               [(levenshtein name distance dont-care)
                (levenshtein name distance phase)])))
         (let ([found-variables (map (lambda (variable)
                                       (fuzzy-search search variable))
                                     variables)]
               [found-syntaxes (map (lambda (syntax)
                                      (fuzzy-search search syntax))
                                    syntaxes)])
           (append (update-phase found-variables)
                   (update-phase found-syntaxes)))]))
  (let* ([exports provides]
         [found (apply append (map do-search exports))]
         [sorted (sort found compare-levenshtein)])
    (if (null? sorted)
      (printf "No ~as available\n" category)
      (for ([i (in-range 1 6)]
            [found sorted])
        (match found
          [(levenshtein name distance phase)
           (printf "~a. Found ~a `~a' at phase ~a\n" i category name phase)])))))

(define (find-file-export file search)
  (find-file (get-exports file #f) "export" search))

(define (find-file-import file search)
  (find-file (get-imports file #t) "import" search))
|#

(define (find-defines file)
  (define defines
    (parameterize ([current-load-relative-directory (let-values ([(care a b)
                                                                  (split-path (path->complete-path (resolve-path (string->path file))))])
                                                      care)])
      (let ([code (parameterize ([current-namespace (make-base-namespace)])
                    (expand (read-file file)))])
        (syntax-case code (module)
          [(module name base (module-begin stuff ...))
           (apply append
                  (for/list ([top-level (syntax->list #'(stuff ...))])
                    (syntax-case top-level (define-values define-syntaxes)
                      [(define-values (name ...) . body)
                       (for/list ([name (syntax->list #'(name ...))])
                         (symbol->string (syntax->datum name))
                         #;
                         (printf "~a\n" (syntax->datum name)))]
                      [(define-syntaxes (name ...) . body)
                       (for/list ([name (syntax->list #'(name ...))])
                         (symbol->string (syntax->datum name))
                         #;
                         (printf "~a\n" (syntax->datum name)))]
                      [else (list)])))]))))
  (for ([item (sort defines string<?)])
    (printf "~a\n" item)))

(define (check-file/raw file phase show-imports? show-exports?)
  (define (print-all stuff)
    (for ([symbol stuff])
         (printf "~a\n" (print symbol))))
  (define (show-all provides)
    (for ([provide provides])
      (when (or (eq? phase 'all)
                (equal? phase (provided-phase provide)))
        (print-all (provided-variables provide))
        (print-all (provided-syntaxes provide)))))

  (define (show-imports)
    (show-all (get-imports file #f)))
  (define (show-exports)
    (show-all (get-exports file #f)))
  (when show-imports?
    (show-imports))
  (when show-exports?
    (show-exports)))

(define (check-file file phase show-imports? show-exports?)
  (define (print-all prefix stuff)
    (for ([symbol stuff])
         (printf "~a~a\n" prefix (print symbol))))
  (define (show-all what provides)
    (define (space n)
      (make-string n #\space))
    (printf "~a\n" what)
    (for ([provide provides])
         (when (or (eq? phase 'all)
                   (equal? phase (provided-phase provide)))
           (printf "  Phase ~a~a\n" (provided-phase provide)
                   (phase-name (provided-phase provide)))
           (printf "    Variables\n")
           (print-all (space 6) (provided-variables provide))
           (printf "    Syntaxes\n")
           (print-all (space 6) (provided-syntaxes provide)))))
  (define (show-imports)
    (show-all "Imports" (get-imports file #f)))
  (define (show-exports)
    (show-all "Exports" (get-exports file #t)))
  (when show-imports?
    (show-imports)
    (printf "\n"))
  (when show-exports?
    (show-exports)))

#|
(define mode (make-parameter 'show))
(define only-phase (make-parameter 'all))
(define show-imports (make-parameter #t))
(define show-exports (make-parameter #t))
(define find-export (make-parameter #f))
(define find-import (make-parameter #f))

(define (do-parse-command-line)
  (local-require racket/cmdline)
  (command-line
    #:program "checker"
    #:once-each
    [("--raw") "Just print a list of identifiers without any formatting"
               (mode 'raw)]
    [("--phase") phase
               "Only show identifiers at this phase"
               (only-phase (string->number phase))]
    [("--exports") "Only show exports"
                   (show-imports #f)]
    [("--imports") "Only show imports"
                   (show-exports #f)]
    [("--defines") "Only show defined identifiers"
                   (mode 'defines)]
    [("--find-export") export "Do a fuzzy match for an export"
                       (begin
                         (mode 'find-export)
                         (find-export export))]
    [("--find-import") import "Do a fuzzy match for an import"
                       (begin
                         (mode 'find-import)
                         (find-import import))]
    #:args files
    files))

(for ([file (do-parse-command-line)])
  (printf "Checking file ~a\n" file)
  (case (mode)
    [(show) (check-file (string->path file) (only-phase) (show-imports) (show-exports))]
    [(raw) (check-file/raw (string->path file) (only-phase) (show-imports) (show-exports))]
    [(defines) (find-defines file)]
    [(find-export) (find-file-export file (find-export))]
    [(find-import) (find-file-import file (find-import))]))
|#
