#lang racket/base
(require racket/cmdline
         "private/util.rkt")
(provide get-dependencies
         show-dependencies
         main)

(define (get-dependencies-table #:include? include? ms)
  (define visited (make-hash)) ;; resolved-module-path => (listof mpi-list)
  (define (loop m ctx)
    (let* ([resolved (module-path-index-resolve m)]
           [ctx (cons m ctx)]
           [already-visited? (hash-ref visited resolved #f)])
      (when (or include? (pair? (cdr ctx)))
        ;; hack to not record initial list (unless inter-dependencies)
        (hash-set! visited resolved
                   (cons ctx (hash-ref visited resolved null))))
      (unless already-visited?
        (unless (symbol? (resolved-module-path-name resolved))
          (let ([imports (get-module-imports m)])
            (for* ([phase+mods (in-list imports)]
                   [mod (in-list (cdr phase+mods))])
            (loop mod ctx)))))))
  (for ([m (in-list ms)])
    (loop (module-path-index-join m #f) null))
  visited)

;; table->dependencies : table -> (listof (list module-path (listof module-path)))
(define (table->dependencies visited)
  (let* ([unsorted
          (for/list ([(key mpi-lists) (in-hash visited)])
            (list (mpi-list->module-path (car mpi-lists))
                  (sort (map mpi-list->module-path
                             (filter pair? (map cdr mpi-lists)))
                        module-path<?)))])
    (sort unsorted
          module-path<?
          #:key car)))

(define (module-path<? A B)
  (cond [(and (symbol? A) (symbol? B))
         (string<? (symbol->string A) (symbol->string B))]
        [(symbol? A) #t]
        [(symbol? B) #f]
        [(and (string? A) (string? B))
         (string<? A B)]
        [(string? A) #t]
        [(string? B) #f]
        [else
         ;; obviously, we don't care that much about performance in this case
         (string<? (format "~s" A) (format "~s" B))]))

;; get-dependencies : module-path ... #:excludse (listof module-path)
;;                 -> (listof (list module-path (listof module-path)))
(define (get-dependencies #:exclude [exclusions null]
                          . module-paths)
  (let* ([table
          (get-dependencies-table #:include? #f module-paths)]
         [exclude-table
          (get-dependencies-table #:include? #t exclusions)])
    (for ([key (in-hash-keys exclude-table)])
      (hash-remove! table key))
    (table->dependencies table)))

(define (show-dependencies #:exclude [exclusions null]
                           #:show-context? [context? #f]
                           . module-paths)
  (for ([dep (in-list (apply get-dependencies #:exclude exclusions module-paths))])
    (let ([mod (car dep)]
          [direct-requirers (cadr dep)])
      (printf "~s" mod)
      (when context?
        (printf " <- ~s" direct-requirers))
      (newline))))

;; ====

(define (main . argv)
  (define mode 'auto)
  (define context? #f)
  (define exclusions null)
  (command-line
   #:argv argv
   #:once-each
   [("-c" "--context") "Show who directly requires each module"
    (set! context? #t)]
   [("-f" "--file") "Interpret arguments as file-paths"
    (set! mode 'file)]
   [("-m" "--module-path") "Interpret arguments as module-paths"
    (set! mode 'module-path)]
   [("-x" "--exclude") exclude "Exclude modules reachable from <exclude>"
    (set! exclusions (cons exclude exclusions))]
   [("-b") "Same as --exclude racket/base"
    (set! exclusions (cons 'racket/base exclusions))]
   #:args module-path
   (let ()
     (define (->modpath x)
       (cond [(string? x)
              (case mode
                ((auto)
                 (if (file-exists? x)
                     `(file ,x)
                     (read (open-input-string x))))
                ((file) `(file ,x))
                ((module-path)
                 (read (open-input-string x))))]
             [else x]))
     (apply show-dependencies
            #:exclude (map ->modpath exclusions)
            #:show-context? context?
            (map ->modpath module-path)))))

#|

For example,

  racket -lm macro-debugger/analysis/show-dependencies -- -bc mzscheme

shows the additional modules used to implement mzscheme beyond those
already needed for the implementation of racket/base. And

  racket -lm macro-debugger/analysis/show-dependencies -- -bc syntax/parse/pre

shows that syntax/parse/pre has no dependencies on the contract
library. Actually, it shows that it has no *residual* dependencies;
contracts are used in the code that is lazily loaded, but using
syntax/parse/pre does not cause a module's compiled code to depend on
racket/contract/base.

|#
