#lang racket/base
(require racket/cmdline
         racket/match
         racket/pretty
         raco/command-name
         syntax/modresolve
         "private/util.rkt")
(provide get-dependencies
         show-dependencies
         main)

;; A Table is hash[resolved-module-path => (listof mpi-list)]

(define (get-dependencies-table ms #:include? include?)
  (define visited (make-hash)) ;; Table
  (define (loop m ctx relto)
    (let* ([resolved (resolve-module-path-index* m relto)]
           [ctx (cons m ctx)]
           [already-visited? (hash-ref visited resolved #f)])
      (when (or include? (pair? (cdr ctx)))
        ;; hack to not record initial list (unless inter-dependencies)
        (hash-set! visited resolved
                   (cons ctx (hash-ref visited resolved null))))
      (unless already-visited?
        (let* ([resolved-mod (resolved-module-path-name resolved)]
               [resolved-base (if (pair? resolved-mod) (car resolved-mod) resolved-mod)])
          (unless (symbol? resolved-base)
            (let ([imports (get-module-imports resolved)])
              (for* ([phase+mods (in-list imports)]
                     [mod (in-list (cdr phase+mods))])
                (loop mod ctx resolved-base))))))))
  (for ([m (in-list ms)])
    (loop (module-path-index-join m #f) null #f))
  visited)

;; resolve-module-path-index* : mpi file-path -> resolved-module-path
(define (resolve-module-path-index* mpi relto)
  (let ([v (resolve-module-path-index mpi relto)])
    (match v
      [(? path?) (make-resolved-module-path (simplify-path v))]
      [(? symbol?) (make-resolved-module-path v)]
      [(list* 'submod (? path? base) syms)
       (make-resolved-module-path (cons (simplify-path base) syms))]
      [(list* 'submod (? symbol? base) syms)
       (error 'resolve-module-path-index*
              "failed to resolve submodule path base in: ~e" v)])))

;; table->dependencies : Table -> (listof (list module-path (listof module-path)))
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
         (symbol<? A B)]
        [(symbol? A) #t]
        [(symbol? B) #f]
        [(and (string? A) (string? B))
         (string<? A B)]
        [(string? A) #t]
        [(string? B) #f]
        [else
         ;; obviously, we don't care that much about performance in this case
         (string<? (format "~s" A) (format "~s" B))]))

;; get-dependencies : module-path ... #:exclude (listof module-path)
;;                 -> (listof (list module-path (listof module-path)))
(define (get-dependencies #:exclude [exclude null]
                          #:exclude-deps [exclude-deps null]
                          . module-paths)
  (let* ([table
          (get-dependencies-table #:include? #f module-paths)]
         [exclude-table
          (get-dependencies-table #:include? #t exclude)]
         [exclude-deps-roots
          (for/hash ([mod (in-list exclude-deps)])
            (values (resolve-module-path-index* (module-path-index-join mod #f) #f) #t))]
         [exclude-deps-table
          (get-dependencies-table #:include? #f exclude-deps)])
    (for ([key (in-hash-keys exclude-table)])
      (hash-remove! table key))
    (for ([key (in-hash-keys exclude-deps-table)])
      (unless (hash-ref exclude-deps-roots key #f)
        (hash-remove! table key)))
    (table->dependencies table)))

(define (show-dependencies #:exclude [exclude null]
                           #:exclude-deps [exclude-deps null]
                           #:show-context? [context? #f]
                           #:multi-line-context? [multi-line-context? #f]
                           . module-paths)
  (for ([dep (in-list (apply get-dependencies
                             #:exclude exclude
                             #:exclude-deps exclude-deps
                             module-paths))])
    (let ([mod (car dep)]
          [direct-requirers (cadr dep)])
      (parameterize ([pretty-print-columns 'infinity]) (pretty-write mod))
      (when context?
        (printf " <- ")
        (cond
          [multi-line-context?
           (for ([direct-requirer (in-list direct-requirers)]
                 [i (in-naturals)])
             (if (zero? i)
                 (printf "\n (")
                 (printf "\n  "))
             (parameterize ([pretty-print-columns 'infinity])
               (pretty-write direct-requirer)))
           (printf ")")]
          [else
           (parameterize ([pretty-print-columns 'infinity])
             (pretty-write direct-requirers))]))
      (newline))))

;; ====

(define (main . argv)
  (define mode 'auto)
  (define context? #f)
  (define multi-line-context? #f)
  (define excludes null)
  (define exclude-deps null)
  (command-line
   #:program (short-program+command-name)
   #:argv argv
   #:once-each
   [("-c" "--context") "Show who directly requires each module"
    (set! context? #t)]
   [("-l" "--multi-line-context") "Like --context, but use multiple lines"
    (set! context? #t)
    (set! multi-line-context? #t)]
   [("-f" "--file") "Interpret arguments as file-paths"
    (set! mode 'file)]
   [("-m" "--module-path") "Interpret arguments as module-paths"
    (set! mode 'module-path)]
   [("-x" "--exclude") mod "Exclude <mod> and its dependencies"
    (set! excludes (cons mod excludes))]
   [("-X" "--exclude-deps") mod "Exclude the dependencies of <mod> (but not <mod> itself)"
    (set! exclude-deps (cons mod exclude-deps))]
   [("-b") "Same as --exclude racket/base"
    (set! excludes (cons 'racket/base excludes))]
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
            #:exclude (map ->modpath excludes)
            #:exclude-deps (map ->modpath exclude-deps)
            #:show-context? context?
            #:multi-line-context? multi-line-context?
            (map ->modpath module-path)))))

(module* main #f
  (apply main (vector->list (current-command-line-arguments))))

#|

For example,

  raco show-dependencies -bc mzscheme

shows the additional modules used to implement mzscheme beyond those
already needed for the implementation of racket/base. And

  raco show-dependencies -bl syntax/parse/pre

shows that syntax/parse/pre has no dependencies on the contract
library. Actually, it shows that it has no *residual* dependencies;
contracts are used in the code that is lazily loaded, but using
syntax/parse/pre does not cause a module's compiled code to depend on
racket/contract/base.

|#
