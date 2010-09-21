#lang racket
(require racket/set
         rackunit
         compiler/decompile
         compiler/zo-parse)
(require/expose compiler/decompile
                (primitive-table))

(define file-to-batch
  (command-line #:program "batch" #:args (filename) filename))

(define the-zo
  (call-with-input-file file-to-batch zo-parse))

(define (find-prims ps v)
  (cond
    [(primval? v)
     (set-add ps (primval-id v))]
    [(or (symbol? v) (number? v) (boolean? v) (module-path-index? v) (path? v) (char? v) (void? v) (string? v) (keyword? v) (regexp? v) (byte-regexp? v) (placeholder? v) (hash-placeholder? v))
     ps]
    [(sequence? v)
     (for/fold ([ps ps])
       ([e v])
       (find-prims ps e))]
    [(cons? v)
     (find-prims (find-prims ps (car v)) (cdr v))]
    [(struct? v)
     (find-prims ps (struct->vector v))]
    [else
     (error 'find-prims "Can't look in ~e~n" v)]))

(for ([p (in-set (find-prims (set) the-zo))])
  (printf "~s~n" (hash-ref primitive-table p)))
      