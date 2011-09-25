#lang racket/base
(require racket/class
         wxme)

(provide reader)

(define what "collapsed-sexp")

(define reader
  (new (class* object% (snip-reader<%>)
         (define/public (read-header version stream) (void))
         (define/public (read-snip text-only? version stream)
           (define left (send stream read-bytes what))
           (define right (send stream read-bytes what))
           (define count (send stream read-integer what))
           (define snips
             (for/list ([x (in-range 0 count)])
               (define snip-class-name (bytes->string/utf-8 (send stream read-bytes what)))
               (read-snip-from-port snip-class-name
                                    'collapsed-snipclass-wxme.rkt
                                    stream)))
           (apply bytes-append snips))
         (super-new))))
