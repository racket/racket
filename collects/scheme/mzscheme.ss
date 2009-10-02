;;----------------------------------------------------------------------
;; mzscheme: provide everything

(module mzscheme '#%kernel
  (#%require "private/more-scheme.ss"
             "private/misc.ss"
             "private/letstx-scheme.ss"
             "private/stxcase-scheme.ss"
             "private/stx.ss"
             "private/stxmz-body.ss"
             "private/qqstx.ss"
             "private/define.ss"
             "private/old-ds.ss"
             "private/old-rp.ss"
             "private/old-if.ss"
             "private/old-procs.ss"
             "private/map.ss" ; shadows #%kernel bindings
             "private/kernstruct.ss"
             "promise.ss"
             (only "private/cond.ss" old-cond)
             "tcp.ss"
             "udp.ss"
             '#%builtin) ; so it's attached

  (#%provide require require-for-syntax require-for-template require-for-label
             provide provide-for-syntax provide-for-label
             (all-from-except "private/more-scheme.ss" case old-case 
                              log-fatal log-error log-warning log-info log-debug
                              hash-update hash-update!)
             (rename old-case case)
             (all-from "private/misc.ss")
             (all-from-except "private/stxcase-scheme.ss" _)
             (all-from-except "private/letstx-scheme.ss" 
                              -define -define-syntax -define-struct
                              cond old-cond else =>)
             (rename old-cond cond)
             define-struct let-struct
             identifier? ;; from "private/stx.ss"
             (all-from "private/qqstx.ss")
             (all-from "private/define.ss")
             (all-from "private/kernstruct.ss")
             force delay promise?
             (all-from-except '#%kernel #%module-begin #%datum 
                              if make-empty-namespace
                              syntax->datum datum->syntax
                              free-identifier=?
                              free-transformer-identifier=?
                              free-template-identifier=?
                              free-label-identifier=?
                              vector-copy!
                              thread-send
                              thread-receive
                              thread-try-receive
                              thread-receive-evt
                              make-hash make-immutable-hash make-weak-hash
                              make-hasheq make-immutable-hasheq make-weak-hasheq
                              hash? hash-eq? hash-weak?
                              hash-ref hash-set! hash-set
                              hash-remove! hash-remove 
                              hash-copy hash-count
                              hash-map hash-for-each 
                              hash-iterate-first hash-iterate-next
                              hash-iterate-value hash-iterate-key
                              log-message log-level? make-logger logger? current-logger logger-name
                              make-log-receiver log-receiver?)
             (rename syntax->datum syntax-object->datum)
             (rename datum->syntax datum->syntax-object)
             (rename free-identifier=? module-identifier=?)
             (rename free-transformer-identifier=? module-transformer-identifier=?)
             (rename free-template-identifier=? module-template-identifier=?)
             (rename free-label-identifier=? module-label-identifier=?)
             (rename free-identifier=?* free-identifier=?)
             make-hash-table hash-table? make-immutable-hash-table
             (rename hash-ref hash-table-get)
             (rename hash-set! hash-table-put!)
             (rename hash-remove! hash-table-remove!)
             (rename hash-count hash-table-count)
             (rename hash-copy hash-table-copy)
             (rename hash-map hash-table-map)
             (rename hash-for-each hash-table-for-each)
             (rename hash-iterate-first hash-table-iterate-first)
             (rename hash-iterate-next hash-table-iterate-next)
             (rename hash-iterate-value hash-table-iterate-value)
             (rename hash-iterate-key hash-table-iterate-key)
             namespace-transformer-require
             transcript-on transcript-off
             (rename cleanse-path expand-path)
             (rename if* if)
             (rename list list-immutable)
             make-namespace
             #%top-interaction
             map for-each andmap ormap
             (rename datum #%datum)
             (rename mzscheme-in-stx-module-begin #%module-begin)
             (rename #%module-begin #%plain-module-begin)
             (rename lambda #%plain-lambda)
             (rename #%app #%plain-app)
             (all-from "tcp.ss")
             (all-from "udp.ss")))
