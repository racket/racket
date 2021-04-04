;; Also used by "cross-serve.ss"

(when omit-debugging?
  (generate-inspector-information (not omit-debugging?))
  (generate-procedure-source-information #t))

(when measure-performance?
  (#%$enable-pass-timing #t)
  (#%$clear-pass-stats))

(enable-arithmetic-left-associative #t)
(expand-omit-library-invocations #t)
(enable-error-source-expression #f)
(fasl-compressed #f)
(compile-omit-concatenate-support #t)

;; Avoid gensyms for generated record-type UIDs. Otherwise,
;; printing one of those gensyms --- perhaps when producing a trace
;; via `dump-memory-stats` --- causes the gensym to be permanent
;; (since it has properties).
(current-generate-id (lambda (sym) (gensym sym)))

;; Since the schemify layer inserts `|#%app|` any time the rator of
;; an application might not be a procedure, we can avoid redundant
;; checks for other applications by enabling unsafe mode. Ditto for
;; potential early reference to `letrec`-bound variables. But do that
;; only if we're compiling the primitive layer in unsafe mode.
(meta-cond
 [(>= (optimize-level) 3)
  (enable-unsafe-application #t)
  (enable-unsafe-variable-reference #t)]
 [else
  (void)])
