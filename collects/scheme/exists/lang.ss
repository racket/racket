#lang scheme

(require racket/contract/exists)

;; this code builds the list of predicates (in case it changes, this may need to be re-run)
#;
(define predicates
  (let ([fn (build-path (collection-path "scheme")
                        "compiled" 
                        "main_ss.zo")])
    (let-values ([(vars stx)
                  (module-compiled-exports
                   (parameterize ([read-accept-compiled #t])
                     (call-with-input-file fn read)))])
      
      (filter (λ (sym) 
                (let ([str (symbol->string sym)])
                  (and (not (equal? str ""))
                       (regexp-match #rx"[?]$" str)
                       (not (regexp-match #rx"[=<>][?]$" str)))))
              (map car (cdr (assoc 0 vars)))))))


(define-for-syntax predicates
  '(absolute-path?
    arity-at-least?
    bitwise-bit-set?
    blame?
    boolean?
    box?
    byte-pregexp?
    byte-ready?
    byte-regexp?
    byte?
    bytes-converter?
    bytes?
    channel?
    char-alphabetic?
    char-blank?
    char-graphic?
    char-iso-control?
    char-lower-case?
    char-numeric?
    char-punctuation?
    char-ready?
    char-symbolic?
    char-title-case?
    char-upper-case?
    char-whitespace?
    char?
    class?
    compiled-expression?
    compiled-module-expression?
    complete-path?
    complex?
    cons?
    continuation-mark-set?
    continuation-prompt-available?
    continuation-prompt-tag?
    continuation?
    contract-first-order-passes?
    contract-stronger?
    contract?
    contract-property?
    contract-struct?
    custodian-box?
    custodian-memory-accounting-available?
    custodian?
    custom-write?
    date-dst?
    date?
    dict-can-functional-set?
    dict-can-remove-keys?
    dict-mutable?
    dict?
    directory-exists?
    empty?
    eof-object?
    ephemeron?
    eq?
    equal?
    eqv?
    even?
    evt?
    exact-integer?
    exact-nonnegative-integer?
    exact-positive-integer?
    exact?
    exn:break?
    exn:fail:contract:arity?
    exn:fail:contract:blame?
    exn:fail:contract:continuation?
    exn:fail:contract:divide-by-zero?
    exn:fail:contract:variable?
    exn:fail:contract?
    exn:fail:filesystem:exists?
    exn:fail:filesystem:version?
    exn:fail:filesystem?
    exn:fail:network?
    exn:fail:object?
    exn:fail:out-of-memory?
    exn:fail:read:eof?
    exn:fail:read:non-char?
    exn:fail:read?
    exn:fail:syntax?
    exn:fail:unsupported?
    exn:fail:user?
    exn:fail?
    exn:misc:match?
    exn:srclocs?
    exn?
    false?
    file-exists?
    file-stream-port?
    fixnum?
    flat-contract?
    flat-contract-property?
    flat-contract-struct?
    generic?
    handle-evt?
    hash-eq?
    hash-eqv?
    hash-has-key?
    hash-placeholder?
    hash-weak?
    hash?
    identifier?
    immutable?
    implementation?
    inexact-real?
    inexact?
    input-port?
    inspector?
    integer?
    interface-extension?
    interface?
    internal-definition-context?
    is-a?
    keyword?
    link-exists?
    list?
    log-level?
    log-receiver?
    logger?
    member-name-key?
    method-in-interface?
    module-path-index?
    module-path?
    module-provide-protected?
    mpair?
    namespace-anchor?
    namespace?
    negative?
    null?
    number?
    object-method-arity-includes?
    object?
    odd?
    output-port?
    pair?
    parameter?
    parameterization?
    path-for-some-system?
    path-string?
    path?
    placeholder?
    port-closed?
    port-provides-progress-evts?
    port-writes-atomic?
    port-writes-special?
    port?
    positive?
    pregexp?
    pretty-print-style-table?
    primitive-closure?
    primitive?
    procedure-arity-includes?
    procedure-arity?
    procedure-closure-contents-eq?
    procedure-struct-type?
    procedure?
    promise-forced?
    promise-running?
    promise?
    pseudo-random-generator?
    rational?
    readtable?
    real?
    regexp-match-exact?
    regexp-match?
    regexp?
    relative-path?
    rename-transformer?
    resolved-module-path?
    security-guard?
    semaphore-try-wait?
    semaphore?
    sequence?
    set!-transformer?
    special-comment?
    srcloc?
    string?
    struct-accessor-procedure?
    struct-constructor-procedure?
    struct-mutator-procedure?
    struct-predicate-procedure?
    struct-type-property?
    struct-type?
    struct?
    subclass?
    subprocess?
    symbol-interned?
    symbol?
    syntax-local-transforming-module-provides?
    syntax-original?
    syntax-transforming?
    syntax?
    system-big-endian?
    tcp-accept-ready?
    tcp-listener?
    tcp-port?
    terminal-port?
    thread-cell?
    thread-dead?
    thread-group?
    thread-running?
    thread?
    udp-bound?
    udp-connected?
    udp?
    unit?
    unknown?
    variable-reference?
    vector?
    void?
    weak-box?
    will-executor?
    zero?))

(define-syntax (predicates/provide stx)
  (with-syntax ([(predicates ...) predicates]
                [(-predicates ...) (map (λ (x) (string->symbol (format "-~a" x))) predicates)])
    #'(begin
        (define -predicates
          (let ([predicates (λ (x) 
                              (if (∃? x)
                                  (error 'predicates "supplied with a wrapped value ~e" x)
                                  (predicates x)))])
            predicates)) 
        ...
        (provide (rename-out (-predicates predicates) ...)
                 (except-out (all-from-out scheme)
                             define-struct
                             predicates ...)))))

(predicates/provide)
