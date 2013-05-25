#lang scheme

(require racket/contract/private/exists)

;; this code builds the list of predicates (in case it changes, this may need to be re-run)
#;
(define runtime-predicates
  (let ([fn (build-path (collection-path "scheme")
                        "compiled" 
                        "main_rkt.zo")]
        [ns (make-base-namespace)])
    (namespace-attach-module (current-namespace) 'scheme ns)
    (parameterize ([current-namespace ns])
      (namespace-require 'scheme))
    (let-values ([(vars stx)
                  (module-compiled-exports
                   (parameterize ([read-accept-compiled #t])
                     (call-with-input-file fn read)))])
      (sort
       (filter (λ (sym) 
                 (let ([str (symbol->string sym)])
                   (and (regexp-match #rx"[?]$" str)
                        (not (regexp-match #rx"[=<>][?]$" str))
                        (procedure-arity-includes? 
                         (namespace-variable-value sym #t #f ns)
                         1))))
               (map car (cdr (assoc 0 vars))))
       string<=?
       #:key symbol->string))))

(define-for-syntax predicates
  '(absolute-path?
    arity-at-least?
    blame-original?
    blame-swapped?
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
    chaperone-contract-property?
    chaperone-contract?
    chaperone?
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
    contract-property?
    contract?
    custodian-box?
    custodian?
    custom-print-quotable?
    custom-write?
    date-dst?
    date?
    directory-exists?
    double-flonum?
    empty?
    eof-object?
    ephemeron?
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
    exn:fail:contract:non-fixnum-result?
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
    flat-contract-property?
    flat-contract?
    flonum?
    generic?
    handle-evt?
    has-contract?
    hash-eq?
    hash-equal?
    hash-eqv?
    hash-placeholder?
    hash-weak?
    hash?
    identifier?
    immutable?
    impersonator-property-accessor-procedure?
    impersonator-property?
    impersonator?
    inexact-real?
    inexact?
    input-port?
    inspector?
    integer?
    interface?
    internal-definition-context?
    keyword?
    link-exists?
    list?
    log-receiver?
    logger?
    member-name-key?
    module-path-index?
    module-path?
    mpair?
    namespace-anchor?
    namespace?
    negative?
    null?
    number?
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
    procedure-arity?
    procedure-struct-type?
    procedure?
    promise-forced?
    promise-running?
    promise?
    pseudo-random-generator?
    rational?
    readtable?
    real?
    regexp?
    relative-path?
    rename-transformer?
    resolved-module-path?
    security-guard?
    semaphore-try-wait?
    semaphore?
    sequence?
    set!-transformer?
    single-flonum?
    special-comment?
    srcloc?
    string?
    struct-accessor-procedure?
    struct-constructor-procedure?
    struct-mutator-procedure?
    struct-predicate-procedure?
    struct-type-property-accessor-procedure?
    struct-type-property?
    struct-type?
    struct?
    subprocess?
    symbol-interned?
    symbol-unreadable?
    symbol?
    syntax-original?
    syntax?
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
    unsupplied-arg?
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
                              (if (∀∃? x)
                                  (error 'predicates "supplied with a wrapped value ~e" x)
                                  (predicates x)))])
            predicates)) 
        ...
        (provide (rename-out (-predicates predicates) ...)
                 (except-out (all-from-out scheme)
                             define-struct
                             predicates ...)))))

(predicates/provide)
