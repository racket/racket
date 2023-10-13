#lang racket/base

(define-syntax-rule (define-fake id ...)
  (begin
    (provide id ...)
    (define id 'id) ...))

(define-syntax-rule (intro-define-fake)
  (define-fake
    lambda
    let
    let*
    letrec
    if
    and
    or
    when
    unless
    begin
    cond
    else
    quote
    quasiquote
    unquote
    unquote-splicing
    quote-syntax

    define
    define-syntax
    struct
    include
    require
    provide
    module+
    quote-module-path

    pair?
    null?
    integer?
    string?
    symbol?
    hash?
    list?
    procedure?
    path-string?
    module-path?
    relative-path?
    handle?
    boolean?
    void

    apply
    call/cc
    call/prompt
    continuation-prompt-available?
    context-consumer
    context-consumer?

    cons
    car
    cdr
    list
    list*
    append
    reverse
    length
    member
    assoc
    remove
    list-ref
    list-set
    list-tail

    caar cadr cdar cddr

    andmap
    ormap
    map
    filter
    sort
    foldl
    for-each

    not
    eq?
    equal?
    void?

    +
    -
    *
    quotient
    modulo
    <
    <=
    =
    >=
    >
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-not

    string-length
    string-ref
    string-u32-ref
    substring
    string=?
    string-ci=?
    string<?
    string->integer
    string->symbol
    string->uninterned-symbol
    symbol->string
    string
    string-sha256
    char
    string-split string-join string-trim
    string-tree?

    hash
    hash-ref
    ref
    hash-set
    hash-remove
    hash-keys
    hash-count
    hash-keys-subset?

    opaque
    opaque-ref

    build-path
    split-path
    at-source

    variable?
    variable
    variable-ref
    variable-set!

    identifier?
    syntax-e
    syntax->datum
    datum->syntax
    bound-identifier=?
    syntax-error
    bad-syntax
    misplaced-syntax
    duplicate-identifier

    fd-open-input
    fd-open-output
    fd-close
    fd-read
    fd-write
    fd-poll
    eof
    fd-terminal?
    fd-valid?
    file->string
    display-to-file

    stat
    ls rm mv mkdir rmdir symlink readlink cp
    current-time
    system-type
    file-exists?
    directory-exists?
    link-exists?
    explode-path
    simple-form-path
    find-relative-path
    build-raw-path
    path-replace-extension
    path-only
    file-name-from-path
    path->complete-path
    ls* rm* cp* mkdir-p
    :no-replace-mode
    :error :truncate :must-truncate :append :update :can-update
    cleanable-file
    cleanable-cancel

    process
    process-status
    process-wait
    find-executable-path
    shell->strings
    string->shell

    error
    alert
    ~v
    ~a
    ~s
    arity-error
    arg-error
    display displayln

    string-read
    module->hash
    build-module-path
    kernel-env
    kernel-eval
    dynamic-require

    runtime-env
    dump-image-and-exit
    exit
    suspend-signal resume-signal

    command-line

    target
    rule
    phony-rule
    input-file-target
    input-data-target
    target-path
    target-shell
    target-name
    target?
    token?
    rule?
    phony-rule?
    sha256?
    file-sha256
    no-sha256
    sha256-length
    build
    build/command-line
    build/command-line*
    build/dep
    build/no-dep
    provide-targets
    find-target
    make-at-dir
    make-targets
    command-target?
    command-target->target
    bounce-to-targets

    glob->matcher glob-match?

    shell
    shell/wait
    build-shell
    shell-subst

    c-compile c-link c-ar
    .c->.o .exe .a
    config-merge config-include config-define

    call-in-main-thread
    thread? thread channel? channel channel-put channel-get channel-try-get
    thread-process-wait
    config-file->hash

    maybe-jobserver-client
    maybe-jobserver-jobs

    maybe-dry-run-mode))

(intro-define-fake)
