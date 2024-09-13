#lang racket/base
(require racket/path
         racket/set
         "module-path.rkt"
         "path-submod.rkt"
         "one-mod.rkt"
         "log.rkt"
         "at-phase-level.rkt")

(provide partition-panes
         reify-panes)

;; A "pane" is a submodule that serves as the home for a group of
;; modules. In slice mode, as used for an executable without syntax
;; that keeps only phase-0 code, each phase level of each module
;; lieves in a pane, but different phase levels can be different panes
;; (i.e., when the pane's content is pruned to phase 0, it will have
;; the only instance of a given module at the relevant phase). In
;; non-slide mode, as used for a library that keeps syntax, each
;; module (with all its phases) lives in a single pane.

;; Returns an order list mapping an optional path/submod to the path/submods that
;; are to be uniquely demodularized into the path/submod. If the option path/submod
;; is #f, that's a module to reference directly, because it cannot be usefuly
;; combined with any other module and `external-singetons?` is true. Every non-excluded
;; module is represented exactly once in the list.
(define (partition-panes one-mods orig-top-path submods
                         ;; `slice?` determines whether we can break up
                         ;; module phase levels into different panes, which is
                         ;; useful for keeping only the code needed overall for
                         ;; phase 0
                         #:slice? slice?
                         ;; `external-singletons?` determines whether a pane
                         ;; that contains just one module should be omitted
                         ;; from any pane, leaving a reference to the original
                         ;; external module, instead
                         #:external-singetons? external-singletons?)
  (define top-path (simple-form-path orig-top-path))

  ;; pane = (cons (set entry ...) (set phase ...))
  ;; Non-slice mode:
  ;;   origin = path/submod
  ;; Slice mode:
  ;;   origin = (cons path/submod phase-shift)

  (define uses (make-hash))   ; origin -> pane

  (define (make-origin path/submod phase-shift)
    (if slice?
        (values (cons path/submod phase-shift) 0)
        (values path/submod phase-shift)))
  (define (origin-path/submod origin)
    (if slice? (car origin) origin))
  (define (origin-phase-shift origin)
    (if slice? (cdr origin) 0))
  
  (define (traverse! path/submod entry phase-shift)
    (unless (symbol? path/submod)
      (define one-m (hash-ref one-mods path/submod))
      (unless (one-mod-excluded? one-m)
        (define-values (origin rel-phase-shift) (make-origin path/submod phase-shift))
        (define done (hash-ref uses origin '(#hash() . #hash())))
        (unless (and (hash-ref (car done) entry #f)
                     (hash-ref (cdr done) rel-phase-shift #f))
          (hash-set! uses origin
                     (cons (hash-set (car done) entry #t)
                           (hash-set (cdr done) rel-phase-shift #t)))
          (for* ([(req-phase-shift path/submods) (in-hash (one-mod-reqs one-m))]
                 [(path/submod) (in-list path/submods)])
            (traverse! path/submod entry (+ phase-shift req-phase-shift)))))))

  (for ([submod (in-list (cons '() submods))])
    (traverse! (path/submod-join top-path submod) submod 0))

  (log-demodularizer-debug " Uses:")
  (for ([(origin entries-and-phases) (in-hash uses)])
    (log-demodularizer-debug " ~a: ~a ~a"
                             origin
                             (hash-keys (car entries-and-phases))
                             (hash-keys (cdr entries-and-phases))))

  (define pre-panes ; pane -> (set origin ...)
    (for/fold ([panes (hash)])
              ([(origin entries-and-phases) (in-hash uses)])
      (hash-set panes entries-and-phases
                (hash-set (hash-ref panes entries-and-phases (hash))
                          origin
                          #t))))

  ;; If two panes have the same entry points and the same phase shifts, but
  ;; shifted relative to each other, then the panes can be merged. This step
  ;; not only reduces the number of panes, it is needed for the panes to not
  ;; have import cycles among them in the case of non-sliced modules. In
  ;; slice mode, meanwhile, it's only the entry points that matter, and all
  ;; phase set have just 0s.
  (define merges ; pane -> (cons pane-to-merge-key-into phase shift)
    (let loop ([entries+phasess (sort (hash-keys pre-panes)
                                      <
                                      #:key (lambda (entrys+phases)
                                              (apply min (hash-keys (cdr entrys+phases)))))]
               [merges (hash)])
      (cond
        [(null? entries+phasess) merges]
        [else
         (define entries+phases (car entries+phasess))
         (cond
           [(hash-ref merges entries+phases #f)
            ;; already merged
            (loop (cdr entries+phasess) merges)]
           [else
            (define new-merges
              (for/fold ([merges merges]) ([entries+phases2 (in-list (cdr entries+phasess))])
                (cond
                  [(equal? (car entries+phases) (car entries+phases2))
                   (define phases (cdr entries+phases))
                   (define phases2 (cdr entries+phases2))
                   (define (get-min-phase phases) (apply min (hash-keys phases)))
                   (cond
                     [(and (= (hash-count phases) (hash-count phases2))
                           (let ([delta (- (get-min-phase phases2)
                                           (get-min-phase phases))])
                             (and (for/and ([phase (in-hash-keys phases)])
                                    (hash-ref phases2 (+ phase delta) #f))
                                  delta)))
                      => (lambda (delta)
                           (hash-set merges entries+phases2 (cons entries+phases
                                                                  delta)))]
                     [else merges])]
                  [else merges])))
            (loop (cdr entries+phasess) new-merges)])])))

  (when (and slice? (positive? (hash-count merges)))
    (error "should not have found any merges in non-slice mode"))

  (define merge-ins ; pane -> (list (cons panes-to-merge-into-key phase-shift))
    (for/fold ([merge-ins #hash()]) ([(from to+delta) (in-hash merges)])
      (define to (car to+delta))
      (define delta (cdr to+delta))
      (hash-set merge-ins to (cons (cons from delta) (hash-ref merge-ins to null)))))
  
  (define panes ; pane -> (list (cons origin phase-shift) ...)
    (for/hash ([(pane origins) (in-hash pre-panes)]
               #:unless (hash-ref merges pane #f))
      (define path/submod+shifts
        (append (for/list ([origin (in-hash-keys origins)])
                  (cons (origin-path/submod origin)
                        (origin-phase-shift origin)))
                (apply
                 append
                 (for/list ([pane+delta (in-list (hash-ref merge-ins pane null))])
                   (define pane (car pane+delta))
                   (define delta (cdr pane+delta))
                   (for/list ([origin (in-hash-keys (hash-ref pre-panes pane))])
                     (cons (origin-path/submod origin)
                           delta))))))
      (values pane
              (sort path/submod+shifts <
                    #:key (lambda (path/submod+delta)
                            (one-mod-order (hash-ref one-mods (car path/submod+delta))))))))

  (define (any-pane-dependency? path/submod)
    (define one-m (hash-ref one-mods path/submod))
    (for*/or ([uses (in-hash-values (one-mod-phase-uses one-m))]
              [path/submod+phase-level (in-list uses)])
      (define dep-path/submod (car path/submod+phase-level))
      (and (not (symbol? dep-path/submod))
           (not (one-mod-excluded? (hash-ref one-mods dep-path/submod))))))

  ;; Name the panes, using an existing submodule name if one is within the pane,
  ;; or an external module if there's only one module in the pane and it does
  ;; not refer to any other panes
  (define-values (named-panes ; (list (cons path/submod-or-#f (list (cons path/submod delta) ...)) ...)
                  added-submods)
    (for/fold ([named-panes null]
               [added-submods null])
              ([(pane path/submod+deltas) (in-hash panes)]
               [i (in-naturals)])
      (define unique-submod
        (for/fold ([submod #f]) ([path/submod+delta (in-list path/submod+deltas)])
          (define path/submod (car path/submod+delta))
          (cond
            [(eq? submod 'many) 'many]
            [(equal? (path/submod-path path/submod) top-path)
             (if (not submod)
                 (path/submod-submod path/submod)
                 'many)]
            [else submod])))
      (when (eq? unique-submod 'many)
        (error "two entry-point submodules are in the same pane"))
      (define-values (name added-submod)
        (cond
          [unique-submod
           (values (path/submod-join top-path unique-submod)
                   #f)]
          [(and (null? (cdr path/submod+deltas))
                external-singletons?
                (not (any-pane-dependency? (caar path/submod+deltas))))
           ;; one non-submodule; no demodularization is useful
           (values #f #f)]
          [else
           (define added-submod (string->symbol (format "demod-pane-~a" i)))
           (values (path/submod-join top-path (list added-submod))
                   added-submod)]))

      (log-demodularizer-debug "  ~a = ~a ~a"
                               name
                               (hash-keys (car pane))
                               (hash-keys (cdr pane)))

      (values (cons (cons name
                          path/submod+deltas)
                    named-panes)
              (if added-submod
                  (cons added-submod added-submods)
                  added-submods))))

  ;; sort panes based on shallowest (largest order index) module in pane
  (define sorted-panes ; (list (cons path/submod-or-#f (list (cons path/submod delta) ...)) ...)
    (sort named-panes
          <
          #:cache-keys? #t
          #:key (lambda (pane+path/submod+deltas)
                  (apply max (for/list ([path/submod+delta (in-list (cdr pane+path/submod+deltas))])
                               (define path/submod (car path/submod+delta))
                               (define m (hash-ref one-mods path/submod))
                               (one-mod-order m))))))

  (values sorted-panes
          added-submods))

;; Remove panes that have `#f` names, and set the corresponding module in `one-mods`
;; to be excluded. For panes that are new, synthesized submodules, create
;; an entry on `one-mods` for the modules. Return just the list of path/names
;; for the submodules to (re-)export demodularized content.
(define (reify-panes sorted-panes one-mods common-excluded-module-mpis
                     #:slice? slice?)
  (define new-sorted-panes
    (for/list ([path/submod+pane-content (in-list sorted-panes)]
               #:do [(define path/submod (car path/submod+pane-content))
                     (when (not path/submod)
                       ;; Singleton to change to excluded
                       (define content (cdr path/submod+pane-content))
                       (define path/submod+phase (car content))
                       (define path/submod (car path/submod+phase))
                       (define one-m (hash-ref one-mods path/submod))
                       (log-demodularizer-debug " Dropping single-module pane: ~a" path/submod)
                       (set! common-excluded-module-mpis
                             (hash-set common-excluded-module-mpis path/submod (cons (one-mod-rel-mpi one-m) 0)))
                       (hash-set! one-mods path/submod (struct-copy one-mod one-m
                                                                    [excluded? #t])))]
               #:when path/submod)
      (unless (hash-ref one-mods path/submod #f)
        ;; Synthesize a `one-mod` record for submodule that holds a pane
        (define rev-reqs
          (for/fold ([reqs #hasheqv()])
                    ([path/submod+delta (in-list (cdr path/submod+pane-content))])
            (define path/submod (car path/submod+delta))
            (define delta (cdr path/submod+delta))
            (hash-set reqs delta (cons path/submod (hash-ref reqs delta null)))))
        (define-values (min-phase max-phase)
          (for/fold ([min-phase 0]
                     [max-phase 0])
                    ([path/submod+delta (in-list (cdr path/submod+pane-content))])
            (define path/submod (car path/submod+delta))
            (define m (hash-ref one-mods path/submod))
            (values (min min-phase (one-mod-min-phase m))
                    (max max-phase (one-mod-max-phase m)))))
        (hash-set! one-mods path/submod
                   (one-mod 0
                            #f ; excluded?
                            #f ; rel-mpi
                            #f ; zo
                            #f ; decl
                            #hasheqv() ; phase-uses
                            (for/hasheqv ([(phase rev-path/submods) (in-hash rev-reqs)])
                              (values phase (reverse rev-path/submods)))
                            #hasheqv() ; exports
                            min-phase
                            max-phase
                            #hasheqv() ; provides
                            #() ; stx-vec
                            #f ; stx-mpi
                            #hasheqv() ; portal-stxes
                            null       ; pre-submodules
                            null)))    ; post-submodules
      path/submod+pane-content))

  ;; For each pane submodule, build an exclusion list that points to the other submodules
  (define self-mpi (module-path-index-join #f #f))
  (define excluded-module-mpiss ; key -> (cons mpi phase-shift)
    ;;                            where a key can be a path/submod (always for non-slice mode)
    ;;                                  or it can be a (at-phase-level path/submod phase)
    ;;                                        to indicate omission at a specific phase level
    ;;                           The phase-shift is how much to add to a phase level of
    ;;                           a module in the pane to select the right phase level
    ;;                           of the pane
    (for/list ([path/submod+pane-content (in-list new-sorted-panes)])
      (define path/submod (car path/submod+pane-content))
      (define submod (path/submod-submod path/submod))
      (define dots (map (lambda (s) "..") submod))
      (define included ; hash set of phase/submod+phase-shift
        (and slice?
             (for/hash ([path/submod+phase-shift (in-list (cdr path/submod+pane-content))])
               (values path/submod+phase-shift #t))))
      (for/fold ([excluded-module-mpis common-excluded-module-mpis])
                ([other-path/submod+pane-content (in-list new-sorted-panes)]
                 #:do [(define other-path/submod (car other-path/submod+pane-content))
                       (define other-pane-content (cdr other-path/submod+pane-content))]
                 #:unless (equal? path/submod other-path/submod))
        (define other-submod (path/submod-submod other-path/submod))
        (define mpi (let* ([mpi self-mpi]
                           [mpi (if (pair? dots)
                                    (module-path-index-join `(submod ,@dots) mpi)
                                    mpi)]
                           [mpi (if (pair? other-submod)
                                    (module-path-index-join `(submod "." ,@other-submod) mpi)
                                    mpi)])
                      mpi))
        (for/fold ([excluded-module-mpis excluded-module-mpis])
                  ([path/submod+phase-shift (in-list other-pane-content)])
          (cond
            [(or (not slice?) (not (hash-ref included path/submod+phase-shift #f)))
             (define path/submod (car path/submod+phase-shift))
             (define phase-shift (cdr path/submod+phase-shift))
             (define key (if slice? (at-phase-level path/submod (- phase-shift)) path/submod))
             (hash-set excluded-module-mpis key (cons mpi phase-shift))]
            [else excluded-module-mpis])))))

  (define included-module-phasess
    (for/list ([path/submod+pane-content (in-list new-sorted-panes)])
      (define content (cdr path/submod+pane-content))
      (for/hash ([path/submod+phase (in-list content)])
        (values (car path/submod+phase)
                (cdr path/submod+phase)))))

  (log-demodularizer-debug " Panes: ~a" (length new-sorted-panes))
  (for ([path/submod+content (in-list new-sorted-panes)]
        [excluded-module-mpis (in-list excluded-module-mpiss)])
    (define path/submod (car path/submod+content))
    (define content (cdr path/submod+content))
    (log-demodularizer-debug "  ~s:" path/submod)
    (for ([path/submod+phase (in-list content)])
      (log-demodularizer-debug "    ~a ~a" (car path/submod+phase) (cdr path/submod+phase)))
    #;
    (log-demodularizer-debug "   NOT")
    #;
    (for ([(key phase-shift) (in-hash excluded-module-mpis)])
      (log-demodularizer-debug "    ~a ~a" key phase-shift)))

  (values (map car new-sorted-panes)
          excluded-module-mpiss
          included-module-phasess
          ;; `one-mods` return value is just a hacky hint that this function is meant to change it
          one-mods))
