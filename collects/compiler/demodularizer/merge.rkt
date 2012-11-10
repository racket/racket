#lang racket/base

(require racket/list
         racket/match
         racket/contract
         compiler/zo-parse
         "util.rkt"
         "mpi.rkt"
         "nodep.rkt"
         "update-toplevels.rkt")

(define MODULE-TOPLEVEL-OFFSETS (make-hash))

(define current-get-modvar-rewrite (make-parameter #f))
(define (merge-compilation-top get-modvar-rewrite top)
  (parameterize ([current-get-modvar-rewrite get-modvar-rewrite])
    (match top
      [(struct compilation-top (max-let-depth prefix form))
       (define-values (new-max-let-depth new-prefix gen-new-forms)
         (merge-form max-let-depth prefix form))
       (define total-tls (length (prefix-toplevels new-prefix)))
       (define total-stxs (length (prefix-stxs new-prefix)))
       (define total-lifts (prefix-num-lifts new-prefix))
       (log-debug (format "max-let-depth ~S to ~S" max-let-depth new-max-let-depth))
       (log-debug (format "total toplevels ~S" total-tls))
       (log-debug (format "total stxs ~S" total-stxs))
       (log-debug (format "num-lifts ~S" total-lifts))
       (make-compilation-top 
        new-max-let-depth new-prefix 
        (make-splice (gen-new-forms new-prefix)))]
      [else (error 'merge "unrecognized: ~e" top)])))

(define (merge-forms max-let-depth prefix forms)
  (if (empty? forms)
      (values max-let-depth prefix (lambda _ empty))
      (let*-values ([(fmax-let-depth fprefix gen-fform) (merge-form max-let-depth prefix (first forms))]
                    [(rmax-let-depth rprefix gen-rforms) (merge-forms fmax-let-depth fprefix (rest forms))])
        (values rmax-let-depth
                rprefix
                (lambda args
                  (append (apply gen-fform args)
                          (apply gen-rforms args)))))))

(define (merge-form max-let-depth prefix form)
  (match form
    [(? mod?)
     (merge-module max-let-depth prefix form)]
    [(struct seq (forms))
     (merge-forms max-let-depth prefix forms)]
    [(struct splice (forms))
     (merge-forms max-let-depth prefix forms)]
    [else
     (values max-let-depth prefix (lambda _ (list form)))]))

(define (merge-prefix root-prefix mod-prefix)
  (match root-prefix
    [(struct prefix (root-num-lifts root-toplevels root-stxs))
     (match mod-prefix
       [(struct prefix (mod-num-lifts mod-toplevels mod-stxs))
        (make-prefix (+ root-num-lifts mod-num-lifts)
                     (append root-toplevels mod-toplevels)
                     (append root-stxs mod-stxs))])]))

(define (compute-new-modvar mv rw)
  (match mv
    [(struct module-variable (modidx sym pos phase constantness))
     (match rw
       [(struct modvar-rewrite (self-modidx provide->toplevel))
        (log-debug (format "Rewriting ~a of ~S" pos (mpi->path* modidx)))
        ((hash-ref MODULE-TOPLEVEL-OFFSETS self-modidx
                     (lambda ()
                       (error 'compute-new-modvar "toplevel offset not yet computed: ~S" self-modidx)))
         (provide->toplevel sym pos))])]))

(define (filter-rewritable-module-variable? toplevel-offset mod-toplevels)
  (define-values
    (i new-toplevels remap)
    (for/fold ([i 0]
               [new-toplevels empty]
               [remap empty])
      ([tl (in-list mod-toplevels)])
      (match tl
        [(and mv (struct module-variable (modidx sym pos phase constantness)))
         (define rw ((current-get-modvar-rewrite) modidx))
         ; XXX We probably don't need to deal with #f phase
         (unless (or (not phase) (zero? phase))
           (error 'eliminate-module-variables "Non-zero phases not supported: ~S" mv))
         (cond
           ; Primitive module like #%paramz
           [(symbol? rw)
            (log-debug (format "~S from ~S" sym rw))
            (values (add1 i)
                    (list* tl new-toplevels)
                    (list* (+ i toplevel-offset) remap))]
           [(module-path-index? rw)
            (values (add1 i)
                    (list* tl new-toplevels)
                    (list* (+ i toplevel-offset) remap))]
           [(modvar-rewrite? rw)
            (values i
                    new-toplevels
                    (list* (compute-new-modvar mv rw) remap))]
           [else
            (error 'filter-rewritable-module-variable? "Unsupported module-rewrite: ~S" rw)])]
        [tl
         (values (add1 i)
                 (list* tl new-toplevels)
                 (list* (+ i toplevel-offset) remap))])))
  ; XXX This would be more efficient as a vector
  (values (reverse new-toplevels)
          (reverse remap)))

(define (merge-module max-let-depth top-prefix mod-form)
  (match mod-form
    [(struct mod (name srcname self-modidx mod-prefix provides requires body syntax-bodies
                       unexported mod-max-let-depth dummy lang-info internal-context
                       pre-submodules post-submodules))
     (define toplevel-offset (length (prefix-toplevels top-prefix)))
     (define topsyntax-offset (length (prefix-stxs top-prefix)))
     (define lift-offset (prefix-num-lifts top-prefix))
     (define mod-toplevels (prefix-toplevels mod-prefix))     
     (define-values (new-mod-toplevels toplevel-remap) (filter-rewritable-module-variable? toplevel-offset mod-toplevels))
     (define num-mod-toplevels
       (length toplevel-remap))
     (define mod-stxs
       (length (prefix-stxs mod-prefix)))
     (define mod-num-lifts
       (prefix-num-lifts mod-prefix))
     (define new-mod-prefix
       (struct-copy prefix mod-prefix
                    [toplevels new-mod-toplevels]))
     (hash-set! MODULE-TOPLEVEL-OFFSETS self-modidx 
                (lambda (n)
                  (list-ref toplevel-remap n)))
     (unless (= (length toplevel-remap)
                (length mod-toplevels))
       (error 'merge-module "Not remapping everything: ~S ~S" 
              mod-toplevels toplevel-remap))    
     (log-debug (format "[~S] Incrementing toplevels by ~a"
              name
              toplevel-offset))
     (log-debug (format "[~S] Incrementing lifts by ~a"
              name
              lift-offset))
     (log-debug (format "[~S] Filtered mod-vars from ~a to ~a" 
              name
              (length mod-toplevels)
              (length new-mod-toplevels)))
     (values (max max-let-depth mod-max-let-depth)
             (merge-prefix top-prefix new-mod-prefix)
             (lambda (top-prefix)
               (log-debug (format "[~S] Updating top-levels" name))
               (define top-lift-start (prefix-lift-start top-prefix))
               (define mod-lift-start (prefix-lift-start mod-prefix))
               (define total-lifts (prefix-num-lifts top-prefix))
               (define max-toplevel (+ top-lift-start total-lifts))
               (define update
                 (update-toplevels 
                  (lambda (n)
                    (cond
                      [(mod-lift-start . <= . n)
                       ; This is a lift
                       (define which-lift (- n mod-lift-start))
                       (define lift-tl (+ top-lift-start lift-offset which-lift))
                       (when (lift-tl . >= . max-toplevel)
                         (error 'merge-module "[~S] lift error: orig(~a) which(~a) max(~a) lifts(~a) now(~a)" 
                                name n which-lift num-mod-toplevels mod-num-lifts lift-tl))
                       lift-tl]
                      [else
                       (list-ref toplevel-remap n)]))
                  (lambda (n)
                    (+ n topsyntax-offset))
                  (prefix-syntax-start top-prefix)))
               (map update body)))]))

(provide/contract
 [merge-compilation-top (-> get-modvar-rewrite/c
                            compilation-top?
                            compilation-top?)])
