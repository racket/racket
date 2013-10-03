#lang racket/base
(require racket/gui/base
         racket/class
         racket/path
         racket/file
         racket/set
         setup/collects
         setup/dirs
         setup/getinfo
         syntax/modread
         syntax/modcode
         syntax/modresolve
         string-constants
         framework
         compiler/compile-file
         
         "create-htdp-executable.rkt")

(provide get-teachpack-from-user)

(define user-installed-teachpacks-collection "installed-teachpacks")
(define teachpack-installation-dir 
  (build-path (find-user-collects-dir) user-installed-teachpacks-collection))

(define (get-teachpack-from-user parent tp-dirs labels tp-syms [already-installed-teachpacks '()])
  (define tpss (map tp-dir->tps tp-syms))
  
  (define label+mpss
    (let ([all-filenames 
           (apply
            append
            (map (λ (tps)
                   (map (λ (tp) (list-ref tp 0)) tps))
                 tpss))])
      (for/list ([tps (in-list tpss)])
        (for/list ([tp (in-list tps)])
          (define filename (list-ref tp 0))
          (define mp (list-ref tp 1))
          (list (path->string 
                 (or (shrink-path-wrt filename all-filenames)
                     (let-values ([(base name dir?) (split-path filename)])
                       name)))
                mp)))))
  
  (define already-installed-labels
    (for/list ([already-installed-teachpack (in-list already-installed-teachpacks)])
      (let/ec k
        (for ([label+mps (in-list label+mpss)])
          (for ([label+mp (in-list label+mps)])
            (when (equal? (list-ref label+mp 1) already-installed-teachpack)
              (k (list-ref label+mp 0)))))
        ;; shouldn't happen, but this will be a slightly graceful fallback, I hope
        (format "~s" already-installed-teachpack))))
  
  (define pre-installed-tpss 
    (for/list ([label+mps (in-list label+mpss)])
      (sort label+mps string<? #:key car)))
  
  (define dlg (new (frame:focus-table-mixin dialog%) 
                   [parent parent]
                   [label (string-constant drscheme)]))
  (define hp (new horizontal-panel% [parent dlg]))
  (define answer #f)
  (define compiling? #f)
  
  (define pre-installed-gbs (map (λ (tps label)
                                   (new group-box-panel%
                                        [label label]
                                        [parent hp]))
                                 tpss labels))
  (define user-installed-gb (new group-box-panel%
                                 [label (string-constant teachpack-user-installed)]
                                 [parent hp]))
  
  (define pre-installed-lbs
    (map (λ (pre-installed-gb pre-installed-tps)
           (define lb 
             (new list-box%
                  [label #f]
                  [choices (map (λ (x) (gui-utils:trim-string (list-ref x 0) 200))
                                pre-installed-tps)]
                  [stretchable-height #t]
                  [min-height 300]
                  [min-width 200]
                  [callback
                   (λ (this evt)
                     (case (send evt get-event-type)
                       [(list-box-dclick) 
                        (update-button-and-conflict)
                        (selected this)]
                       [else
                        (for ([x (in-list (cons user-installed-lb
                                                pre-installed-lbs))]
                              #:unless (eq? x this))
                          (clear-selection x))
                        (update-button-and-conflict)]))]
                  [parent pre-installed-gb]))
           (for ([i (in-naturals)]
                 [tp (in-list pre-installed-tps)])
             (send lb set-data i (list-ref tp 1)))
           lb)
         pre-installed-gbs
         pre-installed-tpss))
  
  (define user-installed-lb
    (new list-box%
         [label #f]
         [choices '()]
         [stretchable-height #t]
         [min-width 200]
         [callback
          (λ (x evt)
            (case (send evt get-event-type)
              [(list-box-dclick) 
               (update-button-and-conflict)
               (selected user-installed-lb)]
              [else
               (for ([pre-installed-lb (in-list pre-installed-lbs)])
                 (clear-selection pre-installed-lb))
               (update-button-and-conflict)]))]
         [parent user-installed-gb]))
  
  (define (selected lb)        
    (when (send lb get-selection)
      (when (and viable-action?
                 (not conflict-tp))
        (unless compiling?
          (set! answer (figure-out-answer))
          (send dlg show #f)))))
  
  (define (clear-selection lb)
    (for-each
     (λ (x) (send lb select x #f))
     (send lb get-selections)))
  
  (define add-button (new button%
                          [parent user-installed-gb]
                          [label (string-constant add-teachpack-to-list...)]
                          [callback (λ (x y) (install-teachpack))]))
  
  (define (install-teachpack)
    (let ([file (get-file (string-constant select-a-teachpack) dlg)])
      (when file
        (let-values ([(base name dir) (split-path file)])
          (let ([dest-file (build-path teachpack-installation-dir name)])
            (when (or (not (file-exists? dest-file))
                      (equal? 1
                              (message-box/custom
                               (string-constant drscheme)
                               (format
                                (string-constant teachpack-already-installed)
                                (path->string name))
                               (string-constant overwrite)
                               (string-constant cancel)
                               #f
                               dlg
                               '(default=2 caution))))
              (make-directory* teachpack-installation-dir)
              (when (file-exists? dest-file)
                (delete-file dest-file))
              (copy-file file dest-file)
              
              ;; compiling the teachpack should be the last thing in this GUI callback
              (compile-new-teachpack dest-file)))))))
  
  (define (compile-new-teachpack filename)
    (let-values ([(_1 short-name _2) (split-path filename)])
      (cond
        [(cannot-compile? filename)
         (post-compilation-gui-cleanup short-name)]
        [else
         (send compiling-message set-label
               (format (string-constant compiling-teachpack) 
                       (path->string short-name)))
         (starting-compilation)
         (let ([nc (make-custodian)]
               [exn #f])
           (let ([t 
                  (parameterize ([current-custodian nc])
                    (thread (λ () 
                              (with-handlers ((exn? (λ (x) (set! exn x))))
                                (parameterize ([current-namespace (make-base-namespace)])
                                  (with-module-reading-parameterization
                                   (lambda ()
                                     (compile-file filename))))))))])
             (thread
              (λ ()
                (thread-wait t)
                (queue-callback
                 (λ () 
                   (cond
                     [exn
                      (message-box (string-constant drscheme)
                                   (exn-message exn))
                      (delete-file filename)
                      (update-user-installed-lb)]
                     [else
                      (post-compilation-gui-cleanup short-name)])
                   (done-compilation)
                   (send compiling-message set-label "")))))))])))
  
  (define (post-compilation-gui-cleanup short-name)
    (update-user-installed-lb)
    (for ([pre-installed-lb (in-list pre-installed-lbs)])
      (clear-selection pre-installed-lb))
    (send user-installed-lb set-string-selection (path->string short-name)))
  
  (define (starting-compilation)
    (set! compiling? #t)
    (update-button-and-conflict)
    (send cancel-button enable #f))
  
  (define (done-compilation)
    (set! compiling? #f)
    (update-button-and-conflict)
    (send cancel-button enable #t))
  
  (define (update-user-installed-lb)
    (let ([files
           (if (directory-exists? teachpack-installation-dir)
               (map path->string 
                    (filter 
                     (λ (x) (file-exists? (build-path teachpack-installation-dir x)))
                     (directory-list teachpack-installation-dir)))
               '())])
      (send user-installed-lb set (sort files string<=?))))
  
  (define viable-action? #f)
  (define conflict-tp #f)
  
  (define (update-button-and-conflict)
    ;; figuring out if there is a conflict.
    (define-values (tp-req tp-label) (figure-out-answer/f))
    (set! conflict-tp #f)
    (define conflict-name #f)
    (define conflict-label #f)
    (when tp-req
      (let/ec k
        (for ([existing-tp (in-list already-installed-teachpacks)]
              [existing-tp-label (in-list already-installed-labels)])
          (unless (equal? existing-tp tp-req)
            (define conflict (teachpacks-conflict existing-tp tp-req))
            (when conflict
              (set! conflict-tp existing-tp)
              (set! conflict-name conflict)
              (set! conflict-label existing-tp-label)
              (k (void)))))))
    (cond
      [conflict-tp
       (send conflict-txt lock #f)
       (send conflict-txt begin-edit-sequence)
       (send conflict-txt erase)
       ;; answer must be non #f here
       ;; also conflict-message must be non #f, too
       (send conflict-txt insert 
             (format (string-constant teachpack-conflict)
                     conflict-label
                     tp-label
                     conflict-name))
       (send conflict-txt change-style red-sd 0 (send conflict-txt last-position))
       (send conflict-txt end-edit-sequence)
       (send conflict-txt lock #t)]
      [else
       (when conflict-txt
         (send conflict-txt lock #f)
         (send conflict-txt begin-edit-sequence)
         (send conflict-txt erase)
         (send conflict-txt end-edit-sequence)
         (send conflict-txt lock #t))])

    (set! viable-action?
          (or (pair? (send user-installed-lb get-selections))
              (ormap (λ (pre-installed-lb)
                       (pair? (send pre-installed-lb get-selections)))
                     pre-installed-lbs)))
        
    ;; updating buttons
    (send ok-button enable 
          (and viable-action? 
               (not compiling?)
               (not conflict-tp)))
    
    (send replace-button show (and viable-action? 
                                   (not compiling?)
                                   conflict-tp))
    (send replace-button set-label (if (and viable-action? conflict-tp)
                                       (format (string-constant remove-and-add-teachpack)
                                               conflict-label tp-label)
                                       "")))
    
  
  (define red-sd (new style-delta%))
  (send red-sd set-delta-foreground (make-object color% 200 0 0))
  
  (define conflict-txt 
    (and (not (null? already-installed-teachpacks))
         (let ([t (new text:hide-caret/selection%)])
           (send t lock #t)
           (send t auto-wrap #t)
           (send t set-autowrap-bitmap #f)
           t)))
  (define conflict-ed
    (and conflict-txt
         (new editor-canvas% 
              [parent dlg]
              [editor conflict-txt]
              [style '(auto-vscroll no-hscroll no-border transparent)]
              [line-count 2])))
  
  (define button-panel (new horizontal-panel% 
                            [parent dlg]
                            [alignment '(right center)]
                            [stretchable-height #f]))
  (define compiling-message (new message% 
                                 [parent button-panel]
                                 [label ""]
                                 [auto-resize #t]))
  (define replace-button 
    (new button% 
         [parent button-panel]
         [label ""]
         [stretchable-width #t]
         [callback (λ (x y)
                     (set! answer (figure-out-answer))
                     (send dlg show #f))]))
  (send replace-button show #f)
  (define-values (ok-button cancel-button)
    (gui-utils:ok/cancel-buttons button-panel
                                 (λ (b e)
                                   (set! answer (figure-out-answer))
                                   (send dlg show #f))
                                 (λ (b e) 
                                   (send dlg show #f))
                                 (string-constant ok) (string-constant cancel)))
  
  (define (figure-out-answer)
    (define-values (tp-req tp-label) (figure-out-answer/f))
    (or tp-req
        (error 'figure-out-answer "no selection!")))
  
  (define (figure-out-answer/f)
    (let/ec done
      (for ([pre-installed-lb (in-list pre-installed-lbs)]
            [tp-dir (in-list tp-dirs)])
        (define sel (send pre-installed-lb get-selection))
        (when sel
          (done (send pre-installed-lb get-data sel)
                (send pre-installed-lb get-string sel))))
      (when (send user-installed-lb get-selection)
        (define str (send user-installed-lb get-string
                          (send user-installed-lb get-selection)))
        (done `(lib ,str ,user-installed-teachpacks-collection)
              str))
      (done #f #f)))
  
  
  (send ok-button enable #f)
  (update-user-installed-lb)
  
  (send dlg show #t)
  (values conflict-tp answer))


(define (tp-dir->tps tp-sym)
  (filter
   values
   (for*/list ([dir (in-list (find-relevant-directories (list tp-sym)))]
               #:when (let ([inf (get-info/full dir)])
                        (and inf (inf tp-sym (λ () #f))))
               [file-or-dir (in-list
                             (let ([files ((get-info/full dir) tp-sym)])
                               (cond
                                 [(eq? files 'all)
                                  (for/list ([x (in-list (directory-list dir))]
                                             #:when
                                             (regexp-match #rx"[.](ss|scm|rkt)$"
                                                           (path->string x))
                                             #:unless
                                             (member (path->string x) '("info.rkt" "info.ss")))
                                    x)]
                                 [(list? files) files]
                                 [else '()])))])
     (let/ec k
       (unless (path? file-or-dir) (k #f))
       (define candidate (build-path dir file-or-dir))
       (unless (file-exists? candidate) (k #f))
       (define mp (path->module-path candidate))
       (when (path-string? mp) (k #f))
       (list candidate mp)))))


(define (teachpacks-conflict tp1 tp2)
  (define tp1-exports (get-exports tp1))
  (define tp2-exports (get-exports tp2))
  (define conflicts
    (sort (set->list (set-intersect tp1-exports tp2-exports))
          symbol<?))
  (if (null? conflicts)
      #f
      (car conflicts)))

(define (get-exports tp)
  (with-handlers ([exn:fail? (λ (x) (list->set '()))])
    (define-values (vars stx) (module-compiled-exports (get-module-code (resolve-module-path tp #f))))
    (set-union (phase0-exports vars) (phase0-exports stx))))

(define (phase0-exports which)
  (define a (assoc 0 which))
  (list->set (map car (if a (cdr a) '()))))

(module+ test
  (require rackunit)
  (check-equal?
   (teachpacks-conflict '(lib "teachpack/htdp/guess.rkt")
                        '(lib "teachpack/htdp/lkup-gui.rkt"))
   #f)
  
  (check-equal?
   (and (teachpacks-conflict '(lib "teachpack/2htdp/image.rkt")
                             '(lib "teachpack/htdp/image.rkt"))
        #t)
   #t))


(module+ main 
  (get-teachpack-from-user 
   #f 
   (list '(lib "teachpack/htdp/image.rkt"))))
