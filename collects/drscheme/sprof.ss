#lang scheme/base
(require scheme/gui/base
         framework
         scheme/class)

;; how long between samples
(define pause-time 0.1)

;; gui updates occur every 'update-frequency' samples
(define update-frequency 4)

(define (make-prod-thread get-threads update-gui)
  (thread (lambda ()
            (define traces-table (make-hash))
            (let loop ([i 0])
              (sleep pause-time)
              (let ([new-traces
                     (map (λ (t) (continuation-mark-set->context (continuation-marks t)))
                          (get-threads))])
                (for-each
                 (λ (trace)
                   (for-each
                    (λ (line)
                      (hash-set! traces-table line (cons trace (hash-ref traces-table line '()))))
                    trace))
                 new-traces)
                (cond
                  [(zero? i)
                   (update-gui traces-table)
                   (loop update-frequency)]
                  [else
                   (loop (- i 1))]))))))

(define (format-fn-name i)
  (let ([id (car i)]
        [src (cdr i)])
    (cond
      [id (format "~a" id)]
      [src
       (format "~a:~a~a"
               (cond
                 [(path? (srcloc-source src))
                  (let-values ([(base name dir?) (split-path (srcloc-source src))])
                    name)]
                 [else (srcloc-source src)])
               (if (srcloc-line src)
                   (format "~a:~a"
                           (srcloc-line src)
                           (srcloc-column src))
                   (srcloc-position src))
               (if id
                   (format ": ~a" id)
                   ""))]
      [else "???"])))

(define (insert-long-fn-name t i)
  (send t begin-edit-sequence)
  (send t erase)
  (let ([id (car i)]
        [src (cdr i)])
    (when src
      (send t insert
            (format "~a:~a"
                    (srcloc-source src)
                    (if (srcloc-line src)
                        (format "~a:~a"
                                (srcloc-line src)
                                (srcloc-column src))
                        (format ":~a" (srcloc-position src))))))
    (when (and id src)
      (send t insert "\n"))
    (when id
      (send t insert (format (format "~a" id))))
    (unless (or id src)
      (send t insert "???")))
  (send t end-edit-sequence))

(define (format-percentage n)
  (let ([trunc (floor (* n 100))])
    (format "~a%" (pad3 trunc))))

(define (pad3 n)
  (cond
    [(< n 10) (format "00~a" n)]
    [(< n 100) (format "0~a" n)]
    [else (format "~a" n)]))

(define cumulative-t%
  (class text:basic%
    (init-field open-button vp ec1 lp info-editor)
    (inherit begin-edit-sequence
             end-edit-sequence
             erase
             find-position 
             get-admin
             dc-location-to-editor-location
             position-paragraph
             insert
             last-position
             highlight-range
             last-paragraph
             lock)
    
    (define gui-display-data '())
    (define clicked-srcloc-pr #f)
    (define line-to-source (make-hasheq))

    (define clear-old-pr void)
    
    (define/override (on-event event)
      (cond
        [(send event button-up? 'left)
         (let ([admin (get-admin)])
           (when admin
             (let ([dc (send admin get-dc)])
               (let-values ([(x y) (dc-location-to-editor-location (send event get-x)
                                                                   (send event get-y))])
                 (let* ([loc (find-position x y)]
                        [para (position-paragraph loc)])
                   (set! clicked-srcloc-pr (and (<= 0 para (last-paragraph))
                                                (car (list-ref gui-display-data para))))
                   (update-gui-display))))))]
        [else (void)]))

    (define/public (set-gui-display-data/refresh traces-table)
      (set! gui-display-data
            (sort (hash-map traces-table (λ (k v) (cons k v)))
                  >
                  #:key (λ (x) (length (cdr x)))))
      (update-gui-display))
    
    (define/public (clear-clicked)
      (set! clicked-srcloc-pr #f)
      (update-gui-display))
    
    (define/private (update-gui-display)
      (lock #f)
      (begin-edit-sequence)
      (erase)
      (set! line-to-source (make-hasheq))
      (clear-old-pr)
      (set! clear-old-pr void)
      (let* ([denom-ht (make-hasheq)]
             [filtered-gui-display-data
              (map 
               (λ (pr)
                 (let ([id (car pr)]
                       [stacks (filter-stacks (cdr pr))])
                   (for-each (λ (stack) (hash-set! denom-ht stack #t)) stacks)
                   (cons id stacks)))
               gui-display-data)]
             [denom-count (hash-count denom-ht)])
        (let loop ([prs filtered-gui-display-data]
                   [first? #t]
                   [i 0])
          (cond
            [(null? prs) (void)]
            [else
             (let* ([pr (car prs)]
                    [fn (car pr)]
                    [count (length (cdr pr))])
               (cond
                 [(zero? count)
                  (loop (cdr prs) first? i)]
                 [else
                  (unless first? (insert "\n"))
                  (let ([before (last-position)])
                    (hash-set! line-to-source i pr)
                    (insert (format-percentage (/ count denom-count)))
                    (insert (format " ~a" (format-fn-name fn)))
                    (let ([after (last-position)])
                      (when (equal? (car pr) clicked-srcloc-pr)
                        (set! clear-old-pr (highlight-range before after "NavajoWhite")))))
                  (loop (cdr prs) #f (+ i 1))]))]))
        (lock #t)
        (end-edit-sequence)
        (update-info-editor clicked-srcloc-pr)
        (send open-button enable (and clicked-srcloc-pr (path? (srcloc-source (cdr clicked-srcloc-pr)))))))
    
    (define/private (filter-stacks stacks)
      (cond
        [(not clicked-srcloc-pr) stacks]
        [else
         (filter (λ (stack) (ormap (λ (stack-ent) (equal? clicked-srcloc-pr stack-ent))
                                   stack))
                 stacks)]))
         
    (define/public (open-current-pr)
      (when clicked-srcloc-pr
        (let ([src (cdr clicked-srcloc-pr)])
          (when (path? (srcloc-source src))
            (printf "open ~s\n" (srcloc-source src))
            (when (number? (srcloc-position src))
              (printf "go to ~s\n" (srcloc-position src)))))))
    
    (define/private (update-info-editor pr)
      (send vp change-children (λ (l) (if pr (list ec1 lp) (list ec1))))
      (when pr
        (insert-long-fn-name info-editor pr)))
    
    (super-new)))

(define (construct-gui f)
  (define info-editor (new text%))
  (define vp (new vertical-panel% [parent f]))
  (define ec1 (new editor-canvas%
                   [parent vp]
                   [min-height 800]
                   [min-width 400]))
  (define lp (new vertical-panel% [parent vp]))
  (define ec2 (new editor-canvas%
                   [parent lp]
                   [min-height 100]
                   [stretchable-height #f]
                   [editor info-editor]))
  (define bp (new horizontal-panel% [stretchable-height #f] [parent lp] [alignment '(center center)]))
  (define open-button (new button%
                           [parent bp]
                           [label "Open"]
                           [callback
                            (λ (x y)
                              (send cumulative-t open-current-pr))]))
  (define unlock (new button%
                      [label "Show All"]
                      [parent bp]
                      [callback
                       (λ (x y)
                         (send cumulative-t clear-clicked))]))
  (define cumulative-t (new cumulative-t% 
                            [open-button open-button]
                            [vp vp]
                            [ec1 ec1]
                            [lp lp]
                            [info-editor info-editor]))
  (send ec1 set-editor cumulative-t)
  (send vp change-children (λ (l) (list ec1)))
  (send cumulative-t hide-caret #t)
  (send cumulative-t lock #t)
  (send info-editor auto-wrap #t)
  (values vp cumulative-t))

;; running an example outside of drscheme
#;
(begin
  (define evt (make-eventspace))
  (define f (parameterize ([current-eventspace evt])
              (new frame% [label ""])))
  (define cumulative-t (construct-gui f))
  (send f show #t)
  
  (void (make-prod-thread (let ([t (current-thread)])
                            (λ () (list t)))
                          (λ (traces-table)
                            (parameterize ([current-eventspace evt])
                              (queue-callback
                               (λ () 
                                 (send cumulative-t set-gui-display-data/refresh traces-table)))))))
  
  (time (dynamic-require '(lib "scribblings/reference/reference.scrbl")
                         #f)))


(begin
  (require drscheme/tool
           scheme/unit)
  (provide tool@)
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)     
      (define (phase1) (void))
      (define (phase2) (void))
      
      #;
      (define-local-member-name
        )
      
      (define unit-frame-mixin
        (mixin (drscheme:unit:frame<%>) ()
          (define main-panel #f)
          (define sprof-main-panel #f)
          (define everything-else #f)
          (define cumulative-t #f)
          (define/override (make-root-area-container cls parent)
            (set! main-panel (super make-root-area-container horizontal-panel% parent))
            (set! everything-else (make-object cls main-panel))
            (set!-values (sprof-main-panel cumulative-t) (construct-gui main-panel))
            (send main-panel change-children (λ (l) (list everything-else)))
            everything-else)
          (super-new)))
      
      (define tab-mixin
        (mixin (drscheme:unit:tab<%>) ()
          (define prof-visible? #f)
          
          (super-new)))
      
      (define repl-mixin
        (mixin (drscheme:rep:text<%>) ()
          (super-new)))
       
      (drscheme:get/extend:extend-tab tab-mixin)
      (drscheme:get/extend:extend-interactions-text repl-mixin)
      (drscheme:get/extend:extend-unit-frame unit-frame-mixin))))
