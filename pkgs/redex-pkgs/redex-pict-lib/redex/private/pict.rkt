#lang racket/base
(require racket/contract
         racket/draw
         racket/class
         racket/match
         racket/pretty
         racket/set
         (only-in racket/list drop-right last partition add-between)
         
         texpict/mrpict
         texpict/utils
         
         redex/private/reduction-semantics
         redex/private/judgment-form
         redex/private/struct
         redex/private/loc-wrapper
         redex/private/matcher
         redex/private/arrow
         "core-layout.rkt")
(require (prefix-in lw/ct: redex/private/loc-wrapper-ct)
         (prefix-in lw/rt: redex/private/loc-wrapper-rt))

(require (for-syntax racket/base
                     redex/private/term-fn
                     syntax/parse))

(provide render-term 
         term->pict
         
         render-term/pretty-write
         term->pict/pretty-write
         
         to-lw/stx
         
         language->pict
         render-language
         render-language-nts
         
         reduction-relation->pict
         render-reduction-relation
         render-reduction-relation-rules
         
         relation->pict
         metafunction->pict
         metafunctions->pict
         judgment-form->pict
         
         render-relation
         render-metafunction
         render-metafunctions
         render-judgment-form
         
         basic-text
         
         default-style
         grammar-style
         paren-style
         label-style
         literal-style
         metafunction-style
         delimit-ellipsis-arguments?
         
         label-font-size
         default-font-size
         metafunction-font-size
         reduction-relation-rule-separation

         where-make-prefix-pict
         where-combine
         
         just-before
         just-after         
         
         non-terminal-gap-space
         rule-pict-style
         arrow-space
         label-space
         metafunction-pict-style
         metafunction-cases
         judgment-form-cases
         compact-vertical-min-width
         extend-language-show-union
         extend-language-show-extended-order
         set-arrow-pict!
         arrow->pict
         horizontal-bar-spacing
         relation-clauses-combine)

(provide/contract
 [linebreaks (parameter/c (or/c #f (listof boolean?)))])


;                                                                             
;                                                                             
;                                                                             
;                       ;;;;                       ;    ;;                    
;                       ;;;;                      ;;    ;;                    
;  ;;; ;;;   ;;;     ;;;;;;; ;;;; ;;;;   ;;;;;  ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;  ;;;;;   ;;;;;;;; ;;;; ;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;; ;;;; ;; ;;;;;;;;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;    ;;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;;    ;;;;;   ;;;;;;;;; ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;     ;;;;;;  ;;;;;;;; ;;;;;;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;;      ;;;;    ;;;;;;;  ;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                                             
;                                                                             
;                                                                             
;                                                               
;                                                               
;                                                               
;                  ;;;;              ;    ;;                    
;                  ;;;;             ;;    ;;                    
;  ;;; ;;;   ;;;   ;;;; ;;;;;;;   ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;  ;;;;;  ;;;; ;;;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;; ;;;; ;; ;;;;     ;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;    ;;;;;;; ;;;;  ;;;;;;;  ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;;    ;;;;;   ;;;; ;;  ;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;     ;;;;;; ;;;; ;;;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;;      ;;;;  ;;;;  ;; ;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                               
;                                                               
;                                                               

(define (do-reduction-relation->pict what rr style)
  (let ([rules (render-reduction-relation-rules)])
    ((rule-pict-style->proc style)
     (map (rr-lws->trees (language-nts (reduction-relation-lang rr)))
          (if rules
              (let ([ht (make-hash)])
                (for ([rp (in-list (reduction-relation-lws rr))]
                      [i (in-naturals)])
                  (hash-set! ht i rp)
                  (hash-set! ht (rule-pict-label rp) rp))
                (map (lambda (label)
                       (hash-ref ht (if (string? label)
                                        (string->symbol label)
                                        label)
                                 (lambda ()
                                   (error what
                                          "no rule found for ~a: ~e"
                                          (if (number? label) "index" "label")
                                          label))))
                     rules))
              (reduction-relation-lws rr))))))

(define (reduction-relation->pict rr #:style [style (rule-pict-style)]) 
  (do-reduction-relation->pict 'reduction-relation->pict rr style))

(define render-reduction-relation-rules (make-parameter #f))

(define (render-reduction-relation rr [filename #f]
                                   #:style [style (rule-pict-style)])
  (if filename
      (save-as-ps/pdf
       (λ () (do-reduction-relation->pict 'render-reduction-relation rr style))
       filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-reduction-relation->pict 'render-reduction-relation rr style))))

(define ((rr-lws->trees nts) rp)
  (let ([tp (λ (x) (lw->pict nts x))])
    (make-rule-pict (rule-pict-arrow rp)
                    (tp (rule-pict-lhs rp))
                    (tp (rule-pict-rhs rp))
                    (rule-pict-label rp)
                    (and (rule-pict-computed-label rp)
                         (let ([rewritten (apply-rewrites (rule-pict-computed-label rp))])
                           (and (not (and (rule-pict-label rp)
                                          (let has-unq? ([x rewritten])
                                            (and (lw? x)
                                                 (or (lw-unq? x)
                                                     (and (list? (lw-e x))
                                                          (ormap has-unq? (lw-e x))))))))
                                (tp rewritten))))
                    (map (lambda (v) 
                           (if (pair? v)
                               (where-pict (tp (car v)) (tp (cdr v)))
                               (tp v)))
                         (rule-pict-side-conditions/pattern-binds rp))
                    (map tp (rule-pict-fresh-vars rp)))))

(define current-label-extra-space (make-parameter 0))
(define reduction-relation-rule-separation (make-parameter 4))

(define ((rule-picts->pict/horizontal left-column-align side-conditions-same-line?) rps)
  (let* ([sep 2]
         [max-rhs (apply max
                         0
                         (map pict-width
                              (map rule-pict-rhs rps)))]
         [max-w (apply max
                       0
                       (map (lambda (rp)
                              (+ sep sep
                                 (pict-width (rule-pict-lhs rp))
                                 (pict-width (arrow->pict (rule-pict-arrow rp)))
                                 (pict-width (rule-pict-rhs rp))))
                            rps))])
    (table 4
           (apply
            append
            (let ([len (length rps)])
              (for/list ([rp (in-list rps)]
                         [i (in-naturals 1)])
                (let ([arrow (hbl-append (blank (arrow-space) 0)
                                         (arrow->pict (rule-pict-arrow rp))
                                         (blank (arrow-space) 0))]
                      [lhs (rule-pict-lhs rp)]
                      [rhs (rule-pict-rhs rp)]
                      [spc (basic-text " " (default-style))]
                      [label (hbl-append (blank (label-space) 0) (rp->pict-label rp))]
                      [sep (blank 4)])
                  (append 
                   (if side-conditions-same-line?
                       (list lhs arrow 
                             (hbl-append rhs
                                         (let ([sc (rp->side-condition-pict rp max-w)])
                                           (inset sc (min 0 (- max-rhs (pict-width sc))) 0 0 0)))
                             label)
                       (list lhs arrow rhs label
                             (blank) (blank)
                             (let ([sc (rp->side-condition-pict rp max-w)])
                               (inset sc (min 0 (- max-rhs (pict-width sc))) 0 0 0))
                             (blank)))
                   (if (= len i)
                       '()
                       (list sep (blank) (blank) (blank))))))))
           (list* left-column-align ctl-superimpose ltl-superimpose)
           (list* left-column-align ctl-superimpose ltl-superimpose)
           (list* sep sep (+ sep (current-label-extra-space))) 2)))

(define arrow-space (make-parameter 0))
(define label-space (make-parameter 0))

(define ((make-vertical-style side-condition-combiner) rps)
  (let* ([mk-top-line-spacer
          (λ (rp)
            (hbl-append (rule-pict-lhs rp)
                        (basic-text " " (default-style))
                        (arrow->pict (rule-pict-arrow rp))
                        (basic-text " " (default-style))
                        (rp->pict-label rp)))]
         [mk-bot-line-spacer
          (λ (rp)
            (rt-superimpose
             (rule-pict-rhs rp)
             (rp->side-condition-pict rp +inf.0)))]
         [multi-line-spacer
          (if (null? rps)
              (blank)
              (ghost
               (launder
                (ctl-superimpose 
                 (apply ctl-superimpose (map mk-top-line-spacer rps))
                 (apply ctl-superimpose (map mk-bot-line-spacer rps))))))]
         [spacer (dc void 
                     (pict-width multi-line-spacer)
                     (pict-descent multi-line-spacer) ;; probably could be zero ...
                     0
                     (pict-descent multi-line-spacer))])
    (if (null? rps)
        (blank)
        (apply
         vl-append
         (add-between
          (map (λ (rp)
                 (side-condition-combiner
                  (vl-append
                   (ltl-superimpose 
                    (htl-append (rule-pict-lhs rp)
                                (basic-text " " (default-style))
                                (arrow->pict (rule-pict-arrow rp)))
                    (rtl-superimpose 
                     spacer
                     (rp->pict-label rp)))
                   (rule-pict-rhs rp))
                  (rp->side-condition-pict rp +inf.0)))
               rps)
          (blank 0 (reduction-relation-rule-separation)))))))

(define compact-vertical-min-width (make-parameter 0))

(define rule-picts->pict/vertical 
  (make-vertical-style vr-append))

(define rule-picts->pict/vertical-overlapping-side-conditions
  (make-vertical-style rbl-superimpose))

(define (rule-picts->pict/compact-vertical rps)
  (let* ([max-w (apply max
                       (compact-vertical-min-width)
                       (map pict-width
                            (append
                             (map rule-pict-lhs rps)
                             (map rule-pict-rhs rps))))]
         [scs (map (lambda (rp)
                     (rp->side-condition-pict rp max-w))
                   rps)]
         [labels (map (lambda (rp)
                        (hbl-append (blank (label-space) 0) (rp->pict-label rp)))
                      rps)]
         [total-w (apply max
                         max-w
                         (append (map pict-width scs)
                                 (map (lambda (lbl)
                                        (+ max-w 2 (label-space) (pict-width lbl)))
                                      labels)))]
         [one-line
          (lambda (sep?)
            (lambda (rp sc label)
              (let ([arrow (hbl-append (arrow->pict (rule-pict-arrow rp)) (blank (arrow-space) 0))]
                    [lhs (rule-pict-lhs rp)]
                    [rhs (rule-pict-rhs rp)]
                    [spc (basic-text " " (default-style))]
                    [sep (blank (compact-vertical-min-width)
                                (reduction-relation-rule-separation))]
                    [add-label (lambda (p label)
                                 (htl-append 
                                  p
                                  (inset label (- total-w (pict-width p) (pict-width label))
                                         0 0 0)))])
                (append
                 (if ((apply + (map pict-width (list lhs spc arrow spc rhs)))
                      . < .
                      max-w)
                     (list 
                      (blank) (add-label (hbl-append lhs spc arrow spc rhs) label)
                      (blank) sc)
                     (list (blank) (add-label lhs label)
                           arrow rhs
                           (blank) sc))
                 (if sep? (list (blank) sep) null)))))])
    (if (null? rps)
        (blank)
        (table 2
               (append
                (apply
                 append
                 (map (one-line #t)
                      (drop-right rps 1) 
                      (drop-right scs 1)
                      (drop-right labels 1)))
                ((one-line #f) (last rps) (last scs) (last labels)))
               ltl-superimpose ltl-superimpose
               2 2))))

;; side-condition-pict : (listof pict) (listof pict) number -> pict
(define (side-condition-pict fresh-vars pattern-binds/sc max-w)
  (let* ([frsh 
          (if (null? fresh-vars)
              null
              (list
               (hbl-append
                (apply 
                 hbl-append
                 (add-between
                  fresh-vars
                  (basic-text ", " (default-style))))
                (basic-text " fresh" (default-style)))))]
         [lst (add-between
               (append
                pattern-binds/sc
                frsh)
               'comma)])
    (if (null? lst)
        (blank)
        (let ([where ((where-make-prefix-pict))])
          (let ([max-w (- max-w (pict-width where))])
            (htl-append where
                        (let loop ([p (car lst)][lst (cdr lst)])
                          (cond
                            [(null? lst) p]
                            [(eq? (car lst) 'comma)
                             (loop (htl-append p (basic-text ", " (default-style)))
                                   (cdr lst))]
                            [((+ (pict-width p) (pict-width (car lst))) . > . max-w)
                             (vl-append p
                                        (loop (car lst) (cdr lst)))]
                            [else (loop (htl-append p (car lst)) (cdr lst))]))))))))

(define where-make-prefix-pict
  (make-parameter (lambda ()
                    (basic-text " where " (default-style)))))

(define (where-pict lhs rhs)
  ((where-combine) lhs rhs))

(define where-combine
  (make-parameter (lambda (lhs rhs)
                    (htl-append lhs (make-=) rhs))))

(define (rp->side-condition-pict rp max-w)
  (side-condition-pict (rule-pict-fresh-vars rp)
                       (rule-pict-side-conditions/pattern-binds rp)
                       max-w))

(define (rp->pict-label rp)
  (cond [(rule-pict-computed-label rp) => bracket]
        [(rule-pict-label rp)
         (string->bracketed-label 
          (format "~a" (rule-pict-label rp)))]
        [else (blank)]))

(define (string->bracketed-label str)
  (define m (regexp-match #rx"^([^_]*)(?:_([^_]*)|)$" str))
  (bracket
   (hbl-append
    ((current-text) (cadr m) (label-style) (label-font-size))
    (if (caddr m)
        ((current-text) (caddr m) `(subscript . ,(label-style)) (label-font-size))
        (blank)))))

(define (bracket label)
  (hbl-append
   ((current-text) " [" (label-style) (label-font-size))
   label
   ((current-text) "]" (label-style) (label-font-size))))

(define (make-horiz-space picts) (blank (pict-width (apply cc-superimpose picts)) 0))

(define rule-pict-style (make-parameter 'vertical))
(define (rule-pict-style->proc style)
  (case style
    [(vertical) rule-picts->pict/vertical]
    [(compact-vertical) rule-picts->pict/compact-vertical]
    [(vertical-overlapping-side-conditions)
     rule-picts->pict/vertical-overlapping-side-conditions]
    [(horizontal-left-align)
     (rule-picts->pict/horizontal ltl-superimpose #f)]
    [(horizontal-side-conditions-same-line)
     (rule-picts->pict/horizontal rtl-superimpose #t)]
    [else ;; horizontal
     (rule-picts->pict/horizontal rtl-superimpose #f)]))

(define (mk-arrow-pict sz style)
  (let ([cache (make-hash)])
    (lambda ()
      (let ([s (default-font-size)])
        ((hash-ref cache s
                   (lambda ()
                     (let ([f (make-arrow-pict sz style 'roman s)])
                       (hash-set! cache s f)
                       f))))))))

(define long-arrow-pict (mk-arrow-pict "xxx" 'straight))
(define short-arrow-pict (mk-arrow-pict "m" 'straight))
(define curvy-arrow-pict (mk-arrow-pict "xxx" 'curvy))
(define short-curvy-arrow-pict (mk-arrow-pict "m" 'curvy))
(define double-arrow-pict (mk-arrow-pict "xxx" 'straight-double))
(define short-double-arrow-pict (mk-arrow-pict "m" 'straight-double))
(define map-arrow-pict (mk-arrow-pict "m" 'map))
(define long-map-arrow-pict (mk-arrow-pict "xxx" 'map))

(define user-arrow-table (make-hasheq))
(define (set-arrow-pict! arr thunk)
  (hash-set! user-arrow-table arr thunk))

(define (arrow->pict arr)
  (let ([ut (hash-ref user-arrow-table arr #f)])
    (if ut
        (ut)
        (case arr
          [(--> -+>) (long-arrow-pict)]
          [(==>) (double-arrow-pict)]
          [(->) (short-arrow-pict)]
          [(=>) (short-double-arrow-pict)]
          [(..>) (basic-text "\u21E2" (default-style))]
          [(>->) (basic-text "\u21a3" (default-style))]
          [(~~>) (curvy-arrow-pict)]
          [(~>) (short-curvy-arrow-pict)]
          [(:->) 
           (if STIX?
               (basic-text "\u21a6" (default-style))
               (map-arrow-pict))]
          [(:-->) 
           (if STIX?
               (basic-text "\u27fc" (default-style))
               (long-map-arrow-pict))]
          [(c->) (basic-text "\u21aa" (default-style))]
          [(-->>) (basic-text "\u21a0" (default-style))]
          [(>--) (basic-text "\u291a" (default-style))]
          [(--<) (basic-text "\u2919" (default-style))]
          [(>>--) (basic-text "\u291c" (default-style))]
          [(--<<) (basic-text "\u291b" (default-style))]
          [else (error 'arrow->pict "unknown arrow ~s" arr)]))))



;                                                                       
;                                                                       
;                                                                       
;  ;;;;                                                                 
;  ;;;;                                                                 
;  ;;;; ;;;;;;;  ;;;; ;;;   ;;;;;;; ;;;; ;;;; ;;;;;;;   ;;;;;;;   ;;;   
;  ;;;; ;;;;;;;; ;;;;;;;;; ;;;;;;;; ;;;; ;;;; ;;;;;;;; ;;;;;;;;  ;;;;;  
;  ;;;;     ;;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;     ;;;; ;;; ;;;; ;;;; ;; 
;  ;;;;  ;;;;;;; ;;;; ;;;; ;;;;;;;; ;;;; ;;;;  ;;;;;;; ;;;;;;;; ;;;;;;; 
;  ;;;; ;;  ;;;; ;;;; ;;;;  ;;;;;;; ;;;; ;;;; ;;  ;;;;  ;;;;;;; ;;;;;   
;  ;;;; ;;;;;;;; ;;;; ;;;; ;   ;;;; ;;;;;;;;; ;;;;;;;; ;   ;;;;  ;;;;;; 
;  ;;;;  ;; ;;;; ;;;; ;;;; ;;;;;;;;  ;;; ;;;;  ;; ;;;; ;;;;;;;;   ;;;;  
;                          ;;;;;;;;                    ;;;;;;;;         
;                           ;;;;;;                      ;;;;;;          
;                                                                       

;; type flattened-language-pict-info =
;;   (listof (cons (listof symbol[nt]) (listof loc-wrapper[rhs])))
;; type language-pict-info = 
;;  (union (vector flattened-language-pict-info language-pict-info) 
;;         flattened-language-pict-info)

(define (render-language lang [filename #f] #:nts [nts (render-language-nts)])
  (if filename
      (save-as-ps/pdf (λ () (do-language->pict 'render-language lang nts)) filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-language->pict 'render-language lang nts))))

(define (language->pict lang #:nts [nts (render-language-nts)])
  (do-language->pict 'language->pict lang nts))

(define (do-language->pict what lang specd-non-terminals)
  (unless (compiled-lang-pict-builder lang)
    (error what "cannot render the result of define-union-language"))
  (let ([all-non-terminals (hash-map (compiled-lang-ht lang) (λ (x y) x))])
    (when specd-non-terminals
      (check-non-terminals what specd-non-terminals lang))
    (make-grammar-pict (compiled-lang-pict-builder lang) 
                       (or specd-non-terminals all-non-terminals)
                       all-non-terminals)))

(define render-language-nts (make-parameter #f))

(define (check-non-terminals what nts lang)
  (let ([langs-nts (language-nts lang)])
    (for-each
     (λ (nt) 
       (unless (memq nt langs-nts)
         (error what 
                "the non-terminal ~s is not one of the language's nonterminals (~a)"
                nt
                (if (null? langs-nts)
                    "it has no non-terminals"
                    (apply
                     string-append
                     "which are: "
                     (format "~a" (car langs-nts))
                     (map (λ (x) (format " ~a" x)) (cdr langs-nts)))))))
     nts)))

;; save-as-ps/pdf : (-> pict) path-string -> void
(define (save-as-ps/pdf mk-pict filename) 
  (let ([ps/pdf-dc (make-ps/pdf-dc filename)])
    (parameterize ([dc-for-text-size ps/pdf-dc])
      (send ps/pdf-dc start-doc "x")
      (send ps/pdf-dc start-page)
      (draw-pict (mk-pict) ps/pdf-dc 0 0)
      (send ps/pdf-dc end-page)
      (send ps/pdf-dc end-doc))))

(define (make-ps/pdf-dc filename)
  (let ([ps-setup (make-object ps-setup%)])
    (send ps-setup copy-from (current-ps-setup))
    (send ps-setup set-file filename)
    (send ps-setup set-mode 'file)
    (define is-pdf? 
      (cond
        [(path? filename) (regexp-match #rx#"[.]pdf$" (path->bytes filename))]
        [else (regexp-match #rx"[.]pdf$" filename)]))
    (define % (if is-pdf? pdf-dc% post-script-dc%))
    (parameterize ([current-ps-setup ps-setup])
      (make-object % #f #f))))

(define non-terminal-gap-space (make-parameter 0))

;; raw-info : language-pict-info
;; nts : (listof symbol) -- the nts that the user expects to see
(define (make-grammar-pict raw-info nts all-nts)
  (let* ([info (remove-unwanted-nts nts (flatten-grammar-info raw-info all-nts nts))]
         [term-space 
          (launder
           (ghost
            (apply cc-superimpose (map (λ (x) (sequence-of-non-terminals (car x)))
                                       info))))])
    (apply vl-append
           (non-terminal-gap-space)
           (map (λ (line)
                  (htl-append 
                   (rc-superimpose term-space (sequence-of-non-terminals (car line)))
                   (lw->pict
                    all-nts
                    (find-enclosing-loc-wrapper (add-bars-and-::= (cdr line))))))
                info))))

(define (sequence-of-non-terminals nts)
  (let ([draw-nt (lambda (nt)
                   (lw->pict nts (build-lw nt 0 0 0 0)))])
    (let loop ([nts (cdr nts)]
               [pict (draw-nt (car nts))])
      (cond
       [(null? nts) pict]
       [else 
        (loop (cdr nts)
              (hbl-append pict 
                          (non-terminal ", ")
                          (draw-nt (car nts))))]))))


(define extend-language-show-union (make-parameter #f))
(define extend-language-show-extended-order (make-parameter #f))

;; remove-unwanted-nts : (listof symbol) flattened-language-pict-info -> flattened-language-pict-info
(define (remove-unwanted-nts nts info)
  (filter (λ (x) (not (null? (car x))))
          (map
           (λ (x) (cons (filter (λ (x) (member x nts)) (car x))
                        (cdr x)))
           info)))


;; flatten-grammar-info : language-pict-info (listof symbol) -> flattened-language-pict-info
(define (flatten-grammar-info info all-nts wanted-nts)
  (define (merge-line nt extension orig-line)
    (cond
     [(and extension orig-line)
      (let ([rhss (cdr extension)])
        (cons nt
              (map (λ (x)
                     (if (and (lw? x) (eq? '.... (lw-e x)))
                         (struct-copy lw
                                      x
                                      [e
                                       (lw->pict all-nts
                                                 (find-enclosing-loc-wrapper
                                                  (add-bars (cdr orig-line))))])
                         x))
                   (cdr extension))))]
     [extension extension]
     [else orig-line]))
  (let ([union? (extend-language-show-union)]
        [ext-order? (extend-language-show-extended-order)])
    (let loop ([info info])
      (cond
        [(vector? info) 
         (let ([orig (loop (vector-ref info 0))]
               [extensions (vector-ref info 1)])
           (if union?
               (cond
                [(not ext-order?)
                 ;; Use original order, adding extra extensions after:
                 (define orig-nts (list->set (map car orig)))
                 (append
                  (map (λ (orig-line)
                         (define nt (car orig-line))
                         (merge-line nt (assoc nt extensions) orig-line))
                       orig)
                  (filter (lambda (extension) (not (set-member? orig-nts (car extension))))
                          extensions))]
                [else
                 ;; Use extension order, adding any extra originals after:
                 (define ext-nts (list->set (map car extensions)))
                 (append
                  (map (λ (extension)
                         (define nt (car extension))
                         (merge-line nt extension (assoc nt orig)))
                       extensions)
                  (filter (lambda (orig-line) (not (set-member? ext-nts (car orig-line))))
                          orig))])
               extensions))]
        [else info]))))

(define (make-::=) (basic-text " ::= " (grammar-style)))
(define (make-bar) 
  (basic-text " | " (grammar-style))
  #;
  (let ([p (basic-text " | " (grammar-style))])
    (dc 
     (λ (dc dx dy)
       (cond
         [(is-a? dc post-script-dc%)
          (let ([old-pen (send dc get-pen)])
            (send dc set-pen "black" .6 'solid)
            (send dc draw-line 
                  (+ dx (/ (pict-width p) 2)) dy
                  (+ dx (/ (pict-width p) 2)) (+ dy (pict-height p)))
            (send dc set-pen old-pen))]
         [else
          (send dc draw-text " | " dx dy)]))
     (pict-width p)
     (pict-height p)
     (pict-ascent p)
     (pict-descent p))))

(define (add-bars-and-::= lst)
  (cond
    [(null? lst) null]
    [else
     (cons
      (let ([fst (car lst)])
        (build-lw
         (rc-superimpose (ghost (make-bar)) (make-::=))
         (lw-line fst)
         (lw-line-span fst)
         (lw-column fst)
         0))
      (let loop ([fst (car lst)]
                 [rst (cdr lst)])
        (cond
          [(null? rst) (list fst)]
          [else 
           (let* ([snd (car rst)]
                  [bar 
                   (cond
                     [(= (lw-line snd)
                         (lw-line fst))
                      (let* ([line (lw-line snd)]
                             [line-span (lw-line-span snd)]
                             [column (+ (lw-column fst)
                                        (lw-column-span fst))]
                             [column-span
                              (max (- (lw-column snd)
                                      (+ (lw-column fst)
                                         (lw-column-span fst)))
                                   0)])
                        (build-lw (make-bar) line line-span column column-span))]
                     [else
                      (build-lw
                       (rc-superimpose (make-bar) (ghost (make-::=)))
                       (lw-line snd)
                       (lw-line-span snd)
                       (lw-column snd)
                       0)])])
             (list* fst
                    bar
                    (loop snd (cdr rst))))])))]))

(define (add-bars lst)
  (let loop ([fst (car lst)]
             [rst (cdr lst)])
    (cond
      [(null? rst) (list fst)]
      [else 
       (let* ([snd (car rst)]
              [bar 
               (cond
                 [(= (lw-line snd)
                     (lw-line fst))
                  (let* ([line (lw-line snd)]
                         [line-span (lw-line-span snd)]
                         [column (+ (lw-column fst)
                                    (lw-column-span fst))]
                         [column-span
                          (- (lw-column snd)
                             (+ (lw-column fst)
                                (lw-column-span fst)))])
                    (build-lw (make-bar) line line-span column column-span))]
                 [else
                  (build-lw
                   (make-bar)
                   (lw-line snd)
                   (lw-line-span snd)
                   (lw-column snd)
                   0)])])
         (list* fst
                bar
                (loop snd (cdr rst))))])))


;                                       
;                                       
;                                       
;                                       
;                        ;              
;                      ;;;              
;  ;;; ;; ;;;    ;;;;  ;;;;  ;;;;;      
;  ;;;;;;;;;;;  ;; ;;; ;;;; ;;;;;;;     
;  ;;; ;;; ;;; ;;; ;;; ;;;  ;;  ;;;     
;  ;;; ;;; ;;; ;;;;;;; ;;;    ;;;;; ;;;;
;  ;;; ;;; ;;; ;;;     ;;;  ;;; ;;; ;;;;
;  ;;; ;;; ;;;  ;;;;;; ;;;; ;;; ;;;     
;  ;;; ;;; ;;;   ;;;;   ;;;  ;;;;;;     
;                                       
;                                       
;                                       
;                                       
;                                                        
;                                                        
;                                                        
;                                                        
;   ;;;;                          ;  ;;;                 
;  ;;;                          ;;;                      
;  ;;;; ;;; ;;; ;;; ;;    ;;;   ;;;; ;;;   ;;;   ;;; ;;  
;  ;;;; ;;; ;;; ;;;;;;;  ;;;;;  ;;;; ;;;  ;;;;;  ;;;;;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;; ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;;     ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;; ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;;;;;; ;;; ;;;  ;;;;;  ;;;; ;;;  ;;;;;  ;;; ;;; 
;  ;;;   ;; ;;; ;;; ;;;   ;;;    ;;; ;;;   ;;;   ;;; ;;; 
;                                                        
;                                                        
;                                                        
;                                                        


(define (make-=) (basic-text " = " (default-style)))

(define-syntax (metafunction->pict stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     #'(metafunctions->pict name)]))

(define-syntax (metafunctions->pict stx)
  (syntax-parse stx 
    [(_ name1:id name2:id ... 
        (~optional (~seq #:contract? contract-e:expr) #:defaults ([contract-e #'#f])))
     #'(metafunctions->pict/proc (list (metafunction name1) (metafunction name2) ...)
                                 contract-e
                                 'metafunctions->pict)]))

(define-syntax (relation->pict stx)
  (syntax-case stx ()
    [(form name1)
     (identifier? #'name1)
     #'(inference-rules-pict/relation 'form (metafunction name1))]))

(define-syntax (render-metafunctions stx)
  (syntax-parse stx
    [(_ name1:id name2:id ... (~seq k:keyword e:expr) ...)
     (define filename #'#f)
     (define contract? #'#f)
     (for ([kwd (in-list (syntax->list #'(k ...)))]
           [e (in-list (syntax->list #'(e ...)))])
       (cond
         [(equal? '#:filename (syntax-e kwd))
          (set! filename e)]
         [(equal? '#:contract? (syntax-e kwd))
          (set! contract? e)]
         [else
          (raise-syntax-error #f "unexpected keyword" stx kwd)]))
     #`(render-metafunction/proc 
        (list (metafunction name1) (metafunction name2) ...)
        #,contract?
        #,filename
        'render-metafunctions)]))

(define-syntax (render-metafunction stx)
  (syntax-parse stx
    [(_ name:id
        (~optional file:expr #:defaults ([file #'#f]))
        (~optional (~seq k:keyword e:expr)))
     #`(render-metafunction/proc (list (metafunction name))
                                 #,(cond
                                     [(not (attribute k)) #'#f]
                                     [(and (equal? (syntax-e (attribute k)) '#:contract?))
                                      #'e]
                                     [else
                                      (raise-syntax-error #f "unknown keyword" stx #'k)])
                                 file
                                 'render-metafunction)]))

(define-syntax (render-relation stx)
  (syntax-case stx ()
    [(form rest ...)
     #'(render-judgment-form rest ...)]))
               
(define linebreaks (make-parameter #f))

(define metafunction-pict-style (make-parameter 'left-right))
(define metafunction-cases (make-parameter #f))
(define (select-mf-cases contracts eqns case-labelss)
  (define mf-cases (metafunction-cases))
  (cond
    [mf-cases
     (define i 0)
     (define sorted-cases (remove-dups (sort (filter number? mf-cases) <)))
     (define named-cases (apply set (filter string? mf-cases)))
     (apply 
      append
      (for/list ([eqns (in-list eqns)]
                 [contract (in-list contracts)]
                 [case-labels (in-list case-labelss)])
        (filter
         values
         (cons 
          contract
          (for/list ([eqn (in-list eqns)]
                     [case-label (in-list case-labels)])
            (begin0
              (cond
                [(and (pair? sorted-cases) (= i (car sorted-cases)))
                 (set! sorted-cases (cdr sorted-cases))
                 eqn]
                [(set-member? named-cases case-label)
                 eqn]
                [else #f])
              (set! i (+ i 1))))))))]
    [else (apply append 
                 (for/list ([eqns (in-list eqns)]
                            [contract (in-list contracts)])
                   (if contract
                       (cons contract eqns)
                       eqns)))]))

(define judgment-form-cases (make-parameter #f))
(define (select-jf-cases eqns conclusions eqn-names)
  (define cases
    (or (judgment-form-cases)
        (and (metafunction-cases)
             (filter number? (metafunction-cases)))))
  (cond
    [cases
     (define-values (rev-eqns rev-concs rev-eqn-names)
       (for/fold ([eqns '()]
                  [concs '()]
                  [eqn-names '()])
                 ([eqn (in-list eqns)]
                  [conc (in-list conclusions)]
                  [eqn-name (in-list eqn-names)]
                  [i (in-naturals)])
         (if (or (member i cases)
                 (member eqn-name cases))
             (values (cons eqn eqns)
                     (cons conc concs)
                     (cons eqn-name eqn-names))
             (values eqns concs eqn-names))))
      (values (reverse rev-eqns) (reverse rev-concs) (reverse rev-eqn-names))]
    [else 
     (values eqns conclusions eqn-names)]))

;; remove-dups : (listof number)[sorted] -> (listof number)[sorted]
;; removes duplicate numbers from 'l'
(define (remove-dups l)
  (let loop ([l l])
    (cond
      [(null? (cdr l)) l]
      [(= (car l) (cadr l))
       (loop (cdr l))]
      [else
       (cons (car l) (loop (cdr l)))])))

(define (metafunctions->pict/proc mfs contract? name)
  (unless (andmap (λ (mf) (equal? (metafunc-proc-lang (metafunction-proc (car mfs)))
                                  (metafunc-proc-lang (metafunction-proc mf))))
                  mfs)
    (error name "expected metafunctions that are all drawn from the same language"))
  (define current-linebreaks (linebreaks))
  (define all-nts (language-nts (metafunc-proc-lang (metafunction-proc (car mfs)))))
  (define sep 2)
  (define style (metafunction-pict-style))
  (define (wrapper->pict lw) (lw->pict all-nts lw))
  (define contracts (for/list ([mf (in-list mfs)]) 
                      (define lws (list-ref (metafunc-proc-pict-info (metafunction-proc mf)) 0))
                      (cond
                        [(and contract? lws)
                         (define doms (list-ref lws 0))
                         (define rngs (list-ref lws 1))
                         (define separators (list-ref lws 2))
                         (render-metafunction-contract
                          (metafunc-proc-lang (metafunction-proc mf))
                          (metafunc-proc-name (metafunction-proc mf))
                          doms
                          rngs
                          separators)]
                        [else #f])))
  (define all-eqns (map (λ (mf) (list-ref (metafunc-proc-pict-info (metafunction-proc mf)) 1)) mfs))
  (define all-lhss 
    (for/list ([mf (in-list mfs)])
      (for/list ([eqn (in-list (list-ref (metafunc-proc-pict-info (metafunction-proc mf)) 1))])
        (wrapper->pict
         (metafunction-call (metafunc-proc-name (metafunction-proc mf))
                            (list-ref eqn 0))))))
  (define case-labels (map (λ (mf) (metafunc-proc-clause-names (metafunction-proc mf))) mfs))
  (define eqns (select-mf-cases contracts all-eqns case-labels))
  (define lhs/contracts (select-mf-cases contracts all-lhss case-labels))
  (define rhss (for/list ([eqn/contract (in-list eqns)])
                 (if (pict? eqn/contract)
                     'contract
                     (wrapper->pict (list-ref eqn/contract 2)))))
  (unless (or (not current-linebreaks)
              (= (length current-linebreaks) (length eqns)))
    (error 'metafunction->pict
           (string-append
            "expected the current-linebreaks parameter to be a list"
            " whose length matches the number of cases in the metafunction"
            " plus one if there is a contract (~a), but got ~s")
           (length eqns)
           current-linebreaks))
  (define linebreak-list (or current-linebreaks
                             (map (lambda (x) #f) eqns)))
  (define mode (case style
                 [(left-right 
                   left-right/vertical-side-conditions
                   left-right/compact-side-conditions
                   left-right/beside-side-conditions)
                  'horizontal]
                 [(up-down
                   up-down/vertical-side-conditions
                   up-down/compact-side-conditions)
                  'vertical]
                 [else (error 'metafunctions->pict "unknown mode")]))
  (define =-pict (make-=))
  (define vertical-side-conditions?
    (memq style '(up-down/vertical-side-conditions
                  left-right/vertical-side-conditions
                  left-right*/vertical-side-conditions)))
  (define compact-side-conditions? 
    (memq style '(up-down/compact-side-conditions
                  left-right/compact-side-conditions
                  left-right*/compact-side-conditions)))
  (define max-line-w/pre-sc (and
                             compact-side-conditions?
                             (for/fold ([biggest 0]) ([lhs/contract (in-list lhs/contracts)]
                                                      [rhs (in-list rhss)]
                                                      [linebreak? (in-list linebreak-list)])
                               (cond
                                 [(equal? rhs 'contract)
                                  ;; this is a contract
                                  (max biggest (pict-width lhs/contract))]
                                 [(eq? mode 'vertical)
                                  (max biggest
                                       (+ (pict-width lhs/contract) (pict-width =-pict))
                                       (pict-width rhs))]
                                 [linebreak?
                                  (max biggest
                                       (pict-width lhs/contract)
                                       (+ (pict-width rhs) sep (pict-width =-pict)))]
                                 [else
                                  (max biggest
                                       (+ (pict-width lhs/contract)
                                          (pict-width rhs)
                                          (pict-width =-pict)
                                          (* 2 sep)))]))))
  (define scs (for/list ([eqn (in-list eqns)])
                (cond
                  [(pict? eqn) #f]
                  [else
                   (define scs (reverse (list-ref eqn 1)))
                   (cond
                     [(null? scs) #f]
                     [else
                      (define-values (fresh where/sc) (partition metafunc-extra-fresh? scs))
                      (side-condition-pict 
                       (foldl (λ (clause picts) 
                                (foldr (λ (l ps) (cons (wrapper->pict l) ps))
                                       picts (metafunc-extra-fresh-vars clause)))
                              '() fresh)
                       (map (match-lambda
                              [(struct metafunc-extra-where (lhs rhs))
                               (where-pict (wrapper->pict lhs) (wrapper->pict rhs))]
                              [(struct metafunc-extra-side-cond (expr))
                               (wrapper->pict expr)])
                            where/sc)
                       (cond
                         [vertical-side-conditions? 
                          ;; maximize line breaks:
                          0]
                         [compact-side-conditions?
                          ;; maximize line break as needed:
                          max-line-w/pre-sc]
                         [else 
                          ;; no line breaks:
                          +inf.0]))])])))
  (case mode
    [(horizontal)
     (define (adjust-for-fills rows)
       ;; Some rows have the form (list <pict> 'fill 'fill),
       ;; in which case we want the <pict> to span all columns.
       ;; To do that, we need to know the total width of the first
       ;; two columns of non-spanning rows.
       (define max-w-before-rhs (apply 
                                 max
                                 (map (lambda (row)
                                        (match row
                                          [(list lhs 'fill 'fill)
                                           (+ sep sep)]
                                          [else
                                           (+ (pict-width (car row))
                                              sep
                                              (pict-width (cadr row))
                                              sep)]))
                                      rows)))
       (apply
        append
        (map (lambda (row)
               (match row
                 [(list lhs 'fill 'fill)
                  (list
                   ;; pretend that content is zero-width:
                   (inset lhs 0 0 (- (pict-width lhs)) 0)
                   (blank)
                   ;; blank that's wide enough to ensure that the
                   ;; right column covers the spanning pict
                   ;; (and no more)
                   (blank (max 0 (- (pict-width lhs) max-w-before-rhs))
                          0))]
                 [else row]))
             rows)))
     (table 3
            (adjust-for-fills
             (apply append
                    (for/list ([lhs/contract (in-list lhs/contracts)]
                               [sc (in-list scs)]
                               [rhs (in-list rhss)]
                               [linebreak? (in-list linebreak-list)])
                      (append
                       (list
                        (cond
                          [(equal? rhs 'contract) ;; contract
                           (list lhs/contract 'fill 'fill)]
                          [linebreak?
                           (list lhs/contract 'fill 'fill)]
                          [(and sc (eq? style 'left-right/beside-side-conditions))
                           (list lhs/contract =-pict (htl-append 10 rhs sc))]
                          [else
                           (list lhs/contract =-pict rhs)]))
                       (if linebreak?
                           (list
                            (list (htl-append sep =-pict rhs)
                                  'fill
                                  'fill))
                           null)
                       (if (or (not sc)
                               (and (not linebreak?)
                                    (eq? style 'left-right/beside-side-conditions)))
                           null
                           (list
                            (list sc 'fill 'fill)))))))
            ltl-superimpose ltl-superimpose
            sep sep)]
    [(vertical)
     (apply vl-append
            sep
            (apply append
                   (for/list ([lhs/contract (in-list lhs/contracts)]
                              [sc (in-list scs)]
                              [rhs (in-list rhss)])
                     (cond
                       [(equal? rhs 'contract) ;; contract
                        (list lhs/contract)]
                       [else
                        (cons
                         (vl-append (htl-append lhs/contract =-pict) rhs)
                         (if (not sc)
                             null
                             (list sc)))]))))]))

(define (render-metafunction-contract lang name doms rngs separators)
  (hbl-append (basic-text (format "~a" name) (metafunction-style))
              (basic-text " : " (default-style))
              (apply hbl-append (add-between (map (λ (x) (lw->pict lang x)) doms) 
                                             (basic-text " " (default-style))))
              (basic-text " → " (default-style))
              (apply hbl-append 
                     (add-between (interleave-ctc-separators
                                   (map (λ (x) (lw->pict lang x)) rngs)
                                   (map (λ (x) (basic-text (format "~a" x) (default-style)))
                                        separators))
                                  (basic-text " " (default-style))))))

(define (interleave-ctc-separators eles betweens)
  (cond
    [(null? betweens) eles]
    [else (list* (car eles)
                 (car betweens)
                 (interleave-ctc-separators (cdr eles) (cdr betweens)))]))

(define (metafunction-call name an-lw)
  (struct-copy lw an-lw
               [e
                (list*
                 ;; the first loc wrapper is just there to make the
                 ;; shape of this line be one that the apply-rewrites
                 ;; function (in core-layout.rkt) recognizes as a metafunction
                 (make-lw "("
                          (lw-line an-lw)
                          0
                          (lw-column an-lw)
                          0 
                          #f
                          #f)
                 (make-lw name
                          (lw-line an-lw)
                          0
                          (lw-column an-lw)
                          0 
                          #f
                          #t)
                 (cdr (lw-e an-lw)))]))  

(define (add-commas-and-rewrite-parens eles)
  (let loop ([eles eles]
             [between-parens? #f]
             [comma-pending #f])
    (cond
      [(null? eles) null]
      [else 
       (let ([an-lw (car eles)])
         (cond
           [(not (lw? an-lw)) 
            (cons an-lw (loop (cdr eles) between-parens? #f))]
           [(equal? "(" (lw-e an-lw))
            (cons (struct-copy lw
                               an-lw
                               [e (open-white-square-bracket)])
                  (loop (cdr eles) #t #f))]
           [(equal? ")" (lw-e an-lw))
            (cons (struct-copy lw
                               an-lw
                               [e (close-white-square-bracket)])
                  (loop (cdr eles) #f #f))]
           [(and between-parens?
                 comma-pending)
            (list* (build-lw (basic-text ", " (default-style))
                             (car comma-pending)
                             0
                             (cdr comma-pending)
                             0)
                   'spring
                   (loop eles #t #f))]
           [else
            (cons an-lw 
                  (loop (cdr eles)
                        between-parens?
                        (if between-parens?
                            (cons (+ (lw-line an-lw) (lw-line-span an-lw))
                                  (+ (lw-column an-lw) (lw-column-span an-lw)))
                            #f)))]))])))

(define (replace-paren x)
  (cond
    [(not (lw? x)) x]
    [(equal? "(" (lw-e x))
     (struct-copy lw
                  x
                  [e (hbl-append -2 
                                 (basic-text "[" (default-style))
                                 (basic-text "[" (default-style)))])]
    [(equal? ")" (lw-e x))
     (struct-copy lw
                  x
                  [e
                   (hbl-append -2 
                               (basic-text "]" (default-style))
                               (basic-text "]" (default-style)))])]
    [else x]))

(define (render-metafunction/proc mfs contract? filename name)
  (cond
    [filename
     (save-as-ps/pdf (λ () (metafunctions->pict/proc mfs contract? name))
                     filename)]
    [else
     (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
       (metafunctions->pict/proc mfs contract? name))]))

(define (render-pict make-pict filename)
  (cond
    [filename
     (save-as-ps/pdf make-pict filename)]
    [else
     (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
       (make-pict))]))

(define (inference-rules-pict name all-eqns eqn-names lang judgment-form?)
  (define all-nts (language-nts lang))
  (define (wrapper->pict lw) (lw->pict all-nts lw))
  (define all-conclusions 
    (map (lambda (eqn) 
           (wrapper->pict
            (metafunction-call name (list-ref eqn 0))))
         all-eqns))
  (define-values (selected-eqns conclusions selected-eqn-names)
    (select-jf-cases all-eqns all-conclusions eqn-names))
  
  (define (wrapper->pict+lines lw)
    (list (wrapper->pict lw) (lw-line lw) (+ (lw-line lw) (lw-line-span lw))))
  
  ;; premises : (listof (listof pict))
  ;; each inner list of premises goes on the same line; each element
  ;; of the outer list is a separate line
  (define premisess 
    (for/list ([eqn (in-list selected-eqns)])
      ;; all-premises : (listof pict number[first-line] number [last-line])
      (define all-premises
        (append (map wrapper->pict+lines (list-ref eqn 2))
                (map (match-lambda
                       [(struct metafunc-extra-where (lhs rhs))
                        (list (where-pict (wrapper->pict lhs) (wrapper->pict rhs))
                              (lw-line lhs)
                              (+ (lw-line rhs) (lw-line-span rhs)))]
                       [(struct metafunc-extra-side-cond (expr))
                        (wrapper->pict+lines expr)]
                       [wrapper (wrapper->pict+lines wrapper)])
                     (list-ref eqn 1))))
      (define sorted-premises (sort all-premises < #:key (λ (x) (list-ref x 1))))
       
      ;; returns #t if the two premises share at least one line in common
      (define (overlaps? premise1 premise2)
        (<= (list-ref premise1 1)
            (list-ref premise2 1)
            (list-ref premise1 2)))
      
      (cond
        [(not judgment-form?) (list (map car all-premises))]
        [(null? sorted-premises) '()]
        [else
         (define line-grouped-premises (list (list (car sorted-premises))))
         (for ([prev-premise (in-list sorted-premises)]
               [premise (in-list (cdr sorted-premises))])
           (cond
             [(overlaps? prev-premise premise)
              (set! line-grouped-premises (cons (cons premise (car line-grouped-premises))
                                                (cdr line-grouped-premises)))]
             [else
              (set! line-grouped-premises (cons (list premise) line-grouped-premises))]))
         (reverse (map (λ (x) (reverse (map car x))) line-grouped-premises))])))

  ((relation-clauses-combine)
   (for/list ([conclusion (in-list conclusions)]
              [premises (in-list premisess)]
              [name (in-list selected-eqn-names)])
     (define top (apply vc-append 4 (map (λ (premises) (apply hbl-append 20 premises)) premises)))
     (define line-w (max (pict-width top) (pict-width conclusion)))
     (define line (dc (λ (dc dx dy) (send dc draw-line dx dy (+ dx line-w) dy))
                      line-w 1)) 
     (define w/out-label
       (vc-append
        (horizontal-bar-spacing)
        top
        line
        conclusion))
     (if name
         (let ([label (string->bracketed-label name)])
           (let-values ([(x y) (rc-find w/out-label line)])
             (hb-append w/out-label 
                        (vl-append label
                                   (blank 0 (- (- (pict-height w/out-label) y)
                                               (/ (pict-height label) 2)))))))
         w/out-label))))

(define horizontal-bar-spacing (make-parameter 4))
(define relation-clauses-combine (make-parameter (λ (l) (apply vc-append 20 l))))

(define-for-syntax (inference-rules-pict/judgment-form form-name)
  (define jf (syntax-local-value form-name))
  (define-values (name lws rule-names lang relation?) 
    (values (judgment-form-name jf) (judgment-form-lws jf) (judgment-form-rule-names jf)
            (judgment-form-lang jf) (judgment-form-relation? jf)))
  (syntax-property
   #`(inference-rules-pict '#,name
                           #,lws
                           '#,rule-names
                           #,lang
                           #,(not relation?))
   'disappeared-use
   (syntax-local-introduce form-name)))

(define-syntax (render-judgment-form stx)
  (syntax-case stx ()
    [(_ form-name . opt-arg)
     (if (judgment-form-id? #'form-name)
         (let ([save-as (syntax-case #'opt-arg ()
                          [() #'#f]
                          [(path) #'path])])
           #`(render-pict (λ () #,(inference-rules-pict/judgment-form #'form-name))
                          #,save-as))
         (raise-syntax-error #f "expected a judgment form name" stx #'form-name))]))

(define-syntax (judgment-form->pict stx)
  (syntax-case stx ()
    [(_ form-name)
     (if (judgment-form-id? #'form-name)
         (inference-rules-pict/judgment-form #'form-name)
         (raise-syntax-error #f "expected a judgment form name" stx #'form-name))]))

;                              
;                              
;                              
;                              
;    ;                         
;    ;                         
;   ;;;     ;;;   ; ;;   ;;;;; 
;    ;     ;   ;  ;;  ;  ; ; ; 
;    ;     ;;;;;  ;   ;  ; ; ; 
;    ;     ;      ;      ; ; ; 
;    ;     ;   ;  ;      ; ; ; 
;     ;;    ;;;   ;      ; ; ; 
;                              
;                              
;

(define-syntax (render-term stx)
  (syntax-case stx ()
    [(_ lang term)
     #'(render-term/proc lang (to-lw term))]
    [(_ lang term filename)
     #'(render-term/proc lang (to-lw term) filename)]))

(define-syntax (term->pict stx)
  (syntax-case stx ()
    [(_ lang term)
     #'(do-term->pict lang (to-lw term))]))

(define (render-term/proc lang lw [filename #f])
  (if filename
      (save-as-ps/pdf (λ () (do-term->pict lang lw)) filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-term->pict lang lw))))

(define (do-term->pict lang lw) (lw->pict (language-nts lang) lw))

(define (render-term/pretty-write lang term [filename #f] #:width [width (pretty-print-columns)])
  (if filename
      (save-as-ps/pdf (λ () (term->pict/pretty-write lang term #:width width)) filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (term->pict/pretty-write lang term #:width width))))

(define (term->pict/pretty-write lang term #:width [width (pretty-print-columns)])
  (define-values (in out) (make-pipe))
  (thread (λ ()
            (parameterize ([pretty-print-columns width])
              (pretty-write term out))
            (close-output-port out)))
  (port-count-lines! in)
  (lw->pict lang (to-lw/stx (read-syntax #f in))))

(define (to-lw/stx stx)
  (lw/rt:add-spans/interp-lws 
   (syntax->datum 
    (lw/ct:to-lw/proc stx #f))))

