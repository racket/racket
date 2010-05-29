#lang scheme/base
(require scheme/path
         scheme/match
         scheme/contract
         scheme/vector
         scheme/list
         syntax/stx
         syntax/kerncase
         setup/main-collects
         planet/planet-archives
         unstable/text
         (for-template scheme/base)
         (for-syntax scheme/base)
         (for-label scheme)
         "private/syntax-core.ss"
         "private/define-core.ss"
         (for-template "private/define-core.ss")
         "contract.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SYNTAX OBJECTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Locations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-source-directory stx)
  (match (syntax-source stx)
    [(? path-string? source)
     (let-values ([(base file dir?) (split-path source)])
       (and (path? base)
            (path->complete-path base
                                 (or (current-load-relative-directory)
                                     (current-directory)))))]
    [_ #f]))

(define (syntax-source-file-name stx)
  (match (syntax-source stx)
    [(? path-string? f)
     (let-values ([(base file dir?) (split-path f)]) file)]
    [_ #f]))

(define (syntax-source-planet-package stx)
  (let* ([dir (syntax-source-directory stx)])
    (and dir (this-package-version/proc dir))))

(define (syntax-source-planet-package-owner stx)
  (let* ([pkg (syntax-source-planet-package stx)])
    (and pkg (pd->owner pkg))))

(define (syntax-source-planet-package-name stx)
  (let* ([pkg (syntax-source-planet-package stx)])
    (and pkg (pd->name pkg))))

(define (syntax-source-planet-package-major stx)
  (let* ([pkg (syntax-source-planet-package stx)])
    (and pkg (pd->maj pkg))))

(define (syntax-source-planet-package-minor stx)
  (let* ([pkg (syntax-source-planet-package stx)])
    (and pkg (pd->min pkg))))

(define (syntax-source-planet-package-symbol stx [suffix #f])
  (match (syntax-source-planet-package stx)
    [(list owner name major minor)
     (string->symbol
      (format "~a/~a:~a:~a~a"
              owner
              (regexp-replace "\\.plt$" name "")
              major
              minor
              (if suffix (text->string "/" suffix) "")))]
    [#f #f]))

(define (make-planet-path stx id/f)
  (datum->syntax
   stx
   (list #'planet (syntax-source-planet-package-symbol stx id/f))
   (or id/f stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformer patterns:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((redirect-transformer id) stx)
  (cond
   [(identifier? stx) id]
   [(and (stx-pair? stx) (identifier? (stx-car stx)))
    (to-syntax (cons id (stx-cdr stx)) #:stx stx)]
   [else
    (syntax-error
     stx
     "expected an identifier (alone or in application position); cannot redirect to ~a"
     (syntax-e id))]))

(define (head-expand stx [stop-ids null])
  (local-expand stx
                (syntax-local-context)
                (append stop-ids (full-kernel-form-identifier-list))
                #f))

(define-syntax-if-unbound quote-syntax/prune
  (make-rename-transformer #'quote-syntax))

(define (full-kernel-form-identifier-list)
  (remove-duplicates
   (list* (quote-syntax/prune #%require)
          (quote-syntax/prune #%provide)
          (quote-syntax/prune module)
          (quote-syntax/prune #%plain-module-begin)
          (kernel-form-identifier-list))
   free-identifier=?))

(define (quote-transformer datum)
  #`(quasiquote
     #,(let loop ([datum datum])
         (cond
          [(syntax? datum) #`(unquote (quote-syntax #,datum))]
          [(pair? datum) #`#,(cons (loop (car datum)) (loop (cdr datum)))]
          [(vector? datum) #`#,(vector-map loop datum)]
          [(box? datum) #`#,(box (loop (unbox datum)))]
          [(hash? datum)
           #`#,((cond [(hash-eqv? datum) make-immutable-hasheqv]
                      [(hash-eq? datum) make-immutable-hasheq]
                      [else make-immutable-hash])
                (hash-map datum (lambda (k v) (cons k (loop v)))))]
          [(prefab-struct-key datum) =>
           (lambda (key)
             #`#,(apply make-prefab-struct
                        key
                        (for/list ([i (in-vector (struct->vector datum) 1)])
                          (loop i))))]
          [else #`#,datum]))))

(define trampoline-prompt-tag
  (make-continuation-prompt-tag 'trampoline))

(define ((trampoline-transformer transform) stx)

  (define intro (make-syntax-introducer))

  (define (body)
    (syntax-local-introduce
     (intro
      (transform (trampoline-evaluator intro)
                 intro
                 (intro (syntax-local-introduce stx))))))

  (call-with-continuation-prompt body trampoline-prompt-tag))

(define ((trampoline-evaluator intro) stx)

  (define ((wrap continue))
    (call-with-continuation-prompt continue trampoline-prompt-tag))

  (define ((expander continue))
    #`(begin #,(syntax-local-introduce (intro stx))
             (#%trampoline #,(wrap continue))))

  (define (body continue)
    (abort-current-continuation trampoline-prompt-tag (expander continue)))

  (call-with-composable-continuation body trampoline-prompt-tag)
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From planet/util:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (this-package-version/proc srcdir)
  (let* ([package-roots (get-all-planet-packages)]
         [thepkg (ormap (predicate->projection (contains-dir? srcdir))
                        package-roots)])
    (and thepkg (archive-retval->simple-retval thepkg))))

;; predicate->projection : #f \not\in X ==> (X -> boolean) -> (X -> X)
(define (predicate->projection pred) (lambda (x) (if (pred x) x #f)))

;; contains-dir? : path -> pkg -> boolean
(define ((contains-dir? srcdir) alleged-superdir-pkg)
  (let* ([nsrcdir (normalize-path srcdir)]
         [nsuperdir (normalize-path (car alleged-superdir-pkg))]
         [nsrclist (explode-path nsrcdir)]
         [nsuperlist (explode-path nsuperdir)])
    (list-prefix? nsuperlist nsrclist)))

(define (list-prefix? sup sub)
  (let loop ([sub sub]
             [sup sup])
    (cond
      [(null? sup) #t]
      [(equal? (car sup) (car sub))
       (loop (cdr sub) (cdr sup))]
      [else #f])))

(define (archive-retval->simple-retval p)
  (list-refs p '(1 2 4 5)))

(define-values (pd->owner pd->name pd->maj pd->min)
  (apply values (map (lambda (n) (lambda (l) (list-ref l n))) '(0 1 2 3))))

(define (list-refs p ns)
  (map (lambda (n) (list-ref p n)) ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EXPORTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stx/f (or/c syntax? #f))

(define nat/f (or/c nat/c #f))
(define pos/f (or/c pos/c #f))

(define src-list/c   (list/c   any/c pos/f nat/f pos/f nat/f))
(define src-vector/c (vector/c any/c pos/f nat/f pos/f nat/f))

(define src/c
  (or/c srcloc?
        syntax?
        src-list/c
        src-vector/c
        #f))

(provide/contract

 [src/c flat-contract?]
 [src-known? (-> src/c boolean?)]
 [src->srcloc  (->* [] [] #:rest (listof src/c) srcloc?)]
 [src->list (->* [] [] #:rest (listof src/c) src-list/c)]
 [src->vector (->* [] [] #:rest (listof src/c) src-vector/c)]
 [src->syntax (->* [] [] #:rest (listof src/c) syntax?)]

 [syntax-datum/c (-> flat-contract? flat-contract?)]
 [syntax-listof/c (-> flat-contract? flat-contract?)]
 [syntax-list/c
  (->* [] [] #:rest (listof flat-contract?) flat-contract?)]

 [syntax-map (-> (-> syntax? any/c) (syntax-listof/c any/c) (listof any/c))]
 [to-syntax
  (->* [any/c]
       [#:stx stx/f #:src src/c #:ctxt stx/f #:prop stx/f #:cert stx/f]
       syntax?)]
 [to-datum (-> any/c (not/c syntax?))]

 [syntax-source-file-name (-> syntax? (or/c path? #f))]
 [syntax-source-directory (-> syntax? (or/c path? #f))]
 [syntax-source-planet-package
  (-> syntax? (or/c (list/c string? string? nat/c nat/c) #f))]
 [syntax-source-planet-package-owner (-> syntax? (or/c string? #f))]
 [syntax-source-planet-package-name (-> syntax? (or/c string? #f))]
 [syntax-source-planet-package-major (-> syntax? (or/c nat/c #f))]
 [syntax-source-planet-package-minor (-> syntax? (or/c nat/c #f))]
 [syntax-source-planet-package-symbol
  (->* [syntax?] [(or/c text? #f)] (or/c symbol? #f))]
 [make-planet-path (-> syntax? (or/c identifier? #f) syntax?)]

 [trampoline-transformer
  (-> (-> (-> syntax? void?) (-> syntax? syntax?) syntax? syntax?)
      (-> syntax? syntax?))]
 [quote-transformer (-> any/c syntax?)]
 [redirect-transformer (-> identifier? (-> syntax? syntax?))]
 [head-expand (->* [syntax?] [(listof identifier?)] syntax?)]
 [full-kernel-form-identifier-list (-> (listof identifier?))]

 [current-syntax (parameter/c (or/c syntax? false/c))]
 [syntax-error (->* [syntax? string?]
                    [#:name (or/c text? #f)]
                    #:rest list?
                    none/c)])

(provide with-syntax* syntax-list)
