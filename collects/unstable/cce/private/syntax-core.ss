#lang scheme/base

(require scheme/contract
         scheme/match
         (only-in unstable/syntax with-syntax*)
         "../text.ss"
         (for-syntax scheme/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Source Locations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide src-known? src->srcloc src->list src->vector src->syntax)

(define srcloc->list
  (match-lambda
    [(struct srcloc [src line col pos span])
     (list src line col pos span)]))

(define srcloc->vector
  (match-lambda
    [(struct srcloc [src line col pos span])
     (vector src line col pos span)]))

(define (srcloc->syntax loc [v null])
  (datum->syntax #f v (srcloc->list loc)))

(define (src->srcloc . locs) (combine-srclocs (map convert-loc locs)))

(define src->list   (compose srcloc->list   src->srcloc))
(define src->vector (compose srcloc->vector src->srcloc))
(define src->syntax (compose srcloc->syntax src->srcloc))

(define (src-known? x)
  (not (equal? (convert-loc x) (convert-loc #f))))

(define convert-loc
  (match-lambda
    [(? srcloc? loc) loc]
    [(or (list   src line col pos span)
         (vector src line col pos span)
         (and #f src line col pos span)
         (and (? syntax?)
              (app syntax-source   src)
              (app syntax-line     line)
              (app syntax-column   col)
              (app syntax-position pos)
              (app syntax-span     span)))
     (make-srcloc src line col pos span)]))

(define combine-srclocs
  (match-lambda
    ;; Two locations with matching source
    [(list (struct srcloc [src line1 col1 pos1 span1])
           (struct srcloc [src line2 col2 pos2 span2])
           locs ...)
     (let* ([line (and line1 line2 (min line1 line2))]
            [col (and line col1 col2
                      (cond [(< line1 line2) col1]
                            [(= line1 line2) (min col1 col2)]
                            [(> line1 line2) col2]))]
            [pos (and pos1 pos2 (min pos1 pos2))]
            [span (and pos span1 span2
                       (- (max (+ pos1 span1) (+ pos2 span2)) pos))])
       (combine-srclocs (cons (make-srcloc src line col pos span) locs)))]
    ;; One location
    [(list loc) loc]
    ;; No locations, or mismatched sources
    [_ (make-srcloc #f #f #f #f #f)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Conversions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide syntax-map to-syntax to-datum)

(define (syntax-map f stx)
  (map f (syntax->list stx)))

(define (to-syntax datum
                   #:stx [stx #f]
                   #:src [src stx]
                   #:ctxt [ctxt stx]
                   #:prop [prop stx]
                   #:cert [cert stx])
  (datum->syntax ctxt
                 datum
                 (if (srcloc? src) (srcloc->list src) src)
                 prop
                 cert))

(define (to-datum v)
  (if (syntax? v)
      (syntax->datum v)
      v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Pattern Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide with-syntax* syntax-list)

(define-syntax-rule (syntax-list template ...)
  (syntax->list (syntax (template ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Errors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide current-syntax syntax-error)

(define current-syntax (make-parameter #f))

(define (syntax-error #:name [name #f] stx msg . args)
  (let* ([cur (current-syntax)]
         [one (if cur cur stx)]
         [two (if cur stx #f)]
         [sym (if name (text->symbol name) #f)]
         [str (apply format msg args)])
    (raise-syntax-error sym str one two)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Contracts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide syntax-datum/c syntax-listof/c syntax-list/c)

(define (syntax-datum/c datum)
  (let* ([datum/c (coerce-contract datum datum)])
    (flat-named-contract (build-compound-type-name 'syntax-datum/c datum/c)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate datum/c)
              (syntax->datum v)))))))

(define (syntax-listof/c elem)
  (let* ([elem/c (coerce-contract elem elem)])
    (flat-named-contract (build-compound-type-name 'syntax-listof/c elem/c)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate (listof elem/c))
              (syntax->list v)))))))

(define (syntax-list/c . elems)
  (let* ([elem/cs (map (lambda (elem) (coerce-contract elem elem)) elems)])
    (flat-named-contract (apply build-compound-type-name 'syntax-list/c elem/cs)
      (lambda (v)
        (and (syntax? v)
             ((flat-contract-predicate (apply list/c elem/cs))
              (syntax->list v)))))))
