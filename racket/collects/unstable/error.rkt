#lang racket/base
(require racket/contract/base
         racket/string
         racket/list
         syntax/srcloc
         syntax/stx)

#|
TODO
 - more options
   - 'pretty : pretty-print, then use multi-line format as necessary
 - need no-contracts version?
 - document or remove #:details arg
|#

;; A DetailsTable is (listof (cons Field any))
;; A Field is one of
;; - string
;; - (cons string (listof FieldOption))
;; A FieldOption is one of
;; - 'multi
;; - 'value
;; - 'maybe

(define field-option/c (or/c 'multi 'value 'maybe))
(define field/c (or/c string? (cons/c string? (listof field-option/c))))

(define details-list/c
  (recursive-contract
   (or/c '() (cons/c field/c (cons/c any/c details-list/c)))))

(provide/contract
 [error*
  (->* [symbol? string?]
       [#:continued (or/c string? (listof string))]
       #:rest details-list/c
       any)]
 [raise-syntax-error*
  (->* [string? (or/c syntax? #f) (or/c syntax? #f)]
       [#:continued (or/c string? (listof string))
        #:within (or/c #f syntax?)]
       #:rest details-list/c
       any)]
 [compose-error-message
  (->* [(or/c symbol? #f) string?]
       [#:continued (or/c string? (listof string))]
       #:rest details-list/c
       string?)]
 [compose-error-detail
  (-> string? (listof field-option/c) any/c
      string?)])

;; ----

(define (error* who message
                #:continued [continued-message null]
                . field+detail-list)
  (raise
   (exn:fail
    (compose* who message
              continued-message
              (field+detail-list->table 'error* field+detail-list null))
    (current-continuation-marks))))

(define (raise-syntax-error* message0 stx sub-stx
                             #:who [who #f]
                             #:within [within-stx #f]
                             #:continued [continued-message null]
                             #:extra-sources [extra-stxs null]
                             . field+detail-list)
  (let* ([source-stx (or stx sub-stx within-stx)]
         [who (or who
                  (let* ([maybe-id (if (stx-pair? stx) (stx-car stx) stx)])
                    (if (identifier? maybe-id) (syntax-e maybe-id) '?)))]
         [message
          (apply compose-error-message who message0
                 #:continued continued-message
                 '("at" maybe) (and sub-stx
                                    (error-print-source-location)
                                    (format "~.s" (syntax->datum sub-stx)))
                 '("within" maybe) (and within-stx
                                        (error-print-source-location)
                                        (format "~.s" (syntax->datum within-stx)))
                 '("in" maybe) (and stx
                                    (error-print-source-location)
                                    (format "~.s" (syntax->datum stx)))
                 field+detail-list)]
         [message
          (if (error-print-source-location)
              (string-append (source-location->prefix source-stx) message)
              message)])
    (raise
     (exn:fail:syntax message
                      (current-continuation-marks)
                      (cond [within-stx (cons within-stx extra-stxs)]
                            [sub-stx (cons sub-stx extra-stxs)]
                            [stx (cons stx extra-stxs)]
                            [else extra-stxs])))))

;; ----

;; compose-error-message : .... -> string
(define (compose-error-message who message
                               #:continued [continued-message null]
                               . field+detail-list)
  (define details
    (field+detail-list->table 'compose-error-message field+detail-list null))
  (compose* who message continued-message details))

;; compose-error-detail : string (listof option) any -> (listof string)
;; Note: includes a leading newline (unless detail omitted).
(define (compose-error-detail field options value)
  (apply string-append (compose-detail* field options value)))

;; ----

(define (compose* who message continued-message details)
  (let* ([parts (apply append
                       (for/list ([detail (in-list details)])
                         (let* ([field+opts (car detail)]
                                [field (if (pair? field+opts) (car field+opts) field+opts)]
                                [options (if (pair? field+opts) (cdr field+opts) '())]
                                [value (cdr detail)])
                           (compose-detail* field options value))))]
         [parts (let loop ([continued continued-message])
                  (cond [(pair? continued) (list* "\n " (car continued) (loop (cdr continued)))]
                        [(string? continued) (loop (list continued))]
                        [(null? continued) parts]))]
         [parts (list* message (if (null? continued-message) "" ";") parts)]
         [parts (if who
                    (list* (symbol->string who) ": " parts)
                    parts)])
    (apply string-append parts)))

(define (compose-detail* field options value)
  (let* ([value? (memq 'value options)]
         [multi? (memq 'multi options)]
         [maybe? (memq 'maybe options)]
         [noindent? (memq 'noindent options)]
         [convert-value0
          (cond [value?
                 (lambda (v) ((error-value->string-handler) v (error-print-width)))]
                [else
                 (lambda (v) (format "~a" v))])]
         [convert-value
          (if noindent?
              (lambda (v indent) (list (convert-value0 v)))
              (lambda (v indent)
                (let* ([s (convert-value0 v)]
                       [lines (string-split s #rx"[\n]" #:trim? #f)]
                       [spacing
                        (case indent
                          ((3) "\n   ") ;; common case, make constant
                          (else (string-append "\n" (make-string indent #\space))))])
                  (add-between lines spacing))))])
    (cond [(and (or maybe? multi? (not value?))
                (not value))
           null]
          [(and maybe? multi?
                (null? value))
           null]
          [multi?
           (list* "\n  " field ": "
                  (let value-loop ([value value])
                    (cond [(pair? value)
                           (list* "\n   "
                                  (append (convert-value (car value) 3)
                                          (value-loop (cdr value))))]
                          [(null? value)
                           null])))]
          [else
           (list* "\n  " field ": "
                  (convert-value value (+ 4 (string-length field))))])))

;; ----

(define (field+detail-list->table who lst onto)
  (cond [(null? lst) onto]
        [else
         (let ([field (car lst)]
               [value (cadr lst)])
           (cons (cons field value)
                 (field+detail-list->table who (cddr lst) onto)))]))
