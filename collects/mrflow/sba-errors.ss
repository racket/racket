
(module sba-errors (lib "mrflow.ss" "mrflow")
  (require
   (prefix cst: "constants.ss")
   (prefix lab: "labels.ss")
   ;"assoc-set-list.ss"
   "assoc-set-hash.ss"
   )
  
  ; (listof label) symbol string
  ; need this before the contracts because of sba-error?
  (define-struct sba-error (labels gravity message) (make-inspector))
  
  ; (assoc-setof label (listof sba-error))
  ; we use a list instead of a set for the sba-errors, because that's what error-table-get
  ; has to return anyway
  (define-struct error-table (assoc-set))
  
  (provide/contract
   (sba-error-gravity (sba-error? . -> . (symbols 'red 'orange 'green)))
   (sba-error-message (sba-error? . -> . string?))
   (error-table-make  (-> error-table?))
   (error-table? (any/c . -> . boolean?))
   (error-table-set (error-table? (listof lab:label?) (symbols 'red 'orange 'green) string? . -> . void?))
   (error-table-get (error-table? lab:label? . -> . (listof sba-error?)))
   )
  
  ; -> error-table
  (define (error-table-make)
    (make-error-table (assoc-set-make)))
  
  ; top -> boolean
  ; error-table? comes from the structure definition
  
  ; error-table (listof label) (union 'red 'orange 'green) string -> void
  ; adds error to the error list for each label
  ; we use terms instead of labels as the key, because a primitive will have several labels
  ; associated with it (one created from the program text, and at least one created from the
  ; type for that primitive), so we need to use as key something unique about the primitive.
  (define (error-table-set error-table labels gravity message)
    (let ([assoc-set (error-table-assoc-set error-table)]
          [error (make-sba-error labels gravity message)])
      (for-each (lambda (label)
                  (let ([term (lab:label-term label)])
                    (if (syntax-position term)
                        (assoc-set-set 
                         assoc-set
                         term
                         (cons error (assoc-set-get assoc-set term cst:thunk-empty))
                         #f)
                        (printf "~a error detected for term ~a: ~a~n"
                                gravity
                                (syntax-object->datum term)
                                message))))
                labels)))
  
  ; error-table label -> (listof sba-error)
  (define (error-table-get error-table label)
    (assoc-set-get (error-table-assoc-set error-table) (lab:label-term label) cst:thunk-empty))
  
  )
