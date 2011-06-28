#lang racket/base
(require syntax/parse
         racket/syntax
         unstable/syntax
         "re-help.rkt"
         (for-template racket/base
                       racket/match
                       "machine.rkt"
                       (except-in "nfa-star.rkt" epsilon)
                       (prefix-in nfa: "nfa-star.rkt")))

(define-literal-set re-ops (complement seq union star epsilon nullset dseq rec unquote))

(define-syntax-class sre 
  #:literal-sets (re-ops)
  #:description "Fully Expanded Regular Expression"
  ; nfa is used for res without complement or dseq
  ; machine is used for others
  ; all-machines is machines all the way down, no nfas
  ; best is the best thing to embed in a machine
  #:attributes (nfa machine all-machines best)
  (pattern ((~and op unquote) e:expr)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa #f
           #:attr machine
           ; XXX contract to be a machine?
           #`e
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine))
  
  ; XXX This may not need to be built in because of unquote
  (pattern ((~and op rec) v:id lhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa #f
           #:attr machine
           #`(letrec ([inner
                       (let-syntax ([v
                                     (make-set!-transformer
                                      (lambda (stx)
                                        (syntax-case stx (set!)
                                          ; Redirect mutation of x to y
                                          [(set! _ _) 
                                           (raise-syntax-error 'rec "Cannot mutate a rec binding" stx)]
                                          ; Normal use of x really gets x
                                          [id (identifier? #'id)  #'(machine-delay (λ () inner))])))])
                         #,(attribute lhs.best))])
               inner)
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine))
  
  (pattern ((~and op complement) lhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa #f
           #:attr machine
           #`(machine-complement #,(attribute lhs.best))
           #:attr all-machines 
           #`(machine-complement #,(attribute lhs.all-machines))
           #:attr best (attribute machine))
  
  (pattern ((~and op star) lhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa 
           (and (attribute lhs.nfa)
                (with-syntax*
                    ([start_star (generate-temporary 'start_star)]
                     [(_ (starts_1 ...) ([accepting-state_1 (accepting-rule_1 ...)] ...) (non-accepting_1 ...))
                      (attribute lhs.nfa)])
                  #'(nfa* (start_star) 
                          ([start_star ([nfa:epsilon (starts_1 ...)])])
                          ([accepting-state_1 ([nfa:epsilon (start_star)] accepting-rule_1 ...)] ...
                           non-accepting_1 ...))))
           #:attr machine
           #`(machine-star #,(attribute lhs.best))
           #:attr all-machines
           #`(machine-star #,(attribute lhs.all-machines))
           #:attr best (or (attribute nfa) (attribute machine)))
  
  (pattern ((~and op seq) lhs:sre rhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa
           (and (attribute lhs.nfa)
                (attribute rhs.nfa)
                (with-syntax* 
                    ([(_ (starts_1 ...) ([accepting-state_1 (accepting-rule_1 ...)] ...) (non-accepting_1 ...))
                      (attribute lhs.nfa)]
                     [(_ (starts_2 ...) (accepting_2 ...) (non-accepting_2 ...))
                      (attribute rhs.nfa)]
                     [([accepting-state_2 . _] ...) #'(accepting_2 ...)])
                  #'(nfa* (starts_1 ...)
                          (accepting_2 ...)
                          ([accepting-state_1 ([nfa:epsilon (starts_2 ...)] accepting-rule_1 ...)] ...
                           non-accepting_1 ...
                           non-accepting_2 ...))))
           #:attr machine
           #`(machine-seq #,(attribute lhs.best) #,(attribute rhs.best))
           #:attr all-machines
           #`(machine-seq #,(attribute lhs.all-machines) #,(attribute rhs.all-machines))
           #:attr best (or (attribute nfa) (attribute machine)))
  
  (pattern ((~and op union) lhs:sre rhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa
           (and (attribute lhs.nfa)
                (attribute rhs.nfa)
                (with-syntax* 
                    ([(_ (starts_1 ...) (accepting_1 ...) (non-accepting_1 ...)) (attribute lhs.nfa)]
                     [(_ (starts_2 ...) (accepting_2 ...) (non-accepting_2 ...)) (attribute rhs.nfa)])
                  #'(nfa* (starts_1 ... starts_2 ...)
                          (accepting_1 ... accepting_2 ...)
                          (non-accepting_1 ... non-accepting_2 ...))))
           #:attr machine
           #`(machine-union #,(attribute lhs.best) #,(attribute rhs.best))
           #:attr all-machines
           #`(machine-union #,(attribute lhs.all-machines) #,(attribute rhs.all-machines))
           #:attr best (or (attribute nfa) (attribute machine)))
  
  (pattern (~and op epsilon)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa
           (with-syntax ([start (generate-temporary 'start)])
             #'(nfa* (start) ([start ()]) ()))
           #:attr machine
           #'machine-epsilon
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine))
  
  (pattern (~and op nullset)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa
           (with-syntax ([end (generate-temporary 'end)])
             #'(nfa* (end) () ([end ()])))
           #:attr machine
           #'machine-null
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine))
           
  (pattern ((~and op dseq) pat:expr rhs:sre)
           #:do [(record-disappeared-uses (list #'op))]
           #:attr nfa #f
           #:attr machine
           #`(machine '(dseq pat) (match-lambda [pat #,(attribute rhs.best)] [_ machine-null]))
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine))
  
  (pattern pat:expr
           #:attr nfa
           (with-syntax ([start (generate-temporary #'pat)]
                         [end (generate-temporary 'end)])
             #'(nfa* (start) ([end ()]) ([start ([pat (end)])])))
           #:attr machine
           #'(machine 'pat (match-lambda [pat machine-epsilon] [_ machine-null]))
           #:attr all-machines (attribute machine)
           #:attr best (attribute machine)))

(provide sre)
