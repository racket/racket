#lang racket/base
(require racket/contract
	 racket/list
	 racket/match
	 racket/function
         "lang-struct.rkt"
	 "match-a-pattern.rkt"
	 "enumerator.rkt")

(provide 
 (contract-out
  [lang-enumerators (-> (listof nt?) (hash/c symbol? enum?))]
  [pat-enumerator (-> (hash/c symbol? enum?)
                      any/c ;; pattern
                      enum?)]
  [enum-ith (-> enum? exact-nonnegative-integer? any/c)]
  [enum? (-> any/c boolean?)]))


(define (lang-enumerators nts)
  (let* ([l-enums (make-hash)]
	 [rec-nt-terms (find-recs nts)]
	 [sorted-nts (sort-nt-terms nts rec-nt-terms)])
    (foldl
     (λ (nt m)
	(hash-set
	 m
	 (nt-name nt)
	 (with-handlers
	     ([exn:fail? fail/enum])
	   (rec-pat/enum `(nt ,(nt-name nt))
			 sorted-nts
			 rec-nt-terms))))
     (hash)
     sorted-nts)))

(define enum-ith decode)
(struct decomposition (ctx term))

(define (pat-enumerator lang-enums pat)
  (enum-names pat
	      (sep-names pat)
	      lang-enums))

(define (rec-pat/enum pat nts rec-nt-terms)
  (enum-names pat
	      nts
	      (sep-names pat)
	      rec-nt-terms))


;; find-recs : lang -> (hash symbol -o> (assoclist rhs bool))
;; Identifies which non-terminals are recursive
(define (find-recs nt-pats)
  (define is-rec?
    (case-lambda
      [(n) (is-rec? n (hash))]
      [(nt seen)
       (or (seen? seen (nt-name nt))
	   (ormap
	    (λ (rhs)
	       (let rec ([pat (rhs-pattern rhs)])
		 (match-a-pattern
		  pat
		  [`any #f]
		  [`number #f]
		  [`string #f]
		  [`natural #f]
		  [`integer #f]
		  [`real #f]
		  [`boolean #f]
		  [`variable #f]
		  [`(variable-except ,s ...) #f]
		  [`(variable-prefix ,s) #f]
		  [`variable-not-otherwise-mentioned #f]
		  [`hole #f]
		  [`(nt ,id)
		   (is-rec? (make-nt
			     id
			     (lookup nt-pats id))
			    (add-seen seen
				      (nt-name nt)))]
		  [`(name ,name ,pat)
		   (rec pat)]
		  [`(mismatch-name ,name ,pat)
		   (rec pat)]
		  [`(in-hole ,p1 ,p2)
		   (or (rec p1)
		       (rec p2))]
		  [`(hide-hole ,p) (rec p)]
		  [`(side-condition ,p ,g ,e) ;; error
		   (error 'unsupported "side-condition")]
		  [`(cross ,s)
		   (error 'unsupported "cross")] ;; error
		  [`(list ,sub-pats ...)
		   (ormap (λ (sub-pat)
			     (match sub-pat
			       [`(repeat ,pat ,name ,mismatch)
				(rec pat)]
			       [else (rec sub-pat)]))
			  sub-pats)]
		  [(? (compose not pair?)) #f])))
	    (nt-rhs nt)))]))
  (define (calls-rec? rhs recs)
    (let rec ([pat (rhs-pattern rhs)])
      (match-a-pattern
       pat
       [`any #f]
       [`number #f]
       [`string #f]
       [`natural #f]
       [`integer #f]
       [`real #f]
       [`boolean #f]
       [`variable #f]
       [`(variable-except ,s ...) #f]
       [`(variable-prefix ,s) #f]
       [`variable-not-otherwise-mentioned #f]
       [`hole #f]
       [`(nt ,id)
	(hash-ref recs id)]
       [`(name ,name ,pat)
	(rec pat)]
       [`(mismatch-name ,name ,pat)
	(rec pat)]
       [`(in-hole ,p1 ,p2)
	(or (rec p1)
	    (rec p2))]
       [`(hide-hole ,p) (rec p)]
       [`(side-condition ,p ,g ,e) ;; error
	(error 'no-enum "side-condition")]
       [`(cross ,s)
	(error 'no-enum "cross")] ;; error
       [`(list ,sub-pats ...)
	(ormap (λ (sub-pat)
		  (match sub-pat
		    [`(repeat ,pat ,name ,mismatch)
		     (rec pat)]
		    [else (rec sub-pat)]))
	       sub-pats)]
       [(? (compose not pair?)) #f])))
  (define (seen? m s)
    (hash-ref m s #f))
  (define (add-seen m s)
    (hash-set m s #t))
  (let ([recs
	 (foldl
	  (λ (nt m)
	     (hash-set m (nt-name nt) (is-rec? nt)))
	  (hash) nt-pats)])
    (foldl
     (λ (nt m)
	(let ([rhs (nt-rhs nt)])
	  (hash-set m (nt-name nt)
		    (map (λ (rhs)
			    (cons rhs (calls-rec? rhs recs)))
			 rhs))))
     (hash)
     nt-pats)))

;; sort-nt-terms : lang (hash symbol -o> (assoclist rhs bool)) -> lang
(define (sort-nt-terms nt-pats recs)
  (map
   (λ (nt)
      (let ([rec-nts (hash-ref recs (nt-name nt))])
	(make-nt (nt-name nt)
		 (sort (nt-rhs nt)
		       (λ (r1 r2)
			  (and (not (cdr (assoc r1 rec-nts)))
			       (cdr (assoc r2 rec-nts))))))))
   nt-pats))

;; sep-names : single-pattern lang -> (assoclist symbol pattern)
(define (sep-names pat)
  (let loop ([pat pat]
	     [named-pats '()])
    (match-a-pattern
     pat
     [`any named-pats]
     [`number named-pats]
     [`string named-pats]
     [`natural named-pats]
     [`integer named-pats]
     [`real named-pats]
     [`boolean named-pats]
     [`variable named-pats]
     [`(variable-except ,s ...) named-pats]
     [`(variable-prefix ,s) named-pats]
     [`variable-not-otherwise-mentioned named-pats]
     [`hole named-pats]
     ;; names inside nts are separate
     [`(nt ,id) named-pats]
     [`(name ,name ,pat)
      (loop pat
	    (add-if-new name pat named-pats))]
     [`(mismatch-name ,name ,pat)
      (loop pat
	    (add-if-new name pat named-pats))]
     [`(in-hole ,p1 ,p2)
      (loop p2
	    (loop p1 named-pats))]
     [`(hide-hole ,p) (loop p named-pats)]
     [`(side-condition ,p ,g ,e) ;; error
      (error 'no-enum "side condition")]
     [`(cross ,s)
      (error 'no-enum "cross")] ;; error
     [`(list ,sub-pats ...)
      (foldl (λ (sub-pat named-pats)
		(match sub-pat
		  ;; unnamed repeat
		  [`(repeat ,pat #f #f)
		   (loop pat named-pats)]
		  ;; named repeat 
		  [`(repeat ,pat ,name #f)
		   (loop pat
			 (add-if-new name 'name-r named-pats))]
		  ;; mismatch named repeat
		  [`(repeat ,pat #f ,mismatch)
		   (loop pat
			 (add-if-new mismatch 'mismatch-r named-pats))]
		  ;; normal subpattern
		  [else (loop sub-pat named-pats)]))
	     named-pats
	     sub-pats)]
     [(? (compose not pair?))
      named-pats])))

(define (add-if-new k v l)
  (cond [(assoc k l) l]
	[else (cons `(,k ,v) l)]))

(define enum-names
  (case-lambda
    [(pat named-pats nts)
     (enum-names-with
      (λ (pat named)
	 (pat/enum-with-names pat nts named))
      pat named-pats)]
    [(pat nts named-pats rec-nt-terms)
     (enum-names-with
      (λ (pat named)
	 (pat/enum-with-names pat nts named rec-nt-terms))
      pat named-pats)]))

(define (enum-names-with f pat named-pats)
  (let rec ([named-pats named-pats]
	    [env (hash)])
    (cond [(null? named-pats) (f pat env)]
	  [else
	   (match
	     (car named-pats)
	     ;; named repeat
	     [`(,name name-r)
	      (error 'unimplemented "named-repeat")]
	     ;; mismatch repeat
	     [`(,name mismatch-r)
	      (error 'unimplemented "mismatch-repeat")]
	     [`(,name ,pat mismatch)
	      (error 'unimplemented "mismatch")]
	     ;; named
	     [`(,name ,pat)
	      (map/enum ;; loses bijection
	       cdr
	       (λ (x) (cons name x))
	       (dep/enum
		(f pat env)
		(λ (term)
		   (rec (cdr named-pats)
			(hash-set env
				  name
				  term)))))]
	     [else (error 'bad-assoc)])])))

(define pat/enum-with-names
  (case-lambda
    [(pat nt-enums named-terms)
     (let loop ([pat pat])
       (match-a-pattern
	pat
	[`any 
	 (sum/enum
	  any/enum
	  (listof/enum any/enum))]
	[`number num/enum]
	[`string string/enum]
	[`natural natural/enum]
	[`integer integer/enum]
	[`real real/enum]
	[`boolean bool/enum]
	[`variable var/enum]
	[`(variable-except ,s ...)
	 ;; todo
	 (error 'unimplemented "var-except")]
	[`(variable-prefix ,s)
	 ;; todo
	 (error 'unimplemented "var-prefix")]
	[`variable-not-otherwise-mentioned
	 (error 'unimplemented "var-not-mentioned")] ;; error
	[`hole
	 (const/enum 'hole)]
	[`(nt ,id)
	 (hash-ref nt-enums id)]
	[`(name ,name ,pat)
	 (const/enum (hash-ref named-terms name))]
	[`(mismatch-name ,name ,pat)
	 (error 'unimplemented "mismatch-name")]
	[`(in-hole ,p1 ,p2) ;; untested
	 (map/enum
	  (λ (t1-t2) ;; loses bijection
	     (plug-hole (car t1-t2)
			(cdr t1-t2)))
	  (λ (plugged)
	     (cons 'hole plugged))
	  (prod/enum
	   (loop p1)
	   (loop p2)))]
	[`(hide-hole ,p)
	 (loop p)]
	[`(side-condition ,p ,g ,e)
	 (error 'no-enum "side condition")]
	[`(cross ,s)
	 (error 'no-enum "cross")]
	[`(list ,sub-pats ...)
	 ;; enum-list
	 (map/enum
	  flatten-1
	  identity
	  (list/enum
	   (map
	    (λ (sub-pat)
	       (match sub-pat
		 [`(repeat ,pat #f #f)
		  (map/enum
		   cdr
		   (λ (ts)
		      (cons (length ts)
			    ts))
		   (dep/enum
		    nats
		    (λ (n)
		       (list/enum
			(build-list n (const (loop pat)))))))]
		 [`(repeat ,pat ,name #f)
		  (error 'unimplemented "named-repeat")]
		 [`(repeat ,pat #f ,mismatch)
		  (error 'unimplemented "mismatch-repeat")]
		 [else (loop sub-pat)]))
	    sub-pats)))]
	[(? (compose not pair?)) 
	 (const/enum pat)]))]
    [(pat nts named-terms rec-nt-terms)
     (let loop ([pat pat])
       (match-a-pattern
	pat
	[`any 
	 (sum/enum
	  any/enum
	  (listof/enum any/enum))]
	[`number num/enum]
	[`string string/enum]
	[`natural natural/enum]
	[`integer integer/enum]
	[`real real/enum]
	[`boolean bool/enum]
	[`variable var/enum]
	[`(variable-except ,s ...)
	 ;; todo
	 (error 'unimplemented "var except")]
	[`(variable-prefix ,s)
	 ;; todo
	 (error 'unimplemented "var prefix")]
	[`variable-not-otherwise-mentioned
	 (error 'unimplemented "var not otherwise mentioned")]
	[`hole
	 (const/enum 'hole)]
	[`(nt ,id)
	 (let ([rhss (lookup nts id)])
	   (apply sum/enum
		  (map
		   (λ (rhs)
		      (cond [(cdr (assoc rhs (hash-ref rec-nt-terms id)))
			     (thunk/enum
			      +inf.f
			      (λ ()
				 (rec-pat/enum (rhs-pattern rhs)
					       nts
					       rec-nt-terms)))]
			    [else
			     (rec-pat/enum (rhs-pattern rhs)
					   nts
					   rec-nt-terms)]))
		   rhss)))]
	[`(name ,name ,pat)
	 (const/enum (hash-ref named-terms name))]
	[`(mismatch-name ,name ,pat)
	 (error 'unimplemented "mismatch-name")]
	[`(in-hole ,p1 ,p2) ;; untested
	 (map/enum
	  (λ (t1-t2)
	     (decomposition (car t1-t2)
			    (cdr t1-t2)))
	  (λ (decomp)
	     (cons (decomposition-ctx decomp)
		   (decomposition-term decomp)))
	  (prod/enum
	   (loop p1)
	   (loop p2)))]
	[`(hide-hole ,p)
	 ;; todo
	 (loop p)]
	[`(side-condition ,p ,g ,e)
	 (error 'no-enum "side-condition")]
	[`(cross ,s)
	 (error 'no-enum "cross")]
	[`(list ,sub-pats ...)
	 ;; enum-list
	 (map/enum
	  flatten-1
	  identity
	  (list/enum
	   (map
	    (λ (sub-pat)
	       (match sub-pat
		 [`(repeat ,pat #f #f)
		  (map/enum
		   cdr
		   (λ (ts)
		      (cons (length ts)
			    ts))
		   (dep/enum
		    nats
		    (λ (n)
		       (list/enum
			(build-list n (const (loop pat)))))))]
		 [`(repeat ,pat ,name #f)
		  (error 'unimplemented "named-repeat")]
		 [`(repeat ,pat #f ,mismatch)
		  (error 'unimplemented "mismatch-repeat")]
		 [else (loop sub-pat)]))
	    sub-pats)))]
	[(? (compose not pair?)) 
	 (const/enum pat)]))]))

(define (flatten-1 xs)
  (append-map
   (λ (x)
      (if (or (pair? x)
	      (null? x))
	  x
	  (list x)))
   xs))

;; lookup : lang symbol -> (listof rhs)
(define (lookup nts name)
  (let rec ([nts nts])
    (cond [(null? nts) (error 'unkown-nt)]
	  [(eq? name (nt-name (car nts)))
	   (nt-rhs (car nts))]
	  [else (rec (cdr nts))])))

(define natural/enum nats)

(define char/enum
  (map/enum
   integer->char
   char->integer
   (range/enum #x61 #x7a)))

(define string/enum
  (map/enum
   list->string
   string->list
   (listof/enum char/enum)))

(define integer/enum
  (sum/enum nats
	    (map/enum (λ (n) (- (+ n 1)))
		      (λ (n) (- (- n) 1))
		      nats)))

(define real/enum (from-list/enum '(0.0 1.5 123.112354)))
(define num/enum
  (sum/enum natural/enum
	    integer/enum
	    real/enum))

(define bool/enum
  (from-list/enum '(#t #f)))

(define var/enum
  (map/enum
   (compose string->symbol list->string list)
   (compose car string->list symbol->string)
   char/enum))

(define any/enum
  (sum/enum num/enum
	    string/enum
	    bool/enum
	    var/enum))

(define (plug-hole ctx term)
  (let loop ([ctx ctx])
    (match
     ctx
     ['hole term]
     [`(,ts ...)
      (map loop ts)]
     [x x])))

(module+ test
	 (require rackunit)

	 (define rep `(,(make-nt 'r
				 `(,(make-rhs `(list variable
						     (repeat variable #f #f)))))))
	 (define rs (hash-ref (lang-enumerators rep) 'r))
	 (test-begin
	  (check-equal? (enum-ith rs 0) '(a))
	  (check-equal? (size rs) +inf.f))
	 (define λc `(,(make-nt 'e
				`(,(make-rhs `(list (repeat variable #f #f)))
				  ,(make-rhs `(list λ variable (nt e)))
				  ,(make-rhs `(list (nt e) (nt e)))))))
	 (define les (lang-enumerators λc))
	 (define es (hash-ref les 'e))
	 (check-equal? (size es) +inf.f))
