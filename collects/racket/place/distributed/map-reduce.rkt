#lang racket/base
(require racket/place
         racket/match
         racket/list
         racket/pretty
         racket/place/distributed)


(provide make-map-reduce-workers
         map-reduce
         map-reduce-worker
         default-sorter)

(define (map-coalesce-values kvl)
  (let loop ([sl kvl]
             [curk null]
             [curv null]
             [nl null])
    (match sl
      [(cons (cons slhk slhv) slt)
       (if (null? curk)
           (loop slt slhk (list slhv) nl)
           (if (equal? slhk curk)
               (loop slt curk (cons slhv curv) nl)
               (loop sl null null (cons (cons curk (reverse curv)) nl))))]
      [(list)
       (reverse
         (if (null? curk)
             nl
             (cons (cons curk (reverse curv)) nl)))])))

(define (coalesce-values kvl kvl2 less-than)
  (let loop ([sl (sort (append kvl kvl2) less-than)]
             [curk null]
             [curv null]
             [nl null])
    (match sl
      [(cons (cons slhk slhv) slt)
       (if (null? curk)
           (loop slt slhk slhv nl)
           (if (equal? slhk curk)
               (loop slt curk (append curv slhv) nl)
               (loop sl null null (cons (cons curk curv) nl))))]
      [(list)
       (reverse
         (if (null? curk)
             nl
             (cons (cons curk curv) nl)))])))


(define (->module-path x)
  (match x
    [(list 'file fn) (list 'file (bytes->string/locale fn))]
    [(cons h t) (cons (->module-path h) (->module-path t))]
    [(? bytes?) (bytes->path x)]
    [else x]))

(define (apply-dynamic-require lst)
  (match-define (list mp sym) lst)
  (dynamic-require (->module-path mp) sym))

(define (map-reduce-worker ch)
  (let loop ([map-vals null])
    (define msg (place-channel-get ch))
    (match msg
      [(list (list 'map mapper key-less-than task) rch)
        (define nmv1 ((apply-dynamic-require mapper) task))
        (define less-than (apply-dynamic-require key-less-than))
        (define nmv2 (sort nmv1 less-than))
        (define nmv (map-coalesce-values nmv2))
        (place-channel-put rch (list 'reduce-ready))
        (loop (cons rch nmv))]
      [(list (list 'reduce-to reducer sorter addr) rch)
        (define reduce-ch (mr-connect-to ch (take addr 2) (third addr)))
        (place-channel-put reduce-ch (list 'reduce reducer sorter (cdr map-vals)))
        (place-channel-put rch (list 'reduce-done))
        (loop null)]
      [(list (list 'reduce reducer sorter kvs) rch)
        (define less-than (apply-dynamic-require sorter))
        (define nmvc (coalesce-values (cdr map-vals) kvs less-than))
        (define nmv ((apply-dynamic-require reducer) nmvc))
        (place-channel-put (car map-vals) (list 'reduce-ready))
        (loop (cons (car map-vals) nmv))]
      [(list (list 'get-results) rch)
        (place-channel-put rch (list 'results (cdr map-vals)))
        (loop null)]
    )))

(define (i->place-name i)
  (string->symbol (string-append "mpw" (number->string i))))


(define (make-map-reduce-workers config)
  (define nodes (spawn-nodes/join/local config))
  (for ([n nodes]
        [i (in-naturals)])
    (supervise-place-at n
                        (->module-path (quote-module-path))
                        'map-reduce-worker
                        #:named (i->place-name)))
  nodes)

(define (default-sorter a b)
  (cond
    [(number? a) (< a b)]
    [(string? a) (string<? a b)]
    [(pair? a) (default-sorter (car a) (car b))]
    [else (raise (format "Cannot sort ~a" a))]))

(define (map-reduce nodes config tasks mapper reducer
                    #:sorter [sorter (list (quote-module-path) 'default-sorter)]
                    #:combiner [combiner #f]
                    #:outputer [outputer #f])
  (define-values (mrth ch)
    (start-message-router/thread
      #:nodes nodes))

  (define simple-config
    (for/list ([c config]
               [i (in-naturals)])
      (append c (list (i->place-name i)))))

  (define connections
    (for/list ([c simple-config])
      (match-define (list-rest host port name rest) c)
      (define npch (mr-connect-to ch (list host port) name))
      (list c npch)))

  (define result
    (let loop ([ts tasks]
               [idle-mappers connections]
               [mapping null]
               [ready-to-reduce null]
               [reducing null])
      ;; (printf "STATE\n")
      ;; (pretty-print (list ts idle-mappers mapping ready-to-reduce reducing))
      ;; (flush-output)
      (match (list ts idle-mappers mapping ready-to-reduce reducing)
        [(list (cons tsh tst) (cons imh imt) mapping rtr r)
         (*channel-put (second imh) (list 'map mapper sorter (list tsh)))
         (loop tst imt (cons imh mapping) rtr r)]
        [(list ts im m (cons rtr1 (cons rtr2 rtrt)) r)
         (*channel-put (second rtr1) (list 'reduce-to reducer sorter (first rtr2)))
         (loop ts im m rtrt (cons rtr1 (cons rtr2 r)))]
        [(list (list) im (list) (list rtr) (list))
         (*channel-put (second rtr) (list 'get-results))
         (second (*channel-get (second rtr)))]
        [else ; wait
         (apply sync/enable-break
                (for/list ([m (append mapping reducing)])
                  (wrap-evt (second m)
                    (lambda (e)
                      (match e
                        [(list 'reduce-ready)
                         (loop ts idle-mappers (remove m mapping) (cons m ready-to-reduce) (remove m reducing))]
                        [(list 'reduce-done)
                         (loop ts (cons m idle-mappers) mapping ready-to-reduce (remove m reducing))]
                        [else
                         (raise (format "Unknown response message ~a" e))])))))])))

  (or (and outputer ((apply-dynamic-require outputer) result))
      result))
