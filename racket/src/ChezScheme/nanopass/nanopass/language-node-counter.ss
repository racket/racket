;;; Copyright (c) 2000-2015 Andrew W. Keep
;;; See the accompanying file Copyright for details

(library (nanopass language-node-counter)
  (export define-language-node-counter)
  (import (rnrs) (nanopass records) (nanopass helpers))

  (define-syntax define-language-node-counter
    (lambda (x)
      (define make-ntspec-counter-assoc
        (lambda (tid)
          (lambda (ntspec)
            (cons ntspec (construct-unique-id tid "count-" (ntspec-name ntspec))))))
      (syntax-case x ()
        [(_ name lang)
         (and (identifier? #'name) (identifier? #'lang))
         (lambda (r)
           (let ([l-pair (r #'lang)])
             (unless l-pair (syntax-violation 'define-language-node-counter (format "unknown language ~s" (datum lang)) #'name x))
             (let ([l (car l-pair)])
               (let ([ntspecs (language-ntspecs l)] [tspecs (language-tspecs l)])
                 (let ([counter-names (map (make-ntspec-counter-assoc #'name) ntspecs)])
                   (define lookup-counter
                     (lambda (ntspec)
                       (cond
                         [(assq ntspec counter-names) => cdr]
                         [else (syntax-violation 'define-language-node-counter
                                 (format "unexpected nonterminal ~s in language ~s"
                                   (syntax->datum (ntspec-name ntspec)) (datum lang))
                                 #'name x)])))
                   (define build-counter-proc
                     (lambda (proc-name l)
                       (lambda (ntspec)
                         (let loop ([alt* (ntspec-alts ntspec)] [term* '()] [nonterm* '()] [pair* '()])
                           (if (null? alt*)
                               #`(lambda (x)
                                   (cond
                                     #,@term*
                                     #,@pair*
                                     #,@nonterm*
                                     [else (errorf who "unrecognized term ~s" x)]))
                               (let ([alt (car alt*)] [alt* (cdr alt*)])
                                 (cond
                                   [(terminal-alt? alt)
                                    (loop alt*
                                          (cons #`[(#,(tspec-pred (terminal-alt-tspec alt)) x) 1] term*)
                                          nonterm* pair*)]
                                   [(nonterminal-alt? alt)
                                    (let ([ntspec (nonterminal-alt-ntspec alt)])
                                      (loop alt* term* 
                                            (cons #`[(#,(ntspec-all-pred ntspec) x)
                                                     (#,(lookup-counter ntspec) x)]
                                                  nonterm*)
                                            pair*))]
                                   [(pair-alt? alt)
                                    (let inner-loop ([fld* (pair-alt-field-names alt)]
                                                     [lvl* (pair-alt-field-levels alt)]
                                                     [maybe?* (pair-alt-field-maybes alt)]
                                                     [acc* (pair-alt-accessors alt)]
                                                     [rec* '()])
                                      (if (null? fld*)
                                          (loop alt* term* nonterm*
                                                (cons #`[(#,(pair-alt-pred alt) x) (+ 1 #,@rec*)] pair*))
                                          (inner-loop (cdr fld*) (cdr lvl*) (cdr maybe?*) (cdr acc*)
                                                      (cons 
                                                        (let ([fld (car fld*)] [maybe? (car maybe?*)] [acc (car acc*)])
                                                          (let ([spec (find-spec fld l)])
                                                            (if (ntspec? spec)
                                                                #`(let ([x (#,acc x)])
                                                                    #,(let loop ([lvl (car lvl*)] [outer-most? #t])
                                                                        (if (fx=? lvl 0)
                                                                            (if maybe?
                                                                                (if outer-most?
                                                                                    #`(if x (#,(lookup-counter spec) x) 0)
                                                                                    #`(+ a (if x (#,(lookup-counter spec) x) 0)))
                                                                                (if outer-most?
                                                                                    #`(#,(lookup-counter spec) x)
                                                                                    #`(+ a (#,(lookup-counter spec) x))))
                                                                            (if outer-most?
                                                                                #`(fold-left
                                                                                    (lambda (a x) #,(loop (- lvl 1) #f))
                                                                                    0 x)
                                                                                #`(fold-left
                                                                                    (lambda (a x) #,(loop (- lvl 1) #f))
                                                                                    a x)))))
                                                                0)))
                                                        rec*))))]
                                   [else (syntax-violation 'define-language-node-counter
                                           (format "unrecognized alt ~s building language node counter" (syntax->datum (alt-syn alt)))
                                           proc-name x)])))))))
                   (with-syntax ([(ntspec? ...) (map ntspec-pred ntspecs)]
                                 [(proc-name ...) (map cdr counter-names)]
                                 [(tspec? ...) (map tspec-pred tspecs)]
                                 [(proc ...) (map (build-counter-proc #'name l) ntspecs)])
                     #'(define-who name
                         (lambda (x)
                           (define proc-name proc) ...
                           (cond
                             [(ntspec? x) (proc-name x)] ...
                             [(tspec? x) 1] ...
                             [else (errorf who "unrecognized language record ~s" x)])))))))))]))))
