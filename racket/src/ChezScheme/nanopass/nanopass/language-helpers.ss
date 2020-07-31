(library (nanopass language-helpers)
  (export prune-language-helper)
  (import (rnrs) (nanopass records))

  (define tspec->ts-syntax
    (lambda (tspec)
      (with-syntax ([(meta-vars ...) (tspec-meta-vars tspec)]
                    [type (tspec-type tspec)])
        #'(type (meta-vars ...)))))

  (define ntspec->nts-syntax
    (lambda (ntspec)
      (with-syntax ([(meta-vars ...) (ntspec-meta-vars ntspec)]
                    [name (ntspec-name ntspec)]
                    [(prods ...) (map alt-syn (ntspec-alts ntspec))])
        #'(name (meta-vars ...) prods ...))))

  (define prune-language-helper
    (lambda (l)
      (let ([entry (language-entry-ntspec l)])
        (let ([nt* (list (nonterm-id->ntspec 'prune-language entry (language-ntspecs l)))])
          (let loop ([nt* nt*] [ts '()] [nts '()])
            (if (null? nt*)
                (with-syntax ([(ts ...) (map tspec->ts-syntax ts)]
                              [(nts ...) (map ntspec->nts-syntax nts)])
                  #'((ts ...) (nts ...)))
                (let ([nt (car nt*)] [nt* (cdr nt*)])
                  (let ([nts (cons nt nts)])
                    (let inner-loop ([prod* (ntspec-alts nt)] [nt* nt*] [ts ts])
                      (if (null? prod*)
                          (loop nt* ts nts)
                          (let ([prod (car prod*)])
                            (cond
                              [(terminal-alt? prod)
                               (inner-loop (cdr prod*) nt*
                                 (let ([tspec (terminal-alt-tspec prod)])
                                   (if (memq tspec ts) ts (cons tspec ts))))]
                              [(nonterminal-alt? prod)
                               (inner-loop (cdr prod*) 
                                 (let ([ntspec (nonterminal-alt-ntspec prod)])
                                   (if (or (memq ntspec nt*) (memq ntspec nts)) nt* (cons ntspec nt*)))
                                 ts)]
                              [(pair-alt? prod)
                               (let inner-inner-loop ([flds (pair-alt-field-names prod)] [nt* nt*] [ts ts])
                                 (if (null? flds)
                                     (inner-loop (cdr prod*) nt* ts)
                                     (let ([fld (car flds)])
                                       (cond
                                         [(meta-name->tspec fld (language-tspecs l)) =>
                                          (lambda (tspec)
                                            (inner-inner-loop (cdr flds) nt*
                                              (if (memq tspec ts) ts (cons tspec ts))))]
                                         [(meta-name->ntspec fld (language-ntspecs l)) =>
                                          (lambda (ntspec)
                                            (inner-inner-loop (cdr flds)
                                              (if (or (memq ntspec nt*) (memq ntspec nts)) nt* (cons ntspec nt*))
                                              ts))]))))])))))))))))))


