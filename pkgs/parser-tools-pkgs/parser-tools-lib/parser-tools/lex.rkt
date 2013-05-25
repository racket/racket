(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See docs.

  (require-for-syntax mzlib/list
                      syntax/stx
                      syntax/define
                      syntax/boundmap
                      "private-lex/util.rkt"
                      "private-lex/actions.rkt"
                      "private-lex/front.rkt"
                      "private-lex/unicode-chars.rkt")

  (require mzlib/stxparam
           syntax/readerr
           "private-lex/token.rkt")

  (provide lexer lexer-src-pos define-lex-abbrev define-lex-abbrevs define-lex-trans
           
           ;; Dealing with tokens and related structures 
           define-tokens define-empty-tokens token-name token-value token?
           (struct position (offset line col))
           (struct position-token (token start-pos end-pos))
           
           ;; File path for highlighting errors while lexing
           file-path
           
           ;; Lex abbrevs for unicode char sets.  See mzscheme manual section 3.4.
           any-char any-string nothing alphabetic lower-case upper-case title-case
           numeric symbolic punctuation graphic whitespace blank iso-control

           ;; A regular expression operator
           char-set)
  
  ;; wrap-action: syntax-object src-pos? -> syntax-object
  (define-for-syntax (wrap-action action src-pos?)
    (with-syntax ((action-stx 
                   (if src-pos?
                       #`(let/ec ret
                           (syntax-parameterize 
                            ((return-without-pos (make-rename-transformer #'ret)))
                            (make-position-token #,action start-pos end-pos)))
                        action)))
      (syntax/loc action
        (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
          (syntax-parameterize 
           ((start-pos (make-rename-transformer #'start-pos-p))
            (end-pos (make-rename-transformer #'end-pos-p))
            (lexeme (make-rename-transformer #'lexeme-p))
            (input-port (make-rename-transformer #'input-port-p)))
           action-stx)))))
        
  (define-for-syntax (make-lexer-trans src-pos?)
    (lambda (stx)
      (syntax-case stx ()
        ((_ re-act ...)
         (begin
           (for-each
            (lambda (x)
              (syntax-case x ()
                ((re act) (void))
                (_ (raise-syntax-error #f
                                       "not a regular expression / action pair"
                                       stx
                                       x))))
            (syntax->list (syntax (re-act ...))))
           (let* ((spec/re-act-lst
                   (syntax->list (syntax (re-act ...))))
                  (eof-act
                   (get-special-action spec/re-act-lst #'eof #''eof))
                  (spec-act 
                   (get-special-action spec/re-act-lst #'special #'(void)))
                  (spec-comment-act 
                   (get-special-action spec/re-act-lst #'special-comment #'#f))
                  (ids (list #'special #'special-comment #'eof))
                  (re-act-lst
                   (filter
                    (lambda (spec/re-act)
                      (syntax-case spec/re-act ()
                        (((special) act)
                           (not (ormap
                                 (lambda (x)
                                   (and (identifier? #'special)
                                        (module-or-top-identifier=? (syntax special) x)))
                                 ids)))
                        (_ #t)))
                    spec/re-act-lst))
		  (name-lst (map (lambda (x) (datum->syntax-object #f (gensym))) re-act-lst))
		  (act-lst (map (lambda (x) (stx-car (stx-cdr x))) re-act-lst))
		  (re-actname-lst (map (lambda (re-act name)
					 (list (stx-car re-act)
					       name))
				       re-act-lst
				       name-lst)))
             (when (null? spec/re-act-lst)
               (raise-syntax-error (if src-pos? 'lexer/src-pos 'lexer) "expected at least one action" stx))
             (let-values (((trans start action-names no-look disappeared-uses)
                           (build-lexer re-actname-lst)))
               (when (vector-ref action-names start) ;; Start state is final
                 (unless (and 
                          ;; All the successor states are final
                          (andmap (lambda (x) (vector-ref action-names (vector-ref x 2)))
                                      (vector->list (vector-ref trans start)))
                          ;; Each character has a successor state
                          (let loop ((check 0)
                                     (nexts (vector->list (vector-ref trans start))))
                            (cond
                              ((null? nexts) #f)
                              (else
                               (let ((next (car nexts)))
                                 (and (= (vector-ref next 0) check)
                                      (let ((next-check (vector-ref next 1)))
                                        (or (>= next-check max-char-num)
                                            (loop (add1 next-check) (cdr nexts))))))))))
                   (eprintf "Warning: lexer at ~a can accept the empty string.\n" stx)))
               (with-syntax ((start-state-stx start)
                             (trans-table-stx trans)
                             (no-lookahead-stx no-look)
			     ((name ...) name-lst)
			     ((act ...) (map (lambda (a)
					       (wrap-action a src-pos?))
					     act-lst))
			     ((act-name ...) (vector->list action-names))
			     (spec-act-stx
                              (wrap-action spec-act src-pos?))
                             (has-comment-act?-stx 
                              (if (syntax-e spec-comment-act) #t #f))
                             (spec-comment-act-stx
                              (wrap-action spec-comment-act src-pos?))
                             (eof-act-stx (wrap-action eof-act src-pos?)))
                 (syntax-property
                  (syntax/loc stx
		    (let ([name act] ...)
                      (let ([proc
                             (lexer-body start-state-stx 
                                         trans-table-stx
                                         (vector act-name ...)
                                         no-lookahead-stx
                                         spec-act-stx
                                         has-comment-act?-stx
                                         spec-comment-act-stx
                                         eof-act-stx)])
                        ;; reverse eta to get named procedures:
                        (lambda (port) (proc port)))))
                  'disappeared-use
                  disappeared-uses)))))))))

  (define-syntax lexer (make-lexer-trans #f))
  (define-syntax lexer-src-pos (make-lexer-trans #t))
    
  (define-syntax (define-lex-abbrev stx)
    (syntax-case stx ()
      ((_ name re)
       (identifier? (syntax name))
       (syntax/loc stx
        (define-syntax name
          (make-lex-abbrev (lambda () (quote-syntax re))))))
      (_ 
       (raise-syntax-error
        #f
        "form should be (define-lex-abbrev name re)"
        stx))))

  (define-syntax (define-lex-abbrevs stx)
    (syntax-case stx ()
      ((_ x ...)
       (with-syntax (((abbrev ...)
                      (map 
                       (lambda (a)
                         (syntax-case a ()
                           ((name re)
                            (identifier? (syntax name))
                            (syntax/loc a (define-lex-abbrev name re)))
                           (_ (raise-syntax-error
                               #f
                               "form should be (define-lex-abbrevs (name re) ...)"
                               stx
                               a))))
                       (syntax->list (syntax (x ...))))))
         (syntax/loc stx (begin abbrev ...))))
      (_
       (raise-syntax-error
        #f
        "form should be (define-lex-abbrevs (name re) ...)"
        stx))))

  (define-syntax (define-lex-trans stx)
    (syntax-case stx ()
      ((_ name-form body-form)
       (let-values (((name body)
                     (normalize-definition (syntax (define-syntax name-form body-form)) #'lambda)))
         
         #`(define-syntax #,name 
             (let ((func #,body))
               (unless (procedure? func)
                 (raise-syntax-error 'define-lex-trans "expected a procedure as the transformer, got ~e" func))
               (unless (procedure-arity-includes? func 1)
                 (raise-syntax-error 'define-lex-trans "expected a procedure that accepts 1 argument as the transformer, got ~e" func))
               (make-lex-trans func)))))
      (_
       (raise-syntax-error
        #f
        "form should be (define-lex-trans name transformer)"
        stx))))
       

  (define (get-next-state-helper char min max table)
    (if (>= min max)
        #f
        (let* ((try (quotient (+ min max) 2))
               (el (vector-ref table try))
               (r1 (vector-ref el 0))
               (r2 (vector-ref el 1)))
          (cond
            ((and (>= char r1) (<= char r2)) (vector-ref el 2))
            ((< char r1) (get-next-state-helper char min try table))
            (else (get-next-state-helper char (add1 try) max table))))))
               
          
          
  
  (define (get-next-state char table)
    (if table
        (get-next-state-helper char 0 (vector-length table) table)
        #f))
  
  (define (lexer-body start-state trans-table actions no-lookahead special-action
                      has-special-comment-action? special-comment-action eof-action)
    (letrec ((lexer
              (lambda (ip)
                (let ((first-pos (get-position ip))
                      (first-char (peek-char-or-special ip 0)))
                  ;(printf "(peek-char-or-special port 0) = ~e\n" first-char)
                  (cond
                    ((eof-object? first-char)
                     (do-match ip first-pos eof-action (read-char-or-special ip)))
                    ((special-comment? first-char)
                     (read-char-or-special ip)
                     (cond
                       (has-special-comment-action?
                        (do-match ip first-pos special-comment-action #f))
                       (else (lexer ip))))
                    ((not (char? first-char))
                     (do-match ip first-pos special-action (read-char-or-special ip)))
                    (else
                     (let lexer-loop (
                                      ;; current-state
                                      (state start-state)
                                      ;; the character to transition on
                                      (char first-char)
                                      ;; action for the longest match seen thus far
                                      ;; including a match at the current state
                                      (longest-match-action 
                                       (vector-ref actions start-state))
                                      ;; how many bytes precede char
                                      (length-bytes 0)
                                      ;; how many characters have been read
                                      ;; including the one just read
                                      (length-chars 1)
                                      ;; how many characters are in the longest match
                                      (longest-match-length 0))
                       (let ((next-state 
                              (cond
                                ((not (char? char)) #f)
                                (else (get-next-state (char->integer char)
                                                      (vector-ref trans-table state))))))
                         (cond
                           ((not next-state)
                            (check-match ip first-pos longest-match-length
                                         length-chars longest-match-action))
                           ((vector-ref no-lookahead next-state)
                            (let ((act (vector-ref actions next-state)))
                              (check-match ip 
                                           first-pos 
                                           (if act length-chars longest-match-length)
                                           length-chars
                                           (if act act longest-match-action))))
                           (else
                            (let* ((act (vector-ref actions next-state))
                                   (next-length-bytes (+ (char-utf-8-length char) length-bytes))
                                   (next-char (peek-char-or-special ip next-length-bytes)))
                              #;(printf "(peek-char-or-special port ~e) = ~e\n"
                                      next-length-bytes next-char)
                              (lexer-loop next-state 
                                          next-char
                                          (if act
                                              act
                                              longest-match-action)
                                          next-length-bytes
                                          (add1 length-chars)
                                          (if act
                                              length-chars
                                              longest-match-length)))))))))))))
      (lambda (ip)
        (unless (input-port? ip)
          (raise-argument-error 
           'lexer 
           "input-port?"
           0
           ip))
        (lexer ip))))
      
  (define (check-match lb first-pos longest-match-length length longest-match-action)
    (unless longest-match-action
      (let* ((match (read-string length lb))
	     (end-pos (get-position lb)))
	(raise-read-error
	 (format "lexer: No match found in input starting with: ~a" match)
	 (file-path)
	 (position-line first-pos)
	 (position-col first-pos)
	 (position-offset first-pos)
	 (- (position-offset end-pos) (position-offset first-pos)))))
    (let ((match (read-string longest-match-length lb)))
      ;(printf "(read-string ~e port) = ~e\n" longest-match-length match)
      (do-match lb first-pos longest-match-action match)))

  (define file-path (make-parameter #f))

  (define (do-match ip first-pos action value)
    #;(printf "(action ~a ~a ~a ~a)\n" 
            (position-offset first-pos) (position-offset (get-position ip)) value ip)
    (action first-pos (get-position ip) value ip))
  
  (define (get-position ip)
    (let-values (((line col off) (port-next-location ip)))
      (make-position off line col)))

  (define-syntax (create-unicode-abbrevs stx)
    (syntax-case stx ()
      ((_ ctxt)
       (with-syntax (((ranges ...) (map (lambda (range)
                                          `(union ,@(map (lambda (x)
                                                           `(char-range ,(integer->char (car x))
                                                                        ,(integer->char (cdr x))))
                                                         range)))
                                        (list (force alphabetic-ranges)
                                              (force lower-case-ranges)
                                              (force upper-case-ranges)
                                              (force title-case-ranges)
                                              (force numeric-ranges)
                                              (force symbolic-ranges)
                                              (force punctuation-ranges)
                                              (force graphic-ranges)
                                              (force whitespace-ranges)
                                              (force blank-ranges)
                                              (force iso-control-ranges))))
                     ((names ...) (map (lambda (sym)
                                         (datum->syntax-object (syntax ctxt) sym #f))
                                       '(alphabetic
                                         lower-case
                                         upper-case
                                         title-case
                                         numeric
                                         symbolic
                                         punctuation
                                         graphic
                                         whitespace
                                         blank
                                         iso-control))))
         (syntax (define-lex-abbrevs (names ranges) ...))))))
                                             
  (define-lex-abbrev any-char (char-complement (union)))
  (define-lex-abbrev any-string (intersection))
  (define-lex-abbrev nothing (union))
  (create-unicode-abbrevs #'here)
  
  (define-lex-trans (char-set stx)
    (syntax-case stx ()
      ((_ str)
       (string? (syntax-e (syntax str)))
       (with-syntax (((char ...) (string->list (syntax-e (syntax str)))))
         (syntax (union char ...))))))

  (define-syntax provide-lex-keyword
    (syntax-rules ()
      [(_ id ...)
       (begin
	 (define-syntax-parameter id
           (make-set!-transformer
            (lambda (stx)
              (raise-syntax-error
               #f
               (format "use of a lexer keyword (~a) is not in an appropriate lexer action"
                       'id)
               stx))))
	 ...
	 (provide id ...))]))
  
  (provide-lex-keyword start-pos end-pos lexeme input-port return-without-pos)

)
