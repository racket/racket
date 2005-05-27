(module features mzscheme
  (provide feature-present?
	   feature->require-clause)

  (define *feature-alist*
    '())

  (define (srfi-id? id)
    (let ((string-id (symbol->string id)))
      (and (> (string-length string-id) 5)
	   (string=? "srfi-"
		     (substring string-id 0 5)))))

  (define (srfi-id->filename srfi-id)
    (let ((string-id (symbol->string srfi-id)))
      (string-append (substring string-id 5 (string-length string-id))
		     ".ss")))

  (define (srfi-id-present? srfi-id)
    (file-exists? (build-path (collection-path "srfi")
			      (srfi-id->filename srfi-id))))

  (define (feature-present? id)
    (or (and (srfi-id? id) (srfi-id-present? id))
	(and (assq id *feature-alist*) #t)))

  (define (feature->require-clause id)
    (if (and (srfi-id? id) (srfi-id-present? id))
	(cons 'lib (list (srfi-id->filename id) "srfi"))
	(cdr (assq id *feature-alist*)))))
