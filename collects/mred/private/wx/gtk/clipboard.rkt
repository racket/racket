#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw/unsafe/bstr
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/local.rkt"
         "../common/freeze.rkt"
          "utils.rkt"
          "types.rkt"
          "pixbuf.rkt")

(provide 
 (protect-out clipboard-driver%
              has-x-selection?
              _GtkSelectionData
              gtk_selection_data_get_length
              gtk_selection_data_get_data
	      primary-atom
	      get-selection-eventspace))

(define (has-x-selection?) #t)

(define _GtkClipboard (_cpointer 'GtkClipboard))
(define _GtkDisplay _pointer)

;; Recent versions of Gtk provide function calls to
;;  access data, but use structure when the functions are
;;  not available
(define-cstruct _GtkSelectionDataT ([selection _GdkAtom]
				    [target _GdkAtom]
				    [type _GdkAtom]
				    [format _int]
				    [data _pointer]
				    [length _int]
				    [display _GtkDisplay]))
				  
(define _GtkSelectionData _GtkSelectionDataT-pointer)

(define-gdk gdk_atom_intern (_fun _string _gboolean -> _GdkAtom))

(define-gtk gtk_clipboard_get (_fun _GdkAtom -> _GtkClipboard))
(define-gtk gtk_clipboard_set_with_data (_fun _GtkClipboard _pointer _uint
                                              _fpointer _fpointer 
                                              _pointer
                                              -> _void))
(define-gtk gtk_selection_data_set (_fun _GtkSelectionData
                                         _GdkAtom
                                         _int
                                         _bytes
                                         _int
                                         -> _void))
(define-gtk gtk_selection_data_free (_fun _GtkSelectionData -> _void))
(define-gtk gtk_selection_data_get_length (_fun _GtkSelectionData -> _int)
  #:fail (lambda () GtkSelectionDataT-length))
(define-gtk gtk_selection_data_get_data (_fun _GtkSelectionData -> _pointer)
  #:fail (lambda () GtkSelectionDataT-data))

(define-cstruct _GtkTargetEntry ([target _pointer]
                                 [flags _uint]
                                 [info _uint]))

(define (get-data cb sel-data info self-box)
  (send (ptr-ref self-box _scheme) provide-data info sel-data))
(define get_data
  (function-ptr get-data (_fun #:atomic? #t _GtkClipboard _GtkSelectionData _int _pointer -> _void)))

(define (clear-owner cb self-box)
  (send (ptr-ref self-box _scheme) replaced self-box))
(define clear_owner
  (function-ptr clear-owner (_fun #:atomic? #t _GtkClipboard _pointer -> _void)))

(define primary-atom (gdk_atom_intern "PRIMARY" #t))
(define clipboard-atom (gdk_atom_intern "CLIPBOARD" #t))

(define the-x-selection-driver #f)

;; ----------------------------------------

(define _request-fun (_fun #:atomic? #t _GtkClipboard (_or-null _GtkSelectionData) _pointer -> _void))
(define _request-string-fun (_fun #:atomic? #t _GtkClipboard _string _pointer -> _void))
(define _request-image-fun (_fun #:atomic? #t _GtkClipboard _GdkPixbuf _pointer -> _void))

(define (handle-receipt backref data convert)
  (let ([l (ptr-ref backref _racket)])
    (free-immobile-cell backref)
    (set-box! (car l) (and data (convert data)))
    (semaphore-post (cdr l))))

(define (make-request-backref)
  (let ([l (cons (box #f) (make-semaphore))])
    (values l (malloc-immobile-cell l))))

(define (wait-request-backref l)
  (semaphore-wait (cdr l))
  (unbox (car l)))

(define (request-received cb data backref)
  (handle-receipt backref 
		  data
		  (lambda (v)
		    (let ([bstr (scheme_make_sized_byte_string
				 (gtk_selection_data_get_data v)
				 (gtk_selection_data_get_length v)
				 1)])
		      bstr))))

(define (string-request-received cb str backref)
  (handle-receipt backref 
		  str
		  (lambda (str) str)))

(define (image-request-received cb pix backref)
  (handle-receipt backref 
		  pix
		  pixbuf->bitmap))

(define request_received (function-ptr request-received _request-fun))
(define string_request_received (function-ptr string-request-received _request-string-fun))
(define image_request_received (function-ptr image-request-received _request-image-fun))

(define-gtk gtk_clipboard_request_contents 
  (_fun _GtkClipboard _GdkAtom (_fpointer = request_received) _pointer -> _void))
(define-gtk gtk_clipboard_request_text 
  (_fun _GtkClipboard (_fpointer = string_request_received) _pointer -> _void))
(define-gtk gtk_clipboard_request_image 
  (_fun _GtkClipboard (_fpointer = image_request_received) _pointer -> _void))
(define-gtk gtk_clipboard_set_image 
  (_fun _GtkClipboard _GdkPixbuf -> _void))

;; ----------------------------------------

(defclass clipboard-driver% object%
  (init-field [x-selection? #f])

  (when x-selection?
    (set! the-x-selection-driver this))

  (define client #f)
  (define client-data #f)
  (define client-types #f)
  (define client-orig-types #f)

  (define cb (gtk_clipboard_get
              (if x-selection?
                  primary-atom
		  clipboard-atom)))
  (define self-box #f)

  (define/public (get-client) client)

  (define/public (set-client c orig-types)
    (let ([all-data (if x-selection?
                        ;; In X selection mode, get the data on demand:
			#f
                        ;; In clipboard mode, we can get the data
                        ;; now, so it's ready if anyone asks:
			(for/list ([t (in-list orig-types)])
			  (send c get-data t)))]
	  [types (for/list ([t (in-list orig-types)])
		   (if (equal? t "TEXT")
		       "UTF8_STRING"
		       t))])
      (let-values ([(orig-types types all-data)
                    ;; For "TEXT", provide "UTF8_STRING", "STRING", and "TEXT":
                    (if (member "TEXT" orig-types)
                        (values (append orig-types (list "TEXT" "TEXT"))
                                (append types (list "STRING" "TEXT"))
                                (and all-data (append all-data
                                                      (let loop ([all-data all-data]
                                                                 [orig-types orig-types])
                                                        (if (equal? "TEXT" (car orig-types))
                                                            (list (car all-data) (car all-data))
                                                            (loop (cdr all-data) (cdr orig-types)))))))
                        (values orig-types types all-data))])
        (let ([target-strings (malloc 'raw _byte (+ (length types)
                                                    (apply + (map string-utf-8-length types))))]
              [targets (malloc _GtkTargetEntry (length types))])
          (for/fold ([offset 0]) ([str (in-list types)]
                                  [i (in-naturals)])
            (let ([t (ptr-add targets i _GtkTargetEntry)])
              (cpointer-push-tag! t 'GtkTargetEntry)
              (set-GtkTargetEntry-target! t (ptr-add target-strings offset))
              (set-GtkTargetEntry-flags! t 0)
              (set-GtkTargetEntry-info! t i))
            (let ([bstr (string->bytes/utf-8 str)])
              (memcpy target-strings offset bstr 0 (bytes-length bstr))
              (let ([offset (+ offset (bytes-length bstr))])
                (ptr-set! (ptr-add target-strings offset) _byte 0)
                (+ offset 1))))
          (set! client c)
          (set! client-data all-data)
          (set! client-types types)
          (set! client-orig-types orig-types)
          
          (atomically
           (let ([this-box (malloc-immobile-cell this)])
             (set! self-box this-box)
             (gtk_clipboard_set_with_data cb
                                          targets 
                                          (length types)
                                          get_data 
                                          clear_owner
                                          this-box)))

          (free target-strings)))))

  (define/public (replaced s-box)
    ;; In atomic mode
    (when (ptr-equal? s-box self-box)
      (set! self-box #f)
      (let ([c client])
        (when c
          (set! client #f)
          (set! client-data #f)
          (set! client-types #f)
          (set! client-orig-types #f)
          (queue-event (send c get-client-eventspace)
                       (lambda ()
                         (send c on-replaced))))))
    (free-immobile-cell s-box))
  
  (define/public (provide-data i sel-data)
    ;; In atomic mode; if it's the selection (not clipboard),
    ;; then hopefully we're in the right eventspace
    (let ([bstr (if client
		    (if client-data
			(list-ref client-data i)
			(constrained-reply (send client get-client-eventspace)
					   (lambda () 
                                             (send client get-data 
                                                   (list-ref client-orig-types i)))
					   #f))
                    #f)])
      (when bstr
        (let ([bstr (if (string? bstr)
                        (string->bytes/utf-8 bstr)
                        bstr)])
          (gtk_selection_data_set sel-data
                                  (gdk_atom_intern (list-ref client-types i) #t)
                                  8
                                  bstr
                                  (bytes-length bstr))))))

  (define/private (self-data data-format)
    ;; Due to the way we block for X-selection data and 
    ;; provide only when the request arrives in the right
    ;; eventspace, we handle self-X-selection specially:
    (and x-selection?
         self-box
         (let ([c client]
               [types client-types]
               [orig-types client-orig-types])
           (for/or ([t (in-list types)]
                    [o (in-list orig-types)])
             (and (equal? t data-format)
                  (let ([e (send c get-client-eventspace)])
                    (if (eq? (current-eventspace) e)
                        (send client get-data t)
                        (let ([s #f]
                              [done (make-semaphore)])
                          (parameterize ([current-eventspace e])
                            (queue-callback
                             (lambda ()
                               (set! s (send client get-data t))
                               (semaphore-post done))))
                          (sync/timeout 0.1 done)
                          s))))))))

  (define/public (get-data data-format)
    (let* ([data-format (if (equal? data-format "TEXT")
			    "UTF8_STRING"
			    data-format)]
	   [atom (gdk_atom_intern data-format #t)])
      (or (self-data data-format)
          (wait-request-backref 
           (atomically
            (let-values ([(l backref) (make-request-backref)])
              (gtk_clipboard_request_contents cb atom backref)
              l))))))

  (define/public (get-text-data)
    (or (let ([s (self-data "UTF8_STRING")])
          (and s (bytes->string/utf-8 s #\?)))
        (wait-request-backref 
         (atomically
          (let-values ([(l backref) (make-request-backref)])
            (gtk_clipboard_request_text cb backref)
            l)))
        ""))

  (define/public (get-bitmap-data)
    (wait-request-backref 
     (atomically
      (let-values ([(l backref) (make-request-backref)])
	(gtk_clipboard_request_image cb backref)
	l))))

  (define/public (set-bitmap-data bm timestamp)
    (define pb (bitmap->pixbuf bm))
    (gtk_clipboard_set_image cb pb))
  
  (super-new))

(define (get-selection-eventspace)
  (and the-x-selection-driver
       (let ([c (send the-x-selection-driver get-client)])
	 (and c
	      (send c get-client-eventspace)))))
