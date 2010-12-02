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
(define _GtkSelectionData (_cpointer 'GtkSelectionData))

(define _freed-string (make-ctype _pointer 
                                  (lambda (s) s)
                                  (lambda (p)
                                    (let ([s (cast p _pointer _string)])
                                      (g_free p)
                                      s))))

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
(define-gtk gtk_clipboard_wait_for_contents (_fun _GtkClipboard _GdkAtom -> (_or-null _GtkSelectionData)))
(define-gtk gtk_selection_data_free (_fun _GtkSelectionData -> _void))
(define-gtk gtk_selection_data_get_length (_fun _GtkSelectionData -> _int)
  #:fail (lambda () GtkSelectionDataT-length))
(define-gtk gtk_selection_data_get_data (_fun _GtkSelectionData -> _pointer)
  #:fail (lambda () GtkSelectionDataT-data))
(define-gtk gtk_clipboard_wait_for_text (_fun _GtkClipboard -> _freed-string))
(define-gtk gtk_clipboard_wait_for_image (_fun _GtkClipboard -> _GdkPixbuf)
  #:wrap (allocator gobject-unref))

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
    ;; In clipboard mode (as opposed to X selection), we can get the data
    ;; now, so it's ready if anyone asks:
    (let ([all-data (if x-selection?
			#f
			(for/list ([t (in-list orig-types)])
			  (send c get-data t)))]
	  [types (for/list ([t (in-list orig-types)])
		   (if (equal? t "TEXT")
		       "UTF8_STRING"
		       t))])
      (let ([target-strings (malloc 'raw _byte (+ (length types)
						  (apply + (map string-utf-8-length types))))]
	    [targets (malloc _GtkTargetEntry (length types))])
	(for/fold ([offset 0]) ([str (in-list types)]
				[i (in-naturals)])
		  (let ([t (cast (ptr-add targets i _GtkTargetEntry) _pointer _GtkTargetEntry-pointer)])
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

	(free target-strings))))

  (define/public (replaced s-box)
    ;; Called in Gtk event-dispatch thread --- atomically with respect
    ;; to any other thread
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
					   #""))
                    #"")])
        (gtk_selection_data_set sel-data
                                (gdk_atom_intern (list-ref client-types i) #t)
                                8
                                bstr
                                (bytes-length bstr))))

  (define/public (get-data format)
    (let ([process (lambda (v)
                     (and v
                          (let ([bstr (scheme_make_sized_byte_string
                                       (gtk_selection_data_get_data v)
                                       (gtk_selection_data_get_length v)
                                       1)])
                            (gtk_selection_data_free v)
                            bstr)))]
	  [format (if (equal? format "TEXT")
		      "UTF8_STRING"
		      format)])
      (process (gtk_clipboard_wait_for_contents cb (gdk_atom_intern format #t)))))

  (define/public (get-text-data)
    (or (gtk_clipboard_wait_for_text cb) ""))

  (define/public (get-bitmap-data)
    (let ([pixbuf (gtk_clipboard_wait_for_image cb)])
      (and pixbuf
           (begin0
            (pixbuf->bitmap pixbuf)
            (gobject-unref pixbuf)))))
  
  (super-new))

(define (get-selection-eventspace)
  (and the-x-selection-driver
       (let ([c (send the-x-selection-driver get-client)])
	 (and c
	      (send c get-client-eventspace)))))
