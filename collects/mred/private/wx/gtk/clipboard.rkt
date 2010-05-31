#lang scheme/base
(require scheme/class
         scheme/foreign
         "../../syntax.rkt"
         "../common/queue.rkt"
         "../common/local.rkt"
         "../common/bstr.rkt"
          "utils.rkt"
          "types.rkt")
(unsafe!)

(provide clipboard-driver%
         has-x-selection?)

(define (has-x-selection?) #t)

(define _GdkAtom _int)
(define _GtkClipboard (_cpointer 'GtkClipboard))
(define _GtkDisplay _pointer)
(define _GtkSelectionData (_cpointer 'GtkSelectionData))

(define-gdk gdk_atom_intern (_fun _string _gboolean -> _GdkAtom))

(define-gtk gtk_clipboard_get (_fun _GdkAtom -> _GtkClipboard))
(define-gtk gtk_clipboard_set_with_data (_fun _GtkClipboard _pointer _uint
                                              _fpointer _fpointer 
                                              _pointer
                                              -> _void))
(define-gtk gtk_selection_data_set (_fun _GtkSelectionData
                                         _GdkAtom
                                         _int
                                         _ubyte
                                         _int
                                         -> _void))
(define-gtk gtk_clipboard_wait_for_contents (_fun _GtkClipboard _GdkAtom -> (_or-null _GtkSelectionData)))
(define-gtk gtk_selection_data_free (_fun _GtkSelectionData -> _void))
(define-gtk gtk_selection_data_get_length (_fun _GtkSelectionData -> _int))
(define-gtk gtk_selection_data_get_data (_fun _GtkSelectionData -> _pointer))
(define-gtk gtk_clipboard_wait_for_text (_fun _GtkClipboard -> _string))

(define-cstruct _GtkTargetEntry ([target _pointer]
                                 [flags _uint]
                                 [info _uint]))

(define (get-data cb sel-data info self-box)
  (send (ptr-ref self-box _scheme) provide-data info sel-data))
(define get_data
  (function-ptr get-data (_fun #:atomic? #t _GtkClipboard _GtkSelectionData _int _pointer -> _void)))

(define (clear-owner cb self-box)
  (send (ptr-ref self-box _scheme) replaced))
(define clear_owner
  (function-ptr clear-owner (_fun #:atomic? #t _GtkClipboard _pointer -> _void)))


(defclass clipboard-driver% object%
  (init-field [x-selection? #f])

  (define client #f)
  (define client-data #f)

  (define cb (gtk_clipboard_get
              (if x-selection?
                  (gdk_atom_intern "CLIPBOARD" #t)
                  (gdk_atom_intern "PRIMARY" #t))))
  (define self-box (malloc-immobile-cell this))

  (define/public (get-client) client)

  (define/public (set-client c types)
    (if x-selection?
        ;; For now, we can't call it on demand, so we don't call at all:
        (queue-event (send c get-client-eventspace)
                     (lambda ()
                       (send c on-replaced)))
        ;; In clipboard mode (as opposed to X selection), we can get the data
        ;; now, so it's ready if anyone asks:
        (let ([all-data (for/list ([t (in-list types)])
                          (send c get-data t))])
          (let ([target-strings (malloc 'raw _byte (+ (length types) (apply + (map string-utf-8-length types))))]
                [targets (malloc _GtkTargetEntry (length types))])
            (for/fold ([offset 0])
                ([str (in-list types)]
                 [i (in-naturals)])
              (let ([t (cast (ptr-add targets i _GtkTargetEntry) _pointer _GtkTargetEntry-pointer)])
                (set-GtkTargetEntry-target! t (ptr-add target-strings offset))
                (set-GtkTargetEntry-flags! t i)
                (set-GtkTargetEntry-info! t 0))
              (let ([bstr (string->bytes/utf-8 str)])
                (memcpy target-strings offset bstr 0 (bytes-length bstr))
                (let ([offset (+ offset (bytes-length bstr))])
                  (ptr-set! (ptr-add target-strings offset) _byte 0)
                  (+ offset 1))))
            (set! client c)
            (set! client-data all-data)
            (gtk_clipboard_set_with_data cb
                                         targets 
                                         (length types)
                                         get_data 
                                         clear_owner
                                         self-box)
            (free target-strings)))))

  (define/public (replaced)
    ;; Called in Gtk event-dispatch thread --- atomically with respect
    ;; to any other thread
    (let ([c client])
      (when c
        (set! client #f)
        (set! client-data #f)
        (queue-event (send c get-client-eventspace)
                     (lambda ()
                       (send c on-replaced))))))

  (define/public (provide-data i sel-data)
    ;; Called in Gtk event-dispatch thread --- atomically with respect
    ;; to any other thread
    (let ([bstr (if client
                    (list-ref client-data i)
                    #"")])
        (gtk_selection_data_set sel-data
                                (gdk_atom_intern "UTF8_STRING" #t)
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
                            bstr)))])
      (process (gtk_clipboard_wait_for_contents cb (gdk_atom_intern format #t)))))

  (define/public (get-text-data)
    (gtk_clipboard_wait_for_text cb))

  (super-new))

