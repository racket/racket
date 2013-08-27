#lang racket/base

(require ffi/unsafe)

(define libxmms (ffi-lib "libxmms"))

(provide session)
(define session
  (make-parameter
   0 (lambda (x)
       (if (integer? x)
          x
          (error 'xmms-session "expecting an integer, got ~s" x)))))

;; used for playlist position values
(define _pos _int)

;; number of equalizer bands
(define eq-bands 10)

;; used for getting the default session from the session parameter
(define-fun-syntax _session
  (syntax-id-rules (_session)
    [_session (type: _int pre: (session))]))

(define-syntax defxmms
  (syntax-rules (:)
    [(_ name : x ...)
     (begin
       (provide name)
       (define name
         (get-ffi-obj
          (regexp-replaces
           'name '((#rx"-" "_") (#rx"[?]$" "") (#rx"^" "xmms_remote_")))
          libxmms (_fun x ...))))]))

(defxmms playlist : (files enqueue?) ::
                    _session
                    (files : (_list i _string))
                    (_int = (length files))
                    (enqueue? : _bool)
                    -> _void)
(defxmms get-version : _session -> _int)
;; The second argument is a GList (see glib/glist.h) which requires structs,
;; but the playlist function is sufficient (looks like this is for glib code).
;; (defxmms playlist-add : _session "GList * list" -> _void)
(defxmms playlist-delete : _session _pos -> _void)
(defxmms play  : _session -> _void)
(defxmms pause : _session -> _void)
(defxmms stop  : _session -> _void)
(defxmms is-playing? : _session -> _bool)
(defxmms is-paused?  : _session -> _bool)
(defxmms get-playlist-pos : _session -> _pos)
(defxmms set-playlist-pos : _session _pos -> _void)
(defxmms get-playlist-length : _session -> _pos)
(defxmms playlist-clear : _session -> _void)
(defxmms get-output-time : _session -> _int)
(defxmms jump-to-time : _session _int -> _void)
(defxmms get-volume      : _session (l : (_ptr o _int)) (r : (_ptr o _int))
                           -> _void -> (list l r))
(defxmms get-main-volume : _session -> _int)
(defxmms get-balance     : _session -> _int)
(defxmms set-volume      : _session (l : _int) (r : _int) -> _void)
(defxmms set-main-volume : _session _int -> _void)
(defxmms set-balance     : _session _int -> _void)
(defxmms get-skin : _session -> _file)
(defxmms set-skin : _session _file -> _void)
(defxmms get-playlist-file  : _session _pos -> _string)
(defxmms get-playlist-title : _session _pos -> _string)
(defxmms get-playlist-time  : _session _pos -> _int)
(defxmms get-info : _session
                    (rate : (_ptr o _int))
                    (freq : (_ptr o _int))
                    (nch  : (_ptr o _int))
                    -> _void -> (list rate freq nch))
(defxmms main-win-toggle : _session (show? : _bool) -> _void)
(defxmms pl-win-toggle   : _session (show? : _bool) -> _void)
(defxmms eq-win-toggle   : _session (show? : _bool) -> _void)
(defxmms is-main-win?    : _session -> _bool)
(defxmms is-pl-win?      : _session -> _bool)
(defxmms is-eq-win?      : _session -> _bool)
(defxmms show-prefs-box : _session -> _void)
(defxmms toggle-aot : _session (ontop? : _bool) -> _void)
(defxmms eject : _session -> _void)
(defxmms playlist-prev : _session -> _void)
(defxmms playlist-next : _session -> _void)
(defxmms playlist-add-url-string : _session _string -> _void)
(defxmms is-running? : _session -> _bool)
(defxmms toggle-repeat  : _session -> _void)
(defxmms toggle-shuffle : _session -> _void)
(defxmms is-repeat?     : _session -> _bool)
(defxmms is-shuffle?    : _session -> _bool)
(defxmms get-eq : _session
                  (preamp : (_ptr o _float))
                  (bands : (_ptr o _pointer))
                  -> _void
                  -> (cons preamp (cblock->list bands _float eq-bands)))
(defxmms get-eq-preamp : _session -> _float)
(defxmms get-eq-band : _session (band : _int) -> _float)
(defxmms set-eq : (l) ::
                  _session
                  (preamp : _float = (car l))
                  (bands : (_list i _float) = (cdr l))
                  -> _void)
(defxmms set-eq-preamp : _session (preamp : _float) -> _void)
(defxmms set-eq-band   : _session (band : _int) _float -> _void)
(defxmms quit : _session -> _void)
(defxmms play-pause : _session -> _void)
(defxmms playlist-ins-url-string : _session _string _pos -> _void)
