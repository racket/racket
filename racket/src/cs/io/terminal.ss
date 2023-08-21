
(export
 terminal-init
 terminal-read-char
 terminal-write-char
 terminal-char-width
 terminal-set-color
 terminal-flush
 terminal-get-screen-size
 terminal-raw-mode
 terminal-postoutput-mode
 terminal-automargin-mode
 terminal-signal-mode
 terminal-nanosleep
 terminal-pause
 terminal-get-clipboard
 terminal-move-cursor
 terminal-clear
 terminal-scroll-reverse
 terminal-bell
 terminal-carriage-return
 terminal-line-feed)

(define terminal-init (foreign-procedure "(cs)ee_init_term" (iptr iptr) boolean))
(define terminal-read-char (foreign-procedure "(cs)ee_read_char" (boolean) scheme-object))
(define terminal-write-char (foreign-procedure "(cs)ee_write_char" (wchar_t) int))
(define terminal-char-width (foreign-procedure "(cs)ee_char_width" (wchar_t) int))
(define terminal-set-color (foreign-procedure "(cs)ee_set_color" (int boolean) void))
(define terminal-flush (foreign-procedure "(cs)ee_flush" () void))
(define terminal-get-screen-size (foreign-procedure "(cs)ee_get_screen_size" () scheme-object))

(define terminal-raw-mode
  (let ()
    (define raw-mode (foreign-procedure "(cs)ee_raw" () void))
    (define no-raw-mode (foreign-procedure "(cs)ee_noraw" () void))
    (lambda (on?)
      (if on?
          (raw-mode)
          (no-raw-mode)))))

(define terminal-postoutput-mode
  (let ()
    (define postoutput-mode (foreign-procedure "(cs)ee_postoutput" () void))
    (define no-postoutput-mode (foreign-procedure "(cs)ee_nopostoutput" () void))
    (lambda (on?)
      (if on?
          (postoutput-mode)
          (no-postoutput-mode)))))

(define terminal-signal-mode
  (let ()
    (define signal-mode (foreign-procedure "(cs)ee_signal" () void))
    (define no-signal-mode (foreign-procedure "(cs)ee_nosignal" () void))
    (lambda (on?)
      (if on?
          (signal-mode)
          (no-signal-mode)))))

(define terminal-automargin-mode
  (let ()
    (define enter-am-mode (foreign-procedure "(cs)ee_enter_am_mode" () void))
    (define exit-am-mode (foreign-procedure "(cs)ee_exit_am_mode" () void))
    (lambda (on?)
      (if on?
          (enter-am-mode)
          (exit-am-mode)))))
    
(define terminal-nanosleep (foreign-procedure "(cs)ee_nanosleep" (unsigned-32 unsigned-32) void))
(define terminal-pause (foreign-procedure "(cs)ee_pause" () void))
(define terminal-get-clipboard (foreign-procedure "(cs)ee_get_clipboard" () scheme-object))

(define terminal-move-cursor
  (let ()
    (define move-cursor-up (foreign-procedure "(cs)ee_up" (integer-32) void))
    (define move-cursor-down (foreign-procedure "(cs)ee_down" (integer-32) void))
    (define move-cursor-left (foreign-procedure "(cs)ee_left" (integer-32) void))
    (define move-cursor-right (foreign-procedure "(cs)ee_right" (integer-32) void))
    (lambda (dir amt)
      (case dir
        [(up) (move-cursor-up amt)]
        [(down) (move-cursor-down amt)]
        [(left) (move-cursor-left amt)]
        [(right) (move-cursor-right amt)]))))

(define terminal-clear
  (let ()
    (define clear-eol (foreign-procedure "(cs)ee_clr_eol" () void))
    (define clear-eos (foreign-procedure "(cs)ee_clr_eos" () void))
    (define clear-screen (foreign-procedure "(cs)ee_clear_screen" () void))
    (lambda (what)
      (case what
        [(eol) (clear-eol)]
        [(eos) (clear-eos)]
        [(screen) (clear-screen)]))))

(define terminal-scroll-reverse (foreign-procedure "(cs)ee_scroll_reverse" (integer-32) void))
(define terminal-bell (foreign-procedure "(cs)ee_bell" () void))
(define terminal-carriage-return (foreign-procedure "(cs)ee_carriage_return" () void))
(define terminal-line-feed (foreign-procedure "(cs)ee_line_feed" () void))
