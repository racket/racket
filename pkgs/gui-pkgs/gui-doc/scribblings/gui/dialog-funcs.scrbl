#lang scribble/doc
@(require "common.rkt" (for-label mrlib/path-dialog))

@title{Dialogs}


These functions get input from the user and/or display
 messages.



@defproc[(get-file [message (or/c label-string? #f) #f]
                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                   [directory (or/c path-string? #f) #f]
                   [filename (or/c path-string? #f) #f]
                   [extension (or/c string? #f) #f]
                   [style (listof (or/c 'packages 'enter-packages 'common)) null]
                   [filters (listof (list/c string? string?)) '(("Any" "*.*"))]
                   [#:dialog-mixin dialog-mixin (make-mixin-contract path-dialog%) (λ (x) x)])
         (or/c path? #f)]{

Obtains a file pathname from the user via the platform-specific
 standard (modal) dialog, using @racket[parent] as the parent window if
 it is specified, and using @racket[message] as a message at the top of
 the dialog if it is not @racket[#f].

The result is @racket[#f] if the user cancels the dialog, the selected
 pathname otherwise. The returned pathname may or may not exist,
 although the style of the dialog is directed towards selecting
 existing files.

If @racket[directory] is not @racket[#f], it is used as the starting
 directory for the file selector (otherwise the starting directory is
 chosen automatically in a platform-specific manner, usually based on
 the current directory and the user's interactions in previous calls
 to @racket[get-file], @racket[put-file], etc.). If
 @racket[filename] is not @racket[#f], it is used as the default filename
 when appropriate, and it should @italic{not} contain a directory path
 prefix.

Under Windows, if @racket[extension] is not @racket[#f], the returned path
 will use the extension if the user does not supply one; the
 @racket[extension] string should not contain a period. The extension is
 ignored on other platforms.

The @racket[style] list can contain @racket['common], a
 platform-independent version of the dialog is used instead of a
 native dialog.  On Mac OS X, if the @racket[style] list
 contains @racket['packages], a user is allowed to select a package
 directory, which is a directory with a special suffix (e.g.,
 ``.app'') that the Finder normally displays like a file.  If the list
 contains @racket['enter-packages], a user is allowed to select a file
 within a package directory. If the list contains both
 @racket['packages] and @racket['enter-packages], the former is ignored.

On Windows and Unix, @racket[filters] determines a set of filters from
 which the user can choose in the dialog. Each element of the
 @racket[filters] list contains two strings: a description of the filter
 as seen by the user, and a filter pattern matched against file names.
 Pattern strings can be a simple ``glob'' pattern, or a number of glob
 patterns separated by a @litchar[";"] character.
 On Unix, a @racket["*.*"] pattern is implicitly replaced with @racket["*"].
 On Mac OS X, suffix names are extracted from all globs that match a
 fixed suffix (e.g., two suffixes of @racket["foo"] and @racket["bar"]
 are extracted from a @racket["*.foo;*.bar;*.baz*"] pattern), and files
 that have any of these suffixes in any filter are selectable; a
 @racket["*.*"] glob makes all files available for selection.

The @racket[dialog-mixin] is applied to @racket[path-dialog%] before
creating an instance of the class for this dialog.

See also @racket[path-dialog%] for a richer interface.

}

@defproc[(get-file-list [message (or/c label-string? #f) #f]
                        [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                        [directory (or/c path-string? #f) #f]
                        [filename (or/c path-string? #f) #f]
                        [extension (or/c string? #f) #f]
                        [style (listof (or/c 'packages 'enter-packages 'common)) null]
                        [filters (listof (list/c string? string?)) '(("Any" "*.*"))]
                        [#:dialog-mixin dialog-mixin (make-mixin-contract path-dialog%) (λ (x) x)])
         (or/c (listof path?) #f)]{
Like
@racket[get-file], except that the user can select multiple files, and the
 result is either a list of file paths of @racket[#f].

}

@defproc[(put-file [message (or/c label-string? #f) #f]
                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                   [directory (or/c path-string? #f) #f]
                   [filename (or/c path-string? #f) #f]
                   [extension (or/c string? #f) #f]
                   [style (listof (or/c 'packages 'enter-packages 'common)) null]
                   [filters (listof (list/c string? string?)) '(("Any" "*.*"))]
                   [#:dialog-mixin dialog-mixin (make-mixin-contract path-dialog%) (λ (x) x)])
         (or/c path? #f)]{

Obtains a file pathname from the user via the platform-specific
 standard (modal) dialog, using @racket[parent] as the parent window if
 it is specified, and using @racket[message] as a message at the top of
 the dialog if it is not @racket[#f].

The result is @racket[#f] if the user cancels the dialog, the selected
 pathname otherwise. The returned pathname may or may not exist,
 although the style of the dialog is directed towards creating a new
 file.

If @racket[directory] is not @racket[#f], it is used as the starting
 directory for the file selector (otherwise the starting directory is
 chosen automatically in a platform-specific manner, usually based on
 the current directory and the user's interactions in previous calls
 to @racket[get-file], @racket[put-file], etc.). If
 @racket[filename] is not @racket[#f], it is used as the default filename
 when appropriate, and it should @italic{not} contain a directory path
 prefix.

On Windows, if @racket[extension] is not @racket[#f], the returned path
 will get a default extension if the user does not supply one. If
 @racket[extension] is the empty string, then the extension is derived
 from the user's @racket[filters] choice if the corresponding pattern is
 of the form @racket[(string-append "*." extension)]; if the pattern is
 @racket["*.*"], then no default extension is added. Finally, if
 @racket[extension] is any string other than the empty string,
 @racket[extension] is used as the default extension when the user's
 @racket[filters] choice has the pattern @racket["*.*"].  Meanwhile, the
 @racket[filters] argument has the same format and auxiliary role as for
 @racket[get-file]. In particular, if the only pattern in @racket[filters]
 is @racket[(string-append "*." extension)], then the result pathname is guaranteed
 to have an extension mapping @racket[extension].

On Mac OS X 10.5 and later, if @racket[extension] is not
 @racket[#f] or @racket[""], the returned path will get a default extension if the
 user does not supply one.  If @racket[filters] contains as
 @racket["*.*"] pattern, then the user can supply any extension that
 is recognized by the system; otherwise, the extension on the returned
 path will be either @racket[extension] or @racket[_other-extension]
 for any @racket[(string-append "*."  _other-extension)] pattern in
 @racket[filters]. In particular, if the only pattern in
 @racket[filters] is empty or contains only @racket[(string-append
 "*." extension)], then the result pathname is guaranteed to have an
 extension mapping @racket[extension].

On Mac OS X versions before 10.5, the returned path will get a
 default extension only if @racket[extension] is not @racket[#f], 
 @racket[extension] is not @racket[""], and
 @racket[filters] contains only @racket[(string-append "*."
 extension)].

On Unix, @racket[extension] is ignored, and @racket[filters] is used
 to filter the visible list of files as in @racket[get-file].

The @racket[style] list is treated as for @racket[get-file].

The @racket[dialog-mixin] is applied to @racket[path-dialog%] before
creating an instance of the class for this dialog.

See also @racket[path-dialog%] for a richer interface.
}

@defproc[(get-directory [message (or/c label-string? #f) #f]
                        [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                        [directory (or/c path-string? #f) #f]
                        [style (listof (or/c 'enter-packages 'common)) null]
                        [#:dialog-mixin dialog-mixin (make-mixin-contract path-dialog%) (λ (x) x)])
         (or/c path #f)]{

Obtains a directory pathname from the user via the platform-specific
 standard (modal) dialog, using @racket[parent] as the parent window if
 it is specified.

If @racket[directory] is not @racket[#f], it is used on some platforms as
 the starting directory for the directory selector (otherwise the
 starting directory is chosen automatically in a platform-specific
 manner, usually based on the current directory and the user's
 interactions in previous calls to @racket[get-file],
 @racket[put-file], etc.).

The @racket[style] argument is treated as for
@racket[get-file], except that only @racket['common] or @racket['enter-packages] can be
specified.  The latter
 matters only on Mac OS X, where @racket['enter-packages]
 enables the user to select package directory or a directory within a
 package. A package is a directory with a special suffix (e.g.,
 ``.app'') that the Finder normally displays like a file.

The @racket[dialog-mixin] is applied to @racket[path-dialog%] before
creating an instance of the class for this dialog.

See also @racket[path-dialog%] for a richer interface.

}

@defproc[(message-box [title label-string?]
                      [message string?]
                      [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                      [style (listof (or/c 'ok 'ok-cancel 'yes-no 
                                           'caution 'stop 'no-icon))
                             '(ok)]
                      [#:dialog-mixin dialog-mixin (make-mixin-contract dialog%) values])
         (or/c 'ok 'cancel 'yes 'no)]{

See also @racket[message-box/custom].

Displays a message to the user in a (modal) dialog, using
 @racket[parent] as the parent window if it is specified. The dialog's
 title is @racket[title]. The @racket[message] string can be arbitrarily
 long, and can contain explicit linefeeds or carriage returns for
 breaking lines.

The style must include exactly one of the following:
@itemize[

 @item{@racket['ok] --- the dialog only has an @onscreen{OK} button
 and always returns @racket['ok].}

 @item{@racket['ok-cancel] --- the message dialog has
 @onscreen{Cancel} and @onscreen{OK} buttons. If the user clicks
 @onscreen{Cancel}, the result is @racket['cancel], otherwise the
 result is @racket['ok].}

 @item{@racket['yes-no] --- the message dialog has @onscreen{Yes} and
 @onscreen{No} buttons. If the user clicks @onscreen{Yes}, the result
 is @racket['yes], otherwise the result is @racket['no]. Note: instead
 of a @onscreen{Yes}/@onscreen{No} dialog, best-practice GUI design is
 to use @racket[message-box/custom] and give the buttons meaningful
 labels, so that the user does not have to read the message text
 carefully to make a selection.}

]

In addition, @racket[style] can contain @racket['caution] to make the
 dialog use a caution icon instead of the application (or generic
 ``info'') icon, @racket['stop] to make the dialog use a stop icon, or
 @racket['no-icon] to suppress the icon. If @racket[style] contains
 multiple of @racket['caution], @racket['stop], and @racket['no-icon],
 then @racket['no-icon] takes precedence followed by @racket['stop].

The class that implements the dialog provides a @racket[get-message]
 method that takes no arguments and returns the text of the message as
 a string. (The dialog is accessible through the
 @racket[get-top-level-windows] function.)

The @racket[message-box] function can be called in a thread other
 than the handler thread of the relevant eventspace (i.e., the eventspace of
 @racket[parent], or the current eventspace if @racket[parent] is @racket[#f]), in which case the
 current thread blocks while the dialog runs on the handler thread.
 
The @racket[dialog-mixin] argument is applied to the class that implements the dialog
before the dialog is created. 
}

@defproc[(message-box/custom [title label-string?]
                             [message string?]
                             [button1-label (or/c label-string? (is-a?/c bitmap%) #f)]
                             [button2-label (or/c label-string? (is-a?/c bitmap%) #f)]
                             [button3-label (or/c label-string? (is-a?/c bitmap%) #f)]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                             [style (listof (or/c 'stop 'caution 'no-icon 'number-order 
                                                  'disallow-close 'no-default 
                                                  'default=1 'default=2 'default=3))
                                   '(no-default)]
                             [close-result any/c #f]
                             [#:dialog-mixin dialog-mixin (make-mixin-contract dialog%) values])
         (or/c 1 2 3 close-result)]{

Displays a message to the user in a (modal) dialog, using
 @racket[parent] as the parent window if it is specified. The dialog's
 title is @racket[title]. The @racket[message] string can be arbitrarily
 long, and can contain explicit linefeeds or carriage returns for
 breaking lines.

The dialog contains up to three buttons for the user to click. The
 buttons have the labels @racket[button1-label],
 @racket[button2-label], and @racket[button3-label], where @racket[#f] for a
 label indicates that the button should be hidden.

If the user clicks the button labelled @racket[button1-label], a @racket[1]
 is returned, and so on for @racket[2] and @racket[3]. If the user closes
 the dialog some other way---which is only allowed when @racket[style]
 does not contain @racket['disallow-close]---then the result is the
 value of @racket[close-result]. For example, the user can usually close
 a dialog by typing an Escape. Often, @racket[2] is an appropriate value
 for @racket[close-result], especially when Button 2 is a @onscreen{Cancel}
 button.

If @racket[style] does not include @racket['number-order], the order of
 the buttons is platform-specific, and labels should be assigned to
 the buttons based on their role:
@itemize[

 @item{Button 1 is the normal action, and it is usually the default
 button. For example, if the dialog has an @onscreen{OK} button, it is
 this one. On Windows, this button is leftmost; on Unix and Mac OS
 X, it is rightmost. (See also
 @racket[system-position-ok-before-cancel?].) Use this button for
 dialogs that contain only one button.}

 @item{Button 2 is next to Button 1, and it often plays the role of
 @onscreen{Cancel} (even when the default action is to cancel, such as
 when confirming a file replacement).}

 @item{Button 3 tends to be separated from the other two (on
 Mac OS X, it is left-aligned in the dialog). Use this button only
 for three-button dialogs.}

]
Despite the above guidelines, any combination of visible buttons is
 allowed in the dialog.

If @racket[style] includes @racket['number-order], then the buttons are
 displayed in the dialog left-to-right with equal spacing between all
 buttons, though aligned within the dialog (centered or right-aligned)
 in a platform-specific manner. Use @racket['number-order] sparingly.

The @racket[style] list must contain exactly one of @racket['default=1],
 @racket['default=2], @racket['default=3], and @racket['no-default] to
 determine which button (if any) is the default. The default button is
 ``clicked'' when the user types Return. If @racket['default=]@racket[n]
 is supplied but button @racket[n] has no label, then it is equivalent to
 @racket['no-default].

In addition, @racket[style] can contain @racket['caution],
 @racket['stop], or @racket['no-icon] to adjust the icon that appears
 n the dialog, the same for @racket[message-box].

The class that implements the dialog provides a @racket[get-message]
 method that takes no arguments and returns the text of the message as
 a string. (The dialog is accessible through the
@racket[get-top-level-windows] function.)

The @racket[message-box/custom] function can be called in a thread
 other than the handler thread of the relevant eventspace (i.e., the eventspace of
 @racket[parent], or the current eventspace if @racket[parent] is @racket[#f]), in which case the
 current thread blocks while the dialog runs on the handler thread.
 
The @racket[dialog-mixin] argument is applied to the class that implements the dialog
before the dialog is created. 
}

@defproc[(message+check-box [title label-string?]
                            [message string?]
                            [check-label label-string?]
                            [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                            [style (listof (or/c 'ok 'ok-cancel 'yes-no 
                                                 'caution 'stop 'no-icon 'checked))
                              '(ok)]
                            [#:dialog-mixin dialog-mixin (make-mixin-contract dialog%) values])
         (values (or/c 'ok 'cancel 'yes 'no) boolean?)]{

See also @racket[message+check-box/custom].

Like @racket[message-box], except that

@itemize[
 @item{the dialog contains a check box whose label is @racket[check-label];}
 @item{the result is two values: the @racket[message-box] result, and a
       boolean indicating whether the box was checked; and}
 @item{@racket[style] can contain @racket['checked] to indicate that the check box
       should be initially checked.}
]}

@defproc[(message+check-box/custom [title label-string?]
                                   [message string?]
                                   [check-label label-string?]
                                   [button1-label (or/c label-string? (is-a?/c bitmap%) #f)]
                                   [button2-label (or/c label-string? (is-a?/c bitmap%) #f)]
                                   [button3-label (or/c label-string? (is-a?/c bitmap%) #f)]
                                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                                   [style (listof (or/c 'stop 'caution 'no-icon 'number-order 
                                                        'disallow-close 'no-default 
                                                        'default=1 'default=2 'default=3))
                                          '(no-default)]
                                   [close-result any/c #f]
                                   [#:dialog-mixin dialog-mixin (make-mixin-contract dialog%) values])
         (or/c 1 2 3 (λ (x) (eq? x close-result)))]{

Like @racket[message-box/custom], except that
@itemize[
 @item{the dialog contains a check box whose label is @racket[check-label];}
 @item{the result is two values: the @racket[message-box] result, and a
       boolean indicating whether the box was checked; and}
 @item{@racket[style] can contain @racket['checked] to indicate that the check box
       should be initially checked.}
]
}

@defproc[(get-text-from-user [title label-string?]
                             [message (or/c label-string? #f)]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                             [init-val string? ""]
                             [style (listof (or/c 'password 'disallow-invalid)) null]
                             [#:validate validate (-> string? boolean?)]
                             [#:dialog-mixin dialog-mixin (make-mixin-contract dialog%) values]) 
         (or/c string? #f)]{

Gets a text string from the user via a modal dialog, using
 @racket[parent] as the parent window, if it is specified. The dialog's
 title is @racket[title]. The dialog's text field is labelled with
 @racket[message] and initialized to @racket[init-val] (but @racket[init-val]
 does not determine the size of the dialog).

The result is @racket[#f] if the user cancels the dialog, the
 user-provided string otherwise.

If @racket[style] includes @racket['password], the dialog's text field
 draws each character of its content using a generic symbol, instead
 of the actual character.

The @racket[validate] function is called each time the text field changed,
with the contents of the text field. If it returns @racket[#f], the background
of the text is colored pink. If @racket['disallow-invalid] is included in
@racket[style], the @onscreen{Ok} button is disabled whenever the text
background is pink.
 
The @racket[dialog-mixin] argument is applied to the class that implements the dialog
before the dialog is created. 
}

@defproc[(get-choices-from-user [title label-string?]
                                [message (or/c label-string? #f)]
                                [choices (listof label-string?)]
                                [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                                [init-choices (listof exact-nonnegative-integer?) null]
                                [style (listof (or/c 'single 'multiple 'extended)) '(single)])
         (or/c (listof exact-nonnegative-integer?) #f)]{

Gets a list box selection from the user via a modal dialog, using
 @racket[parent] as the parent window if it is specified. The dialog's
 title is @racket[title]. The dialog's list box is labelled with
 @racket[message] and initialized by selecting the items in
 @racket[init-choices]. 

The style must contain exactly one of @indexed-racket['single],
 @indexed-racket['multiple], or @indexed-racket['extended]. The styles have
 the same meaning as for creating a @racket[list-box%] object. (For
 the single-selection style, only the last selection in
 @racket[init-choices] matters.)

The result is @racket[#f] if the user cancels the dialog, the
 list of selections otherwise.



}

@defproc[(get-color-from-user [message (or/c label-string? #f) #f]
                              [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                              [init-color (or/c (is-a?/c color%) #f) #f]
                              [style (listof 'alpha) null])
         (or/c (is-a?/c color%) #f)]{

Lets the user select a color though the platform-specific
 (modal) dialog, using @racket[parent] as the parent window if it is
 specified. The @racket[message] string is displayed as a prompt in the
 dialog if possible. If @racket[init-color] is provided, the dialog is
 initialized to the given color.

The result is @racket[#f] if the user cancels the dialog, the selected
 color otherwise.

If @racket[style] contains @racket['alpha], then the user is present with
a field for filling in the alpha field of the resulting @racket[color%] object.
If it does not, then the alpha component of @racket[init-color] is ignored,
and the result always has alpha of @racket[1.0].
}

@defproc[(get-font-from-user [message (or/c label-string? #f) #f]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                             [init-font (or/c (is-a?/c font%) #f) #f]
                             [style null? null])
         (or/c (is-a?/c font%) #f)]{

Lets the user select a font though the platform-specific
 (modal) dialog, using @racket[parent] as the parent window if it is
 specified. The @racket[message] string is displayed as a prompt in the
 dialog if possible. If @racket[init-font] is provided, the dialog is
 initialized to the given font.

@italicptyStyleNote[@racket[style]]

The result is @racket[#f] if the user cancels the dialog, the selected
 font otherwise.



}

@defproc[(get-ps-setup-from-user [message (or/c label-string? #f) #f]
                                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                                 [init-setup (or/c (is-a?/c ps-setup%) #f) #f]
                                 [style null? null])
         (or/c (is-a?/c ps-setup%) #f)]{

Lets the user select a PostScript configuration though a (modal)
 dialog, using @racket[parent] as the parent window if it is
 specified. The @racket[message] string is displayed as a prompt in the
 dialog. If @racket[init-setup] is provided, the dialog is initialized to
 the given configuration, otherwise the current configuration from
@racket[current-ps-setup]  is used.

@italicptyStyleNote[@racket[style]]

The result is @racket[#f] if the user cancels the dialog, , a
 @racket[ps-setup%] object that encapsulates the selected PostScript
 configuration otherwise.



}

@defproc[(get-page-setup-from-user [message (or/c label-string? #f) #f]
                                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f]
                                   [init-setup (or/c (is-a?/c ps-setup%) #f) #f]
                                   [style null? null])
         (or/c (is-a?/c ps-setup%) #f)]{

Like
@racket[get-ps-setup-from-user], but the dialog configures page layout for native printing
 with @racket[printer-dc%]. A dialog is shown only if
@racket[can-get-page-setup-from-user?] returns @racket[#t], otherwise no dialog is shown and the result
 is @racket[#f].

The @racket[parent] argument is used as the parent window for a dialog if
 it is specified. The @racket[message] string might be displayed as a
 prompt in the dialog. If @racket[init-setup] is provided, the dialog is
 initialized to the given configuration, otherwise the current
 configuration from
@racket[current-ps-setup]  is used.

@italicptyStyleNote[@racket[style]]

The result is @racket[#f] if the user cancels the dialog, a
 @racket[ps-setup%] object that encapsulates the selected
 configuration otherwise.



}

@defproc[(can-get-page-setup-from-user?)
         boolean?]{
Returns @racket[#t] if the current platform supports a
 page-layout dialog for use with @racket[printer-dc%] printing. 
 Currently, all platforms support a page-layout dialog.

}
