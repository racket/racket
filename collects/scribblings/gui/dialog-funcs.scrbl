#lang scribble/doc
@(require "common.ss" 
          (for-label mrlib/path-dialog))

@title{Dialogs}


These functions get input from the user and/or display
 messages.



@defproc[(get-file [message (or/c string? false/c) #f]
                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                   [directory (or/c path-string? false/c) #f]
                   [filename (or/c path-string? false/c) #f]
                   [extension (or/c string? false/c) #f]
                   [style (listof (one-of/c 'packages 'enter-packages)) null]
                   [filters (listof (list/c string? string?)) '(("Any" "*.*"))])
         (or/c path? false/c)]{

Obtains a file pathname from the user via the platform-specific
 standard (modal) dialog, using @scheme[parent] as the parent window if
 it is specified, and using @scheme[message] as a message at the top of
 the dialog if it is not @scheme[#f].

The result is @scheme[#f] if the user cancels the dialog, the selected
 pathname otherwise. The returned pathname may or may not exist,
 although the style of the dialog is directed towards selecting
 existing files.

If @scheme[directory] is not @scheme[#f], it is used as the starting
 directory for the file selector (otherwise the starting directory is
 chosen automatically in a platform-specific manner, usually based on
 the current directory and the user's interactions in previous calls
 to @scheme[get-file], @scheme[put-file], etc.). If
 @scheme[filename] is not @scheme[#f], it is used as the default filename
 when appropriate, and it should @italic{not} contain a directory path
 prefix.

Under Windows, if @scheme[extension] is not @scheme[#f], the returned path
 will use the extension if the user does not supply one; the
 @scheme[extension] string should not contain a period. The extension is
 ignored on other platforms.

The @scheme[style] list can contain @scheme['common], a
 platform-independent version of the dialog is used instead of a
 native dialog.  Under Mac OS X, if the @scheme[style] list
 contains @scheme['packages], a user is allowed to select a package
 directory, which is a directory with a special suffix (e.g.,
 ``.app'') that the Finder normally displays like a file.  If the list
 contains @scheme['enter-packages], a user is allowed to select a file
 within a package directory. If the list contains both
 @scheme['packages] and @scheme['enter-packages], the former is ignored.

Under Windows and X, @scheme[filters] determines a set of filters from
 which the user can choose in the dialog. Each element of the
 @scheme[filters] list contains two strings: a description of the filter
 as seen by the user, and a filter pattern matched against file names.

See also @scheme[path-dialog%].


}

@defproc[(get-file-list [message (or/c string? false/c) #f]
                        [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                        [directory (or/c path-string? false/c) #f]
                        [filename (or/c path-string? false/c) #f]
                        [extension (or/c string? false/c) #f]
                        [style null? null]
                        [filters (listof (list/c string? string?)) '(("Any" "*.*"))])
         (or/c (listof path?) false/c)]{
Like
@scheme[get-file], except that the user can select multiple files, and the
 result is either a list of file paths of @scheme[#f].

}

@defproc[(put-file [message (or/c string? false/c) #f]
                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                   [directory (or/c path-string? false/c) #f]
                   [filename (or/c path-string? false/c) #f]
                   [extension (or/c string? false/c) #f]
                   [style (listof (one-of/c 'packages 'enter-packages)) null]
                   [filters (listof (list/c string? string?)) '(("Any" "*.*"))])
         (or/c path? false/c)]{

Obtains a file pathname from the user via the platform-specific
 standard (modal) dialog, using @scheme[parent] as the parent window if
 it is specified, and using @scheme[message] as a message at the top of
 the dialog if it is not @scheme[#f].

The result is @scheme[#f] if the user cancels the dialog, the selected
 pathname otherwise. The returned pathname may or may not exist,
 although the style of the dialog is directed towards creating a new
 file.

If @scheme[directory] is not @scheme[#f], it is used as the starting
 directory for the file selector (otherwise the starting directory is
 chosen automatically in a platform-specific manner, usually based on
 the current directory and the user's interactions in previous calls
 to @scheme[get-file], @scheme[put-file], etc.). If
 @scheme[filename] is not @scheme[#f], it is used as the default filename
 when appropriate, and it should @italic{not} contain a directory path
 prefix.

Under Windows, if @scheme[extension] is not @scheme[#f], the returned path
 will get a default extension if the user does not supply one. If
 @scheme[extension] is the empty string, then the extension is derived
 from the user's @scheme[filters] choice if the corresponding pattern is
 of the form @scheme[(string-append "*." extension)]; if the pattern is
 @scheme["*.*"], then no default extension is added. Finally, if
 @scheme[extension] is any string other than the empty string,
 @scheme[extension] is used as the default extension when the user's
 @scheme[filters] choice has the pattern @scheme["*.*"].  Meanwhile, the
 @scheme[filters] argument has the same format and auxiliary role as for
@scheme[get-file]. In particular, if the only pattern in @scheme[filters]
 is @scheme[(string-append "*." extension)], then the result pathname is guaranteed
 to have an extension mapping @scheme[extension].

Under Mac OS X 10.5 and later, if @scheme[extension] is not
 @scheme[#f], the returned path will get a default extension if the
 user does not supply one.  If @scheme[filters] contains as
 @scheme["*.*"] pattern, then the user can supply any extension that
 is recognized by the system; otherwise, the extension on the returned
 path will be either @scheme[extension] or @scheme[_other-extension]
 for any @scheme[(string-append "*."  _other-extension)] pattern in
 @scheme[filters]. In particular, if the only pattern in
 @scheme[filters] is empty or contains only @scheme[(string-append
 "*." extension)], then the result pathname is guaranteed to have an
 extension mapping @scheme[extension].

Under Mac OS X versions before 10.5, the returned path will get a
 default extension only if @scheme[extension] is not @scheme[#f] and
 @scheme[filters] contains only @scheme[(string-append "*."
 extension)].

The @scheme[extension] argument is ignored under X, and
 @scheme[filters] can be used to specify glob-patterns.

The @scheme[style] list is treated as for @scheme[get-file].

See also @scheme[path-dialog%].

}

@defproc[(get-directory [message (or/c string? false/c) #f]
                        [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                        [directory (or/c path? false/c) #f]
                        [style (listof (one-of/c 'enter-packages)) null])
         (or/c path false/c)]{

Obtains a directory pathname from the user via the platform-specific
 standard (modal) dialog, using @scheme[parent] as the parent window if
 it is specified.

If @scheme[directory] is not @scheme[#f], it is used on some platforms as
 the starting directory for the directory selector (otherwise the
 starting directory is chosen automatically in a platform-specific
 manner, usually based on the current directory and the user's
 interactions in previous calls to @scheme[get-file],
 @scheme[put-file], etc.).

The @scheme[style] argument is treated as for
@scheme[get-file], except that only @scheme['common] or @scheme['enter-packages] can be
specified.  The latter
 matters only under Mac OS X, where @scheme['enter-packages]
 enables the user to select package directory or a directory within a
 package. A package is a directory with a special suffix (e.g.,
 ``.app'') that the Finder normally displays like a file.

See also @scheme[path-dialog%].


}

@defproc[(message-box [title label-string?]
                      [message string]
                      [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                      [style (listof (one-of/c 'ok 'ok-cancel 'yes-no 'caution 'stop)) '(ok)])
         (one-of/c 'ok 'cancel 'yes 'no)]{
See also @scheme[message-box/custom].



Displays a message to the user in a (modal) dialog, using
 @scheme[parent] as the parent window if it is specified. The dialog's
 title is @scheme[title]. The @scheme[message] string can be arbitrarily
 long, and can contain explicit linefeeds or carriage returns for
 breaking lines.

The style must include exactly one of the following:
@itemize[

 @item{@scheme['ok] --- the dialog only has an @onscreen{OK} button
 and always returns @scheme['ok].}

 @item{@scheme['ok-cancel] --- the message dialog has
 @onscreen{Cancel} and @onscreen{OK} buttons. If the user clicks
 @onscreen{Cancel}, the result is @scheme['cancel], otherwise the
 result is @scheme['ok].}

 @item{@scheme['yes-no] --- the message dialog has @onscreen{Yes} and
 @onscreen{No} buttons. If the user clicks @onscreen{Yes}, the result
 is @scheme['yes], otherwise the result is @scheme['no]. Note: instead
 of a @onscreen{Yes}/@onscreen{No} dialog, best-practice GUI design is
 to use @scheme[message-box/custom] and give the buttons meaningful
 labels, so that the user does not have to read the message text
 carefully to make a selection.}

]

In addition, @scheme[style] can contain @scheme['caution] to make the
 dialog use a caution icon instead of the application (or generic
 ``info'') icon. Alternately, it can contain @scheme['stop] to make the
 dialog use a stop icon. If @scheme[style] contains both @scheme['caution]
 and @scheme['stop], then @scheme['caution] is ignored.

The class that implements the dialog provides a @scheme[get-message]
 method that takes no arguments and returns the text of the message as
 a string. (The dialog is accessible through the
@scheme[get-top-level-windows] function.)




}

@defproc[(message-box/custom [title label-string?]
                             [message string]
                             [button1-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                             [button2-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                             [button3-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                             [style (listof (one-of/c 'stop 'caution 'number-order 
                                                      'disallow-close 'no-default 
                                                      'default=1 'default=2 'default=3))
                                   '(no-default)]
                             [close-result any/c #f])
         (one-of/c 1 2 3 close-result)]{

Displays a message to the user in a (modal) dialog, using
 @scheme[parent] as the parent window if it is specified. The dialog's
 title is @scheme[title]. The @scheme[message] string can be arbitrarily
 long, and can contain explicit linefeeds or carriage returns for
 breaking lines.

The dialog contains up to three buttons for the user to click. The
 buttons have the labels @scheme[button1-label],
 @scheme[button2-label], and @scheme[button3-label], where @scheme[#f] for a
 label indicates that the button should be hidden.

If the user clicks the button labelled @scheme[button1-label], a @scheme[1]
 is returned, and so on for @scheme[2] and @scheme[3]. If the user closes
 the dialog some other way---which is only allowed when @scheme[style]
 does not contain @scheme['disallow-close]---then the result is the
 value of @scheme[close-result]. For example, the user can usually close
 a dialog by typing an Escape. Often, @scheme[2] is an appropriate value
 for @scheme[close-result], especially when Button 2 is a @onscreen{Cancel}
 button.

If @scheme[style] does not include @scheme['number-order], the order of
 the buttons is platform-specific, and labels should be assigned to
 the buttons based on their role:
@itemize[

 @item{Button 1 is the normal action, and it is usually the default
 button. For example, if the dialog has an @onscreen{OK} button, it is
 this one. Under Windows, this button is leftmost; under X and Mac OS
 X, it is rightmost. (See also
 @scheme[system-position-ok-before-cancel?].) Use this button for
 dialogs that contain only one button.}

 @item{Button 2 is next to Button 1, and it often plays the role of
 @onscreen{Cancel} (even when the default action is to cancel, such as
 when confirming a file replacement).}

 @item{Button 3 tends to be separated from the other two (under
 Mac OS X, it is left-aligned in the dialog). Use this button only
 for three-button dialogs.}

]
Despite the above guidelines, any combination of visible buttons is
 allowed in the dialog.

If @scheme[style] includes @scheme['number-order], then the buttons are
 displayed in the dialog left-to-right with equal spacing between all
 buttons, though aligned within the dialog (centered or right-aligned)
 in a platform-specific manner. Use @scheme['number-order] sparingly.

The @scheme[style] list must contain exactly one of @scheme['default=1],
 @scheme['default=2], @scheme['default=3], and @scheme['no-default] to
 determine which button (if any) is the default. The default button is
 ``clicked'' when the user types Return. If @scheme['default=]@scheme[n]
 is supplied but button @scheme[n] has no label, then it is equivalent to
 @scheme['no-default].

In addition, @scheme[style] can contain @scheme['caution] to make the
 dialog use a caution icon instead of the application (or generic
 ``info'') icon. Alternately, it can contain @scheme['stop] to make the
 dialog use a stop icon. If @scheme[style] contains both @scheme['caution]
 and @scheme['stop], then @scheme['caution] is ignored.

The class that implements the dialog provides a @scheme[get-message]
 method that takes no arguments and returns the text of the message as
 a string. (The dialog is accessible through the
@scheme[get-top-level-windows] function.)



}

@defproc[(message+check-box [title label-string?]
                            [message string]
                            [check-label label-string?]
                            [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                            [style (listof (one-of/c 'ok 'ok-cancel 'yes-no 
                                                     'caution 'stop 'checked))
                              '(ok)])
         (one-of/c 'ok 'cancel 'yes 'no)]{

See also @scheme[message+check-box/custom].

Like @scheme[message-box], except that

@itemize[
 @item{the dialog contains a check box whose label is @scheme[check-label];}
 @item{the result is two values: the @scheme[message-box] result, and a
       boolean indicating whether the box was checked; and}
 @item{@scheme[style] can contain @scheme['checked] to indicate that the check box
       should be initially checked.}
]}

@defproc[(message+check-box/custom [title label-string?]
                                   [message string]
                                   [check-label label-string?]
                                   [button1-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                                   [button2-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                                   [button3-label (or/c label-string? (is-a?/c bitmap%) false/c)]
                                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                                   [style (listof (one-of/c 'stop 'caution 'number-order 
                                                            'disallow-close 'no-default 
                                                            'default=1 'default=2 'default=3))
                                          '(no-default)]
                                   [close-result any/c #f])
         (one-of/c 1 2 3 close-result)]{

Like @scheme[message-box/custom], except that
@itemize[
 @item{the dialog contains a check box whose label is @scheme[check-label];}
 @item{the result is two values: the @scheme[message-box] result, and a
       boolean indicating whether the box was checked; and}
 @item{@scheme[style] can contain @scheme['checked] to indicate that the check box
       should be initially checked.}
]




}

@defproc[(get-text-from-user [title string]
                             [message (or/c string? false/c)]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                             [init-val string? ""]
                             [style (listof (one-of/c 'password)) null])
         (or/c string? false/c)]{

Gets a text string from the user via a modal dialog, using
 @scheme[parent] as the parent window if it is specified. The dialog's
 title is @scheme[title]. The dialog's text field is labelled with
 @scheme[message] and initialized to @scheme[init-val] (but @scheme[init-val]
 does not determine the size of the dialog).

The result is @scheme[#f] if the user cancels the dialog, the
 user-provided string otherwise.

If @scheme[style] includes @scheme['password], the dialog's text field
 draws each character of its content using a generic symbol, instead
 of the actual character.



}

@defproc[(get-choices-from-user [title string]
                                [message (or/c string? false/c)]
                                [choices (listof string?)]
                                [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                                [init-choices (listof exact-nonnegative-integer?) null]
                                [style (listof (one-of/c 'single 'multiple 'extended)) '(single)])
         (or/c (listof exact-nonnegative-integer?) false/c)]{

Gets a list box selection from the user via a modal dialog, using
 @scheme[parent] as the parent window if it is specified. The dialog's
 title is @scheme[title]. The dialog's list box is labelled with
 @scheme[message] and initialized by selecting the items in
 @scheme[init-choices]. 

The style must contain exactly one of @indexed-scheme['single],
 @indexed-scheme['multiple], or @indexed-scheme['extended]. The styles have
 the same meaning as for creating a @scheme[list-box%] object. (For
 the single-selection style, only the last selection in
 @scheme[init-choices] matters.)

The result is @scheme[#f] if the user cancels the dialog, the
 list of selections otherwise.



}

@defproc[(get-color-from-user [message (or/c string? false/c) #f]
                              [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                              [init-color (or/c (is-a?/c color%) false/c) #f]
                              [style null? null])
         (or/c (is-a?/c color%) false/c)]{

Lets the user select a color though the platform-specific
 (modal) dialog, using @scheme[parent] as the parent window if it is
 specified. The @scheme[message] string is displayed as a prompt in the
 dialog if possible. If @scheme[init-color] is provided, the dialog is
 initialized to the given color.

@italicptyStyleNote[@scheme[style]]

The result is @scheme[#f] if the user cancels the dialog, the selected
 color otherwise.



}

@defproc[(get-font-from-user [message (or/c string? false/c) #f]
                             [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                             [init-font (or/c (is-a?/c font%) false/c) #f]
                             [style null? null])
         (or/c (is-a?/c font%) false/c)]{

Lets the user select a font though the platform-specific
 (modal) dialog, using @scheme[parent] as the parent window if it is
 specified. The @scheme[message] string is displayed as a prompt in the
 dialog if possible. If @scheme[init-font] is provided, the dialog is
 initialized to the given font.

@italicptyStyleNote[@scheme[style]]

The result is @scheme[#f] if the user cancels the dialog, the selected
 font otherwise.



}

@defproc[(get-ps-setup-from-user [message (or/c string? false/c) #f]
                                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                                 [init-setup (or/c (is-a?/c ps-setup%) false/c) #f]
                                 [style null? null])
         (or/c (is-a?/c ps-setup%) false/c)]{

Lets the user select a PostScript configuration though a (modal)
 dialog, using @scheme[parent] as the parent window if it is
 specified. The @scheme[message] string is displayed as a prompt in the
 dialog. If @scheme[init-setup] is provided, the dialog is initialized to
 the given configuration, otherwise the current configuration from
@scheme[current-ps-setup]  is used.

@italicptyStyleNote[@scheme[style]]

The result is @scheme[#f] if the user cancels the dialog, , a
 @scheme[ps-setup%] object that encapsulates the selected PostScript
 configuration otherwise.



}

@defproc[(get-page-setup-from-user [message (or/c string? false/c) #f]
                                   [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                                   [init-setup (or/c (is-a?/c ps-setup%) false/c) #f]
                                   [style null? null])
         (or/c (is-a?/c ps-setup%) false/c)]{

Like
@scheme[get-ps-setup-from-user], but the dialog configures page layout for native printing
 with @scheme[printer-dc%]. A dialog is shown only if
@scheme[can-get-page-setup-from-user?] returns @scheme[#t], otherwise no dialog is shown and the result
 is @scheme[#f].

The @scheme[parent] argument is used as the parent window for a dialog if
 it is specified. The @scheme[message] string might be displayed as a
 prompt in the dialog. If @scheme[init-setup] is provided, the dialog is
 initialized to the given configuration, otherwise the current
 configuration from
@scheme[current-ps-setup]  is used.

@italicptyStyleNote[@scheme[style]]

The result is @scheme[#f] if the user cancels the dialog, a
 @scheme[ps-setup%] object that encapsulates the selected
 configuration otherwise.



}

@defproc[(can-get-page-setup-from-user?)
         boolean?]{
Returns @scheme[#t] if the current platform (Mac OS X) supports a
 page-layout dialog for use with @scheme[printer-dc%] printing, and
 if the page-layout dialog is different from the print-job dialog that
 is automatically shown when a @scheme[printer-dc%] is
 created. Returns @scheme[#f] if no separate page-layout dialog is
 needed (Windows and Unix).

}
