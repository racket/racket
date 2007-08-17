#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@title{PostScript}

@defparam[current-ps-afm-file-paths paths (listof path?)]{

A parameter determines the list of paths that is used to find AFM
 files. See @secref["mr:postscriptfonts"] for more information.

}

@defparam[current-ps-cmap-file-paths paths (listof path?)]{

A parameter that determines the list of paths that is used to find
 CMap files. See @secref["mr:postscriptfonts"] for more information.

}

@defparam[current-ps-setup pss (is-a?/c ps-setup%)]{

A parameter that determines the current PostScript configuration
 settings. See @scheme[post-script-dc%] and @scheme[printer-dc%].

}
