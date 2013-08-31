#lang meta/web

(require "git.rkt")

(define-context "stubs/pre")

(define temporary-outdated-warning
  @div[style: "border: 1px dotted red; padding: 0ex 1ex"]{
    @p*{
      This page is not currently updating with new nightly builds, but it will
      resume soon.
    @~
      Meanwhile, recent builds for various platforms are available from the
      following sites:
      @ul*{@~ @a[href: "http://www.cs.utah.edu/plt/snapshots/"]{
                University of Utah}
           @~ @a[href: "http://plt.eecs.northwestern.edu/snapshots/"]{
                Northwestern University}}}})

(define (hole tag)
  @list{@||
        {{{@tag}}}
        @||})

(define title-hole   (list "Prebuilt:" (hole "TITLE")))
(define content-hole (hole "CONTENT"))
(define version-hole
  (div style: "text-align: right; font-size: small; font-style: italic;"
       (hole "VERSION")))

(define template
  ;; generic skeleton for all files that don't have a specific template below.
  @page[#:window-title title-hole]{
    @temporary-outdated-warning
    @content-hole
    @hr
    @version-hole})

(provide index)
(define index
  @page[#:file "" #:window-title "Prebuilt materials"]{
    @temporary-outdated-warning
    @p{This directory contains Racket materials that are built daily from the
       development repository.  See below for instructions.}
    @hr
    @content-hole
    @hr
    @p*{
      The nightly-builds page is being built every night from the current @git,
      which lets you use the latest material with close to zero hassle that is
      normally associated with checking out the development tree.  You can
      choose whether you want to use the full source tree, which means that you
      will get a lot more than you get with a standard distribution, or the
      installers that contain the same material as a standard distribution.
    @~
      For the easiest way of getting a build, choose an installer for your
      platform from the @a[href: "installers/"]{@tt{installers}} directory.
    @~
      For an approach that is more suitable for scripting, you should:
      @ol*{@~ start at the @a[href: "binaries/"]{@tt{binaries}} subdirectory
              for your platform,
           @~ download the @tt{racket-...-full.tgz} file,
           @~ unpack it with GNU Tar (or something compatible to it), for
              example: "@tt{tar xzf racket-...-full.tgz}".}
      Note that there are many other @tt{tgz} files that contain various
      subsets of the tree, for example, you can get just the documentation
      part, the clean @tt{plt/src} part, the full tree before any compilation,
      documentation @tt{.plt} files, or for each platform a @tt{tgz} file that
      contains just native-code binary files.
    @~
      It is also easy to setup a script that will automate the process of
      retrieving the @tt{tgz} file, unpacking and installing it.  This is
      explained in more details in @script{scripts}.  In addition to being
      convenient for updating your tree, it can be used by an automatic job
      scheduler (for example, a cron job on Unix) to make tree that is always
      updated.}
    @hr
    @version-hole})

(define pre-installers
  @page[#:file "pre-installers/" #:title "Nightly build pre-installers"]{
    @temporary-outdated-warning
    @p{This directory contains distribution packages in tgz format.  They are
       later converted to the actual platform-specific
       @a[href: "../installers/"]{installers}.}
    @hr
    @content-hole
    @hr
    @version-hole})

(provide installers)
(define installers
  @page[#:file "installers/" #:title "Nightly build installers"]{
    @temporary-outdated-warning
    @p{Pre-release software is built using actively developed sources.
       Binaries are built nightly, and minute-by-minute changes are available
       through @|git|.}
    @; ----------------------------------------
    @h2{Option 1: Installer}
    @content-hole
    @version-hole
    @table[width: "80%" align: 'center]{
      @tr{@td[valign: 'top]{Installers:}
          @td{@nbsp}
          @td{@dl*{@~ @b{Racket}
                   @~ Same as the normal distribution, containing the basic
                      systems and DrRacket.
                   @~ @b{Racket Textual}
                   @~ A small distribution that contains only Racket and
                      console applications based on it, including the Racket
                      web server.  No docs and no GUI applications.
                   @~ @b{Racket Full}
                   @~ Contains the @i{complete} Racket tree from the
                      @git{repository}, including full documentation, full
                      source tree, libraries that are not completely stable,
                      and esoteric material.}}}
      @tr{@td[colspan: 3]{
            Note: the default installation directory contains the version
            number to avoid clobbering a normal installation.
            @small{(Under Windows, registry names are different, too.)}}}}
    @; ----------------------------------------
    @h2{Option 2: git Repository}
    @p{Full source is available from the @git{repository}.}
    @; ----------------------------------------
    @h2{Option 3: Other}
    @p{Want Racket sources from @git but don't want to build binaries?
       Want to browse a @git checkout? The
       @a[href: "../"]{nightly build page} has everything you
       could want.}})

(provide (rename-out [docs* docs]))
(define (docs* #:sub [sub #f] . text)
  (if sub
    (apply a href: (list (url-of docs) sub "/") (if (null? text) sub text))
    (apply docs text)))
(define docs
  @page[#:file "docs/" #:title "Prebuilt documentation"]{
    @temporary-outdated-warning
    @p{This directory contains documentation files in all forms, compiled from
       the current sources.}
    @hr
    @content-hole
    @hr
    @version-hole})

(define binaries
  @page[#:file "binaries/" #:title "Prebuilt binaries"]{
    @temporary-outdated-warning
    @p{This directory contains a subdirectory for each supported platform.}
    @hr
    @content-hole
    @hr
    @version-hole})

(define script
  @page[#:file "script.html" #:title "Using the nightly Racket builds"]{
    @temporary-outdated-warning
    @(define (url . s) @list{http://pre.racket-lang.org/@s})
    @(define (pre* . text) (apply pre style: "margin-left: 2em;" text))
    @p*{
      Using the nightly builds to get the latest version is very simple, and
      does not require git.  If you use the @a[href: "installers"]{installers}
      directory, then things are as simple as they are with a normal Racket
      distribution – the only slight difference is that by default the
      directory name contains the version number, so it does not clobber an
      official installation (this also works on Windows, where the registry
      keys are separate for each nightly version).
    @~
      But there is a lot of material in here that might fit better a script
      that updates a Racket tree automatically.  For this, the only tools you
      will need are GNU tar, a utility to fetch files from the web, and some
      scripting facility.  For example, assuming that your @tt{racket}
      directory is in "@tt{$BASE}", retrieving the latest source tree is as
      simple as:
      @pre*{
        cd $BASE
        wget @url{racket-src.tgz} -O - | tar xzf -}
      or with @tt{curl}:
      @pre*{
        cd $BASE
        curl @url{racket-src.tgz} | tar xzf -}
    @~
      This will fetch and unpack the full @tt{racket} source tree at the
      current directory, on top of any @tt{racket} directory that might happen
      to be there.  Since it probably doesn't make sense to have older binaries
      and newer source files, it is a good idea to remove any previous tree (if
      any):
      @pre*{
        cd $BASE
        rm -rf racket
        curl @url{racket-clean-tree.tgz} | tar xzf -}
    @~
      This is possible with any of the subsets that are packed as tgz files.
      For example, to update just the binaries for OS X (over an existing
      @tt{racket} tree):
      @pre*{
        cd $BASE
        wget @url{binaries/i386-osx-mac/racket-i386-osx-mac-binaries.tgz} -O - \
          || tar xzf -}
      To get a fully built tree for Windows:
      @pre*{
        cd $BASE
        rm -rf racket
        wget @url{binaries/i386-win32/racket-i386-win32-full.tgz} -O - \
          || tar xzf -
        cd racket}
      Note that there is no “install” step: the archive contains a ready-to-run
      tree.
    @~
      Finally, there is a @tt{stamp} file in the nightly build directory that
      can be used for scripts that periodically poll for new builds.  This file
      is updated last in the build process, so if a build fails it will stay
      the same and not fool you into getting the same build.  To demonstrate
      using this, here is an @tt{sh} script that compares a local copy of the
      @tt{stamp} file to the one on the web, and if there is any difference,
      retrieves and installs the new full Windows build (assuming it is in my
      home directory):
      @pre*{
        #!/bin/sh
        cd
        URL="@url{}"
        touch stamp # make sure that it is there
        if ! curl -s $URL/stamp | diff -q stamp - >/dev/null 2>&1; then
          curl -s $URL/stamp > stamp # remember the new stamp
          #----------
          rm -rf racket
          wget $URL/binaries/i386-win32/racket-i386-win32-full.tgz -O - @;
           | tar xzf -
          #----------
        fi}
      The marked part of this script can be replaced by any of the options
      mentioned above, for other platforms or something other than retrieving a
      full build.
    @~
      This script will retrieve and install a new build only when one is ready.
      It is suitable for running periodically via a crontab entry.  For
      example, save it in @tt{~/bin/update-full-racket}, run @tt{crontab -e} to
      edit your @tt{crontab} entries, and add a line that looks like this:
      @pre*{13 */6 * * * ~/bin/update-full-racket}
      This will run the script on the 13th minute of every sixth hour.  It is
      harmless to run it every hour, but there's no real need for it.
      Currently, the nightly build process starts at 3:50am (EDT) and lasts for
      about two hours, but every once in a while there would be additional
      builds (eg, after a minor version change).}
    @h3{A note about defensive scripting:}
    @p{Writing scripts that work for you is easy.  Making them robust enough to
       be usable by others or by site-wide setups can be a delicate job — you
       have to plan for bad @tt{PATH} setting (for example, a cron job is
       started in a pretty much bare environment), errors that happen in one
       command etc.  The following is just like the above script, modified to
       be more robust in the following way:
       @ol*{@~ The @tt{PATH} environment variable is explicitly set.
            @~ Use variable definitions to make customization easy.
            @~ Usages of @tt{$URL} and others are quoted in case they will ever
               contain spaces.
            @~ If we fail to retrieve a file, we quit the script.
            @~ Use a temporary directory to retrieve the tree, and then move it
               to its real place (so if it fails we don't end up with no
               @tt{racket}) through renaming (if we delete @tt{racket} and then
               rename the new one, we might fail halfway into the deletion).
            @~ Also, there might be some binary process running from an old
               file which might prevent removing the directory, so failure to
               remove the old tree does not abort the script.
            @~ The new stamp is remembered only if everything succeeded.}}
    @hr
    @pre*{
      #!/bin/sh
      PATH="/bin:/usr/bin"
      # where is our racket tree placed?
      MAINDIR="$HOME"
      # where should the local stamp file copy be stored?
      STAMP="$MAINDIR/stamp"
      # where is the online stuff?
      URL="@url{}"
      cd "$MAINDIR"
      touch "$STAMP" # make sure that it is there
      curl -s "$URL/stamp" > "$STAMP-new"
      if diff -q "$STAMP" "$STAMP-new" >/dev/null 2>&1; then
        # nothing changed
        rm "$STAMP-new"
      else
        #----------
        mkdir "$MAINDIR/racket-temp-$$"
        cd "$MAINDIR/racket-temp-$$"
        wget "$URL/binaries/i386-win32/racket-i386-win32-full.tgz" -O - \
        | tar xzf - \
        || exit 1
        cd "$MAINDIR"
        if [ -e "racket" ]; then mv "racket" "racket-temp-$$/racket-old"; fi \
        && mv "racket-temp-$$/racket" . \
        || exit 1
        rm -rf "racket-temp-$$"
        #----------
        cd "$MAINDIR"
        rm "$STAMP" # remember the new stamp only if no failure so far
        mv "$STAMP-new" "$STAMP"
      fi
    }
    @hr})
