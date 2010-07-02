#lang at-exp s-exp "../common.rkt"

(define-context "stubs/git")

(provide git)
(define git
  @page[#:title "Development Repository" #:file ""]{
    This is a stub page to get the header for the gitweb server.})

;; ----------------------------------------------------------------------------
;; gitweb stuff

(define header+footer
  (delay (cdr (or (regexp-match
                   ;; extract just the meat between the <body>...</body>
                   #rx"<body[^<>]*>(.*?){{{BODY}}}(.*?)</body>"
                   (xml->string @page[#:id 'git #:html-only #t "{{{BODY}}}"]))
                  (error 'gitweb-skeleton "internal error")))))
(define header @plain[#:file "header.html" (car  (force header+footer))])
(define footer @plain[#:file "footer.html" (cadr (force header+footer))])

(define gitweb-logo (copyfile (in-here "gitweb-logo.png") "tiny-logo.png"))

(define home-text
  (delay @text{
    @p{This is the Racket git server.}
    @p{See the "brief", PLT-oriented @intro{introduction to git}.}}))
(define home-file @plain[#:file "home-text.html" #:referrer values home-text])

(define gitweb-config
  @plain[#:file "gitweb_config.perl"]{
    our $projectroot = "repos";
    @||
    # used in the page's title tag
    our $site_name = "Racket Repository";
    # header/footer and home text html files
    our $site_header = "header.html";
    our $site_footer = "footer.html";
    our $home_text = "@(regexp-replace #rx"^.*/" (home-file) "")";
    push @"@"stylesheets, "@(the-resources 'style-path)";
    @||
    our $favicon = "@(the-resources 'icon-path)";
    our $logo = "@gitweb-logo";
    our $logo_url = "http://racket-lang.org/";
    our $logo_label = "Racket Homepage";
    @||
    # specification of visible repositories (managed by gitolite)
    our $projects_list = "plt-repos";
    our $projects_list_description_width = 80;
    # hide repositories that are not listed in the above
    our $strict_export = 1;
    @||
    # show repository only if this file exists
    our $export_ok = "git-daemon-export-ok";
    @||
    # respond with "server busy" above this
    our $maxload = 4;
    @||
    # label for the "home" link
    our $home_link_str = "root";
    @||
    $feature{'pathinfo'}{'default'} = [1];
    $my_uri = "http://git.racket-lang.org/";
    $home_link = "http://git.racket-lang.org/";
    # ? $feature{'javascript-actions'}{'default'} = [0];
    @||
    # some smaller features, usually overrideable by individual repos
    $feature{'grep'}{'default'} = [1];
    $feature{'grep'}{'override'} = 1;
    $feature{'blame'}{'default'} = [1];
    $feature{'blame'}{'override'} = 1;
    $feature{'pickaxe'}{'default'} = [1];
    $feature{'pickaxe'}{'override'} = 1;
    $feature{'snapshot'}{'default'} = ['zip', 'tgz'];
    # $feature{'snapshot'}{'override'} = 1;
    $feature{'avatar'}{'default'} = ['gravatar'];
    # $feature{'avatar'}{'override'} = 1;
    })

;; ----------------------------------------------------------------------------
;; git "guide"

;; TODO: link man pages; make pre blocks with frames and shaded bg; toc
(define intro
  @page[#:title "git intro"]{
    @(begin (define cmd tt)
            (define path tt)
            (define man tt)
            (define git-host        "git.racket-lang.org")
            (define at-racket       "@racket-lang.org")
            (define at-git-racket   "@git.racket-lang.org")
            (define at-lists-racket "@lists.racket-lang.org")
            (define -- mdash))
    @h1{Getting git}
    @p{I @strong{highly} recommend getting a new git installation.  Git itself
       is pretty stable (that is, you probably will not run into bugs with
       whatever version you have installed), but there are many usability
       related improvements.  Specifically, I am using 1.7.x and it is likely
       that some things in this document are specific to that version.}
    @p{You can
       @a[href: "http://git-scm.com/download"]{download a recent version},
       available in binary form for some popular platforms (RPMs for Fedora and
       RedHat, Windows, OSX).  In addition to these, you can get a build for
       @ul{@li{Ubuntu:
               @pre{sudo add-apt-repository ppa:git-core/ppa
                    sudo apt-get install git-core}}
           @li{OSX using macports:
               @pre{sudo port selfupdate
                    sudo port install git-core +svn}}}
       (For OSX, you can also get @a[href: "http://gitx.frim.nl/"]{@cmd{GitX}}
       @-- it's a good gui front-end for git, similar to @cmd{gitk} and
       @cmd{git gui}.)}
    @p{You can also build git from source is @-- here are the steps that I'm
       using to install a new version:
       @pre{GVER=1.7.1
            BASE=http://www.kernel.org/pub/software/scm/git
            TARGET=/usr/local
            cd /tmp; curl $BASE/git-$GVER.tar.gz | gunzip | tar xf -; @;
            cd git-$GVER
            make prefix=$TARGET all && sudo make prefix=$TARGET install}
       If you do this and you want the man pages too, then getting the
       pre-built man pages is the easiest route (building them requires some
       "exotic" tools):
       @pre{cd $TARGET/share/man
            curl $BASE/git-manpages-$GVER.tar.gz | gunzip | sudo tar xf -}}
    @h1{General git setup}
    @p{Commits to a git repository are done locally, so git needs to know who
       you are.  (Unlike subversion, where you need to identify yourself to be
       able to talk to the server, so the commit object is created there based
       on who you authenticated as.)  To get git to know you, run the following
       two commands:
       @pre{git config --global user.name "My Name"
            git config --global user.email "foo@at-racket"}
       This sets your @em{default} name and email for @em{all} repositories @--
       it stores this information in @path{~/.gitconfig} which is the global
       configuration file, used for such defaults.  You can edit this file
       directly too @-- it is in a fairly obvious textual format.  There is a
       lot that can be configured, see below for some of these (and see the
       @man{git-config} man page for many more details).}
    @p{In addition to this file, each repository has its own configuration file
       (located at @path{.git/config}).  Whenever git needs to check some
       option, it will use both the repository-specific config file (if you're
       in a repository) and the global one.  The @cmd{--global} flag above
       tells git to set the option in the global file.  Note that a
       configuration file cannot be part of the repository itself @-- so when
       you get a repository, you still need to do any local configuration you
       want.  (This is intentional, since the configuration file can specify
       various commands to run, so it avoids a major security hazard.)}
    @p{Important: this sets your default identity name and email for @em{all}
       repositories.  This may be a problem if you want to commit to different
       git repositories under different identities.  See the section on
       customizing git below for more details on this.}
    @h1{SSH setup}
    @p{Being a distributed system, you can do everything locally on your own
       repository, but eventually you will want to communicate with other
       people and you'll need to push these changes elsewhere.  The most
       popular way to communicate with remote repositories @-- including
       repositories on the Racket server, is via ssh.  (Access is controlled
       via a tool called "gitolite" @-- more on this below.)  The username and
       hostname of the server is "git@at-git-racket" @-- and you should be able
       to connect to this account using the ssh identity key that corresponds
       to the public key that you gave me.  To try it, run
       @pre{ssh git@at-git-racket}
       and the server (gitolite, actually) should reply with information about
       your current permissions.  The exact details of this is not important
       for now, just the fact that you were able to connect and get some
       reply.}
    @p{Using an ssh configuration file (usually ~/.ssh/config), you can set up
       a short name for the server.  For example, you can have this:
       @pre{Host pltgit
              HostName @git-host
              User git}
       and now you can simply use @cmd{ssh pltgit info} instead of the last
       example: @cmd{ssh} will know that @cmd{pltgit} is actually defined as
       @cmd{git@at-git-racket}.}
    @p{This is the @strong{preferred} way to set things up: besides being more
       convenient in that you need to type less -- it is also a useful extra
       level of indirection, so if the server settings ever change (for
       example, we might switch to a non-standard port number), you can simply
       edit your ssh config file, and continue working as usual.  In addition,
       such a configuration is needed if you created a specific ssh identity
       file to be used with git -- specifying an alternative identity file on
       the `ssh' command line is possible (an "-i" flag, in the case of
       openssh), but remember that most of your interactions with the remote
       server are done implicitly through git.  (It is possible to configure
       how git invokes ssh, but it is much easier to just configure ssh).  In
       this case, you will have:
       @pre{Host pltgit
              HostName @git-host
              User git
              IdentityFile ~/.ssh/my-plt-git-identity-file}}
    @h1{Gitolite: the server's gateway}
    @p{All access to the PLT server is done via @cmd{ssh}, and this is where
       gitolite comes in as the "who can do what" manager.  What actually
       happens on the server is that no matter what command you're trying to
       run (as you usually would, for example: @cmd{ssh somewhere ls}), the
       server has settings that make it always run its own command @-- and that
       is a gitolite script.  The script knows the command that you were
       actually trying to run, and it will reply based on that.  In the above
       ssh example, you're not specifying any command (so if it wasn't for the
       pre-set gitolite script, you'd be asking for a remote shell to start),
       and gitolite responds by telling you about your permissions.}
    @p{This is actually the @cmd{info} command, so you get the same reply with
       @cmd{ssh pltgit info}.  Again, this connects to ssh and tries to run
       @cmd{info}; gitolite sees that you're trying to run @cmd{info}, and
       instead of running it, it responds with that information.  There are a
       few additional commands that you can use this way @-- these are all
       "meta commands" in the sense that you're not interacting with a git
       process on the other end, but rather get gitolite to perform various
       tasks on your behalf.  You can run the @cmd{help} command
       (@cmd{ssh pltgit help}) to see a list of available commands.  They are
       mostly useful in dealing with your private repositories on the server,
       which will be discussed further below.}
    @h1{A (very) quick introduction to git}
    @p{This is a quick description; see the last section for more resources
       (specifically,
        @a[href: "http://eagain.net/articles/git-for-computer-scientists/"]{
          Git for Computer Scientists}
       covers these basics well).  Understanding how git models and stores data
       will make it significantly easier to work with it.}
    @p{A git repository is actually a database of a few kinds of objects, which
       form a DAG.  There are only a few of these kinds of objects, and they
       are all addressed by the SHA1 checksum of their contents.  You will
       generally see a lot of these SHA1 strings (40 hexadecimal characters),
       since they form a kind of a universal address for such objects.  (For
       convenience, any unique prefix of a SHA1 can be used with git commands
       when you need to refer to it.)  Whenever the following descriptions
       mention a pointer @-- this is actually such a SHA1 hash.}
    @ul{@li{A @em{blob} object is a generic container for any information,
            which (usually) represents a file.  This object has no pointers to
            any other objects.  It does not have anything except for the actual
            contents: no name, permission bits, etc.}
        @li{A @em{tree} object represents a directory hierarchy: it contains a
            list of names, and for each name a pointer to the object that is
            its contents.  Some of these will point at blobs (when the tree
            contains a file), and some of these will point at other trees (when
            it contains a sub-tree).  (These objects are similar to directories
            in a file system in that they contain all "meta" information on
            files: their names and permission bits are kept here.)}
        @li{A @em{commit} object represents a versioned snapshot of a tree,
            forming a line of work.  It has the following bits of information:
            @ul{@li{tree: a pointer to the tree object that was committed}
                @li{parent: a pointer to the previous commit, which this one revised}
                @li{author: the identity of the commit author (name, email, date)}
                @li{committer: the identity of the committer}
                @li{the text of the commit message (which can be arbitrarily long)}}
            The parent field is actually any number of parents: there will be no
            parents if this is the first commit in the line of work, or more
            than one parent if this is a "merge" commit that merges two lines of
            work.  Furthermore, there is nothing that prevents a git repository
            from having completely separate lines of work @-- in fact, you can
            have several independent projects contained in your repository.
            @br
            @small{(Note that git distinguishes the author of a commit from the
              person who actually performed the commit, for example @-- a patch
              could be created by X, and sent to Y to be committed.)}}
        @li{Finally, there is a `tag' object, which is very roughly a pointer
            to another object (almost always a commit), and is not important
            for now.}}
    @p{The fact that all of these objects are addressed by the SHA1 hash of
       their contents has some immediate important implications.}
    @ul{@li{Since SHA1 are cryptographic checksums, they can be considered
            @em{unique} for all practical purposes.}
        @li{The git repository is inherently hash-consed: you can never have
            "two identical files" in git -- because a file is stored at its
            SHA1 hash, two identical files will always be stored once. (Note
            that the name of a file is stored in the tree that contains it, so
            the SHA1 of the contents does not depend on it.)  The same holds
            for two trees: if you have two identical directories (same contents
            of files, same names, etc), then there will actually be only one
            tree stored in the repository.}
        @li{Furthermore, these addresses are actually global: any two
            repositories that hold a file with the same contents will have it
            at the exact same SHA1 hash.  (For example, if I have a repository
            that contains several projects, and each project contains several
            copies of the same LGPL text, then I'll have only a single blob
            object with that contents.)  This is not only making the store
            efficient, it also makes it possible to refer to an object by its
            hash @-- for example, you can refer to the SHA1 of a specific file
            at a specific version in an email, and this will have the exact
            same meaning for anyone that reads the file (eg, anyone can run
            @cmd{git show @i{SHA1}} to see that file).  (This does require that
            the readers have the actual object in their repository, of course
            @-- but no mistakes can happen, statistically speaking.)}
        @li{This holds for commits too: since a commit has the SHA1 of the tree
            it points to, then the commit SHA1 depends on the tree it points
            to.  More importantly, since a commit object has the SHA1 of its
            parent(s), then the commit depends on them.  This means that
            "replaying" a number of commits on a different parent commit (eg,
            when doing a "rebase") will always result in a separate line of
            commit objects.  These SHA1s are also global, meaning that talking
            about a specific revision by its SHA1 will always refer to it
            unambiguously (as long as others have that object in their
            repositories).}
        @li{By itself, this kind of storage @em{cannot} have any reference
            cycle.  (At least there is no practical way to get one.)  The
            storage is therefore inherently a DAG.  In addition to this object
            store, git does have a number of external references (eg, a branch
            is actually a pointer to a SHA1) @-- and those could be arbitrary,
            but the object storage itself cannot have cycles.}
        @li{The fact that a commit has a pointer to a tree is what makes git
            keep revisions of the whole tree @-- a commit cannot mark a change
            to a subtree.  (At least not with the usual higher-level commands
            that git implements.)}}
    @p{On top of this object store, there is a layer of meta-information about
       it.  The most important component here are branches (and tags).  A
       branch is basically a file that has the SHA1 of a specific commit (for
       example, your @cmd{master} branch is a SHA1 that is stored in
       @path{.git/refs/heads/master}).  This is what makes branch creation
       extremely cheap: all you need to do is create a new file with the SHA1.}
    @p{In addition, the @cmd{HEAD} (where your working directory is currently),
       will usually have a symbolic reference rather than a SHA1 (you can see
       this symbolic reference in the @path{.git/HEAD} file, which should
       usually look like @cmd{ref: refs/heads/@i{branch-name}}).  When you
       commit a new version, a new commit object is created, and the branch
       that the HEAD points to is updated.  It is also possible to checkout a
       specific SHA1 of a commit directly @-- the result of this is called
       "detached HEAD", since the HEAD is not a symbolic reference.  The
       possible danger in doing this is that @cmd{git commit} will create new
       commits that are derived from the one you're on, but no branch is
       updated; if you later checkout something else, no reference is left to
       your new commit which means that it could be lost now.  For this reason,
       if you checkout a SHA1 directly, git will spit out a detailed warning,
       including instructions on how you could name your current position
       (create a branch that points there).}
    @p{Tags come in two flavors: lightweight tags are SHA1 pointers like
       branches.  The problem with this is that such a tag could easily move to
       a different commit, which is considered bad practice.  For this reason,
       there are also "annotated tags", which are tag objects that are created
       in the object store.  These tags contain information that is similar to
       a commit (there's the tagger's identity, the commit that it points to,
       and a log message) @-- and they are reliable since you can refer to
       their SHA1.  In this case, the symbolic reference for such a tag (its
       name) will point to the tag object in the store (it is also possible to
       move it, but that would also be bad practice).  Furthermore, tags (of
       both kinds) can point to any object in the store @-- they can point to a
       tree or even to a specific blob.  This is sometimes used to store
       meta-information (eg, web pages) inside the repository.  (The repository
       for git itself has a tag that points to a blob holding the maintainer's
       GPG key.)}
    @p{Note that all of this is under a more high level of managing information
       between branches and repositories, with push/pull being the main
       operations at that level.  A high-level overview (more below):
       @ul{@li{a branch is a line of development, represented as a pointer to
               the commit at its tip@";"}
           @li{branches can be organized into hierarchies using "/" as a
               separator@";"}
           @li{some branches are local, and some are remote -- remote ones are
               named @path{remotes/origin/@i{branch}}@";"}
           @li{local branches are represented as files in
               @path{.git/refs/heads/@i{branch}} and remote ones are in
               @path{.git/refs/remotes/origin/@i{branch}}@";"}
           @li{@cmd{origin} is just the conventional name for the original
               repository you cloned @-- later on you can add new remote
               repositories so you can push and pull to/from them
               conveniently@";"}
           @li{some local branches are set to track remote ones, usually (but
               not necessarily) the two will have the same name@";"}
           @li{you can also have local branches to track other local branches
               (with pushing and pulling happening inside your repository)@";"}
           @li{@cmd{git fetch} is used to update your remote branches @-- ie,
               connect to the remote repository, get new commits (and the
               required parents and trees), and update your remote branch with
               the new tips@";"}
           @li{@cmd{git merge} and @cmd{git rebase} are used to update one
               branch with commits on another@";"}
           @li{@cmd{git pull} is, roughly speaking, a convenient way to do a
               fetch followed by a merge (or a rebase, when used with
               @cmd{--rebase}).}}}
    @p{@-- Incomplete @--}
    })
