#lang meta/web

(define-context "stubs/git"
  #:robots (add-newlines (for/list ([d '(plt libs testing play)])
                           @list{Disallow: /@|d|/})))

(provide git)
(define git
  @page[#:title "Development Repository" #:file ""]{
    This is a stub page to get the header for the gitweb server.})

;; ----------------------------------------------------------------------------
;; gitweb stuff

(define header+footer
  (lazy (cdr (or (regexp-match
                  ;; extract just the meat between the <body>...</body>
                  #rx"<body[^<>]*>(.*?){{{BODY}}}(.*?)</body>"
                  (xml->string @page[#:id 'git #:html-only #t "{{{BODY}}}"]))
                 (error 'gitweb-skeleton "internal error")))))
(define header @plain[#:file "header.html" (car  (force header+footer))])
(define footer @plain[#:file "footer.html" (cadr (force header+footer))])

(define gitweb-logo (copyfile (in-here "gitweb-logo.png") "tiny-logo.png"))

(define home-text
  (lazy @text{
    @p{This is the Racket git server.}
    @p{See the "brief", PLT-oriented @intro{introduction to git}.}}))
(define home-file @plain[#:file "home-text.html" home-text])

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
    our $default_projects_order = "age";
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

(define intro (let ()

(define (cmd  . text) (span class: "code" text))
(define (path . text) (span class: "path" text))
(define (man name . text)
  (a href: (list "http://www.kernel.org/pub/software/scm/git/docs/"
                 (and name (list name ".html")))
     (if (null? text) (span class: "man" name) text)))
(define (selflink . url) (a href: url url))
(define git-host        "git.racket-lang.org")
(define at-racket       "@racket-lang.org")
(define at-git-racket   "@git.racket-lang.org")
(define (npre . text) (apply pre style: "margin-left: 0;" text))
(define style
  @style/inline[type: 'text/css]{
    .p {
      display: block;
      margin: 1em 0;
      @; text-indent: 1em;
    }
    .code, .path, .man, pre {
      font-family: monospace;
      font-size: large;
      font-weight: bold;
      background-color: #eeeeee;
    }
    .code, .path, .man {
      white-space: nowrap;
    }
    .the_text a:link, .the_text a:visited {
      text-decoration: underline;
    }
    .the_text pre {
      margin-left: 2em;
      padding: 0.6em 0 0.6em 0.6em;
    }
    .the_text ul, .the_text ol, .the_text dl,
    .the_text li, .the_text dt, .the_text dd {
      margin-top: 1em;
      margin-bottom: 1em;
    }})

;; xhtml strict doesn't allow lists inside <p>, so fake our own paragraphs
;; using divs:
(define p* (make-separated-tag values (λ text (apply div class: 'p text))))

@page[#:title "git intro" #:extra-headers style]{

@sections[#:newpages? #t]

@div[class: 'the_text]{

@section{Getting git}
@p*{
  I @strong{highly} recommend getting a new git installation.  Git itself is
  pretty stable (that is, you probably will not run into bugs with whatever
  version you have installed), but there are many usability related
  improvements.  Specifically, I am using 1.7.x and it is likely that some
  things in this document are specific to that version.
@~
  You can @a[href: "http://git-scm.com/download"]{download a recent version},
  available in binary form for several popular platforms.  In addition to
  these, you can get a build for
  @ul*{
  @~ Ubuntu:
     @pre{sudo add-apt-repository ppa:git-core/ppa
          sudo apt-get update
          sudo apt-get install git-core}
  @~ OSX using macports:
     @pre{sudo port selfupdate
          sudo port install git-core +svn}}
  (For OSX, you can also get @a[href: "http://gitx.frim.nl/"]{@cmd{GitX}} —
  it's a good gui front-end for git, similar to @cmd{gitk} and @cmd{git gui}.)
@~
  You can also build git from source is — here are the steps that I'm using to
  install a new version:
  @pre{GVER=1.7.10
       BASE=http://git-core.googlecode.com/files
       TARGET=/usr/local
       cd /tmp; curl $BASE/git-$GVER.tar.gz | gunzip | tar xf -; cd git-$GVER
       make prefix=$TARGET all && sudo make prefix=$TARGET install}
  If you do this and you want the @man[#f]{man pages} too, then getting the
  pre-built man pages is the easiest route (building them requires some
  “exotic” tools):
  @pre{cd $TARGET/share/man
       curl $BASE/git-manpages-$GVER.tar.gz | gunzip | sudo tar xf -}}

@section{General git setup}
@p*{
  Commits to a git repository are done locally, so git needs to know who you
  are.  (Unlike subversion, where you need to identify yourself to be able to
  talk to the server, so the commit object is created there based on who you
  authenticated as.)  To get git to know you, run the following two commands:
  @pre{git config --global user.name "My Name"
       git config --global user.email "foo@at-racket"}
  This sets your @em{default} name and email for @em{all} repositories — it
  stores this information in @path{~/.gitconfig} which is the global
  configuration file, used for such defaults.  You can edit this file directly
  too — it is in a fairly obvious textual format.  There is a lot that can be
  configured, see below for some of these (and see the @man{git-config} man
  page for many more details).
@~
  In addition to this file, each repository has its own configuration file
  (located at @path{.git/config}).  Whenever git needs to check some option, it
  will use both the repository-specific config file (if you're in a repository)
  and the global one.  The @cmd{--global} flag above tells git to set the
  option in the global file.  Note that a configuration file cannot be part of
  the repository itself — so when you get a repository, you still need to do
  any local configuration you want.  (This is intentional, since the
  configuration file can specify various commands to run, so it avoids a major
  security hazard.)
@~
  Important: this sets your default identity name and email for @em{all}
  repositories.  This may be a problem if you want to commit to different git
  repositories under different identities.  See the section on customizing git
  below for more details on this.}

@section{SSH setup}
@p*{
  Since git is a distributed system, you can do everything locally on your own
  repository, but obviously, the goal is to communicate with other people so
  you'll need to push these changes somewhere else.  The most popular way to
  communicate with remote repositories — including repositories on the PLT
  server, is via ssh.  (Access is controlled via a tool called “gitolite” —
  more on this below.)  The username and hostname of the server is
  @cmd{git@at-git-racket} — and you should be able to connect to this account
  using the ssh identity key that corresponds to the public key that you use
  with the git server.  To try it, run
  @pre{ssh git@at-git-racket}
  and the server (gitolite, actually) should reply with information about your
  current permissions.  The exact details of this is not important for now,
  just the fact that you were able to connect and get some reply.
@~
  Using an ssh configuration file (usually @path{~/.ssh/config}), you
  can set up a short name for the server.  For example, you can have this:
  @pre{Host pltgit
         HostName @git-host
         User git}
  and now you can simply use @cmd{ssh pltgit info} instead of the last example:
  @cmd{ssh} will know that @cmd{pltgit} is actually defined as
  @cmd{git@at-git-racket}.
@~
  This is the @strong{preferred} way to set things up: besides being more
  convenient in that you need to type less — it is also a useful extra level of
  indirection, so if the server settings ever change (for example, we might
  switch to a non-standard port number), you can simply edit your ssh config
  file, and continue working as usual.  In addition, such a configuration is
  needed if you created a specific ssh identity file to be used with git —
  specifying an alternative identity file on the @cmd{ssh} command line is
  possible (an @cmd{-i} flag, in the case of openssh), but remember that most
  of your interactions with the remote server are done implicitly through git.
  (It is possible to configure how git invokes ssh, but it is much easier to
  just configure ssh).  In this case, you will have:
  @pre{Host pltgit
         HostName @git-host
         User git
         IdentityFile ~/.ssh/my-plt-git-identity-file}
@~
  In addition to an ssh configuration file, git also has a way to create prefix
  shorthands.  For example, if you use this configuration:
  @pre|{git config --global url.git@foo.org:.insteadOf foo:}|
  then whenever git expects a repository URL, it will replace @cmd{foo:}
  with @cmd|{git@foo.org:}|, for example:
  @pre{git clone foo:bar}
  While it is possible to use this instead of an ssh config file to access the
  @cmd{plt} repository, the former is preferable.  The reason for that is that
  you will also interact with the server directly via ssh commands (described
  in the following section).  Keeping the alias in your ssh configuration means
  that you will use the same alias for both @cmd{git} commands and other
  @cmd{ssh}-based commands.  You may still want to use it for other servers,
  specifically, here is a popular setup for github (this is configuration text
  that you can paste into your global @cmd{.gitconfig} file):
  @pre|{[url "git://github.com/"]
          insteadOf = github:
        [url "git@github.com:"]
          pushInsteadOf = github:
          pushInsteadOf = git://github.com/}|
  It translates @cmd{github:} to a github read-only @cmd{git://} URL, and it
  translates pushes to the same prefix to use github's ssh URLs.  Note that it
  also translates the read-only @cmd{git://} url to an ssh url for pushing.
  (The same setup can be used for @cmd{gist.github.com}, to deal with github
  gists via @cmd{git}.)}

@section{Gitolite: the server's gateway}
@p*{
  All access to the PLT server is done via @cmd{ssh}, and this is where
  gitolite comes in as the “who can do what” manager.  What actually happens on
  the server is that no matter what command you're trying to run (as you
  usually would, for example: @cmd{ssh somewhere ls}), the server has settings
  that make it always run its own command — and that is a gitolite script.  The
  script knows the command that you were actually trying to run, and it will
  reply based on that.  In the above ssh example, you're not specifying any
  command (so if it wasn't for the pre-set gitolite script, you'd be asking for
  a remote shell to start), and gitolite responds by telling you about your
  permissions.
@~
  This is actually the @cmd{info} command, so you get the same reply with
  @cmd{ssh pltgit info}.  Again, this connects to ssh and tries to run
  @cmd{info}; gitolite sees that you're trying to run @cmd{info}, and instead
  of running it, it responds with that information.  There are a few additional
  commands that you can use this way — these are all “meta commands” in the
  sense that you're not interacting with a git process on the other end, but
  rather get gitolite to perform various tasks on your behalf.  You can run the
  @cmd{help} command (@cmd{ssh pltgit help}) to see a list of available
  commands.  They are mostly useful in dealing with your private repositories
  on the server, which will be discussed further below.}

@section{A (very) quick introduction to git}
@p*{
  This is a quick description; see the last section for more resources
  (specifically,
  @a[href: "http://eagain.net/articles/git-for-computer-scientists/"]{
    Git for Computer Scientists} covers these basics well).  Understanding how
  git models and stores data will make it significantly easier to work with it.
@~
  A git repository is actually a database of a few kinds of objects, which form
  a DAG.  There are only a few of these kinds of objects, and they are all
  addressed by the SHA1 checksum of their contents.  You will generally see a
  lot of these SHA1 strings (40 hexadecimal characters), since they form a kind
  of a universal address for such objects.  (For convenience, any unique prefix
  of a SHA1 can be used with git commands when you need to refer to it.)
  Whenever the following descriptions mention a pointer — this is actually such
  a SHA1 hash.}
@ul*{
@~ A @em{blob} object is a generic container for any information, which
   (usually) represents a file.  This object has no pointers to any other
   objects.  It does not have anything except for the actual contents: no name,
   permission bits, etc.
@~ A @em{tree} object represents a directory hierarchy: it contains a list of
   names, and for each name a pointer to the object that is its contents.  Some
   of these will point at blobs (when the tree contains a file), and some of
   these will point at other trees (when it contains a sub-tree).  (These
   objects are similar to directories in a file system in that they contain all
   “meta” information on files: their names and permission bits are kept here.)
@~ A @em{commit} object represents a versioned snapshot of a tree, forming a
   line of work.  It has the following bits of information:
   @ul*{@~ tree: a pointer to the tree object that was committed
        @~ parent: a pointer to the previous commit, which this one revised
        @~ author: the identity of the commit author (name, email, date)
        @~ committer: the identity of the committer
        @~ the text of the commit message (which can be arbitrarily long)}
   The parent field is actually any number of parents: there will be no parents
   if this is the first commit in the line of work, or more than one parent if
   this is a “merge” commit that merges two lines of work.  Furthermore, there
   is nothing that prevents a git repository from having completely separate
   lines of work — in fact, you can have several independent projects contained
   in your repository.
   @br
   @small{(Note that git distinguishes the author of a commit from the person
     who actually performed the commit, for example — a patch could be created
     by X, and sent to Y to be committed.)}
@~ Finally, there is a @em{tag} object, which is very roughly a pointer to
   another object (almost always a commit), and is not important for now.}
@p*{
  The fact that all of these objects are addressed by the SHA1 hash of their
  contents has some immediate important implications.}
@ul*{
@~ Since SHA1 are cryptographic checksums, they can be considered @em{unique}
   for all practical purposes.
@~ The git repository is inherently hash-consed: you can never have “two
   identical files” in git — because a file is stored at its SHA1 hash, two
   identical files will always be stored once. (Note that the name of a file is
   stored in the tree that contains it, so the SHA1 of the contents does not
   depend on it.)  The same holds for two trees: if you have two identical
   directories (same contents of files, same names, etc), then there will
   actually be only one tree stored in the repository.
@~ Furthermore, these addresses are actually global: any two repositories that
   hold a file with the same contents will have it at the exact same SHA1 hash.
   (For example, if I have a repository that contains several projects, and
   each project contains several copies of the same LGPL text, then I'll have
   only a single blob object with that contents.)  This is not only making the
   store efficient, it also makes it possible to refer to an object by its hash
   — for example, you can refer to the SHA1 of a specific file at a specific
   version in an email, and this will have the exact same meaning for anyone
   that reads the file (eg, anyone can run @cmd{git show @i{SHA1}} to see that
   file).  (This does require that the readers have the actual object in their
   repository, of course — but no mistakes can happen, statistically speaking.)
@~ This holds for commits too: since a commit has the SHA1 of the tree it
   points to, then the commit SHA1 depends on the tree it points to.  More
   importantly, since a commit object has the SHA1 of its parent(s), then the
   commit depends on them.  This means that “replaying” a number of commits on
   a different parent commit (eg, when doing a “rebase”) will always result in
   a separate line of commit objects.  These SHA1s are also global, meaning
   that talking about a specific revision by its SHA1 will always refer to it
   unambiguously (as long as others have that object in their repositories).
@~ By itself, this kind of storage @em{cannot} have any reference cycle.  (At
   least there is no practical way to get one.)  The storage is therefore
   inherently a DAG.  In addition to this object store, git does have a number
   of external references (eg, a branch is actually a pointer to a SHA1) — and
   those could be arbitrary, but the object storage itself cannot have cycles.
@~ The fact that a commit has a pointer to a tree is what makes git keep
   revisions of the whole tree — a commit cannot mark a change to a subtree.
   (At least not with the usual higher-level commands that git implements.)}
@p*{
  On top of this object store, there is a layer of meta-information about it.
  The most important component here are branches (and tags).  A branch is
  basically a file that has the SHA1 of a specific commit (for example, your
  @cmd{master} branch is a SHA1 that is stored in
  @path{.git/refs/heads/master}).  This is what makes branch creation extremely
  cheap: all you need to do is create a new file with the SHA1.
@~
  In addition, the @cmd{HEAD} (where your working directory is currently), will
  usually have a symbolic reference rather than a SHA1 (you can see this
  symbolic reference in the @path{.git/HEAD} file, which should usually look
  like @cmd{ref: refs/heads/@i{branch-name}}).  When you commit a new version,
  a new commit object is created, and the branch that the @cmd{HEAD} points to
  is updated.  It is also possible to checkout a specific SHA1 of a commit
  directly — the result of this is called “detached HEAD”, since the HEAD is
  not a symbolic reference.  The possible danger in doing this is that @cmd{git
  commit} will create new commits that are derived from the one you're on, but
  no branch is updated; if you later checkout something else, no reference is
  left to your new commit which means that it could be lost now.  For this
  reason, if you checkout a SHA1 directly, git will spit out a detailed
  warning, including instructions on how you could name your current position
  (create a branch that points there).
@~
  Tags come in two flavors: lightweight tags are SHA1 pointers like branches.
  The problem with this is that such a tag could easily move to a different
  commit, which is considered bad practice.  For this reason, there are also
  “annotated tags”, which are tag objects that are created in the object store.
  These tags contain information that is similar to a commit (there's the
  tagger's identity, the commit that it points to, and a log message) — and
  they are reliable since you can refer to their SHA1.  In this case, the
  symbolic reference for such a tag (its name) will point to the tag object in
  the store (it is also possible to move it, but that would also be bad
  practice).  Furthermore, tags (of both kinds) can point to any object in the
  store — they can point to a tree or even to a specific blob.  This is
  sometimes used to store meta-information (eg, web pages) inside the
  repository.  (The repository for git itself has a tag that points to a blob
  holding the maintainer's GPG key.)
@~
  Note that all of this is under a more high level of managing information
  between branches and repositories, with push/pull being the main operations
  at that level.  A high-level overview (more below):
  @ul*{
  @~ a branch is a line of development, represented as a pointer to the commit
     at its tip;
  @~ branches can be organized into hierarchies using @path{/} as a separator;
  @~ some branches are local, and some are remote — remote ones are named
     @path{remotes/origin/@i{branch}};
  @~ local branches are represented as files in
     @path{.git/refs/heads/@i{branch}} and remote ones are in
     @path{.git/refs/remotes/origin/@i{branch}};
  @~ @cmd{origin} is just the conventional name for the original repository you
     cloned — later on you can add new remote repositories so you can push and
     pull to/from them conveniently;
  @~ some local branches are set to track remote ones, usually (but not
     necessarily) the two will have the same name;
  @~ you can also have local branches to track other local branches (with
     pushing and pulling happening inside your repository);
  @~ @cmd{git fetch} is used to update your remote branches — ie, connect to
     the remote repository, get new commits (and the required parents and
     trees), and update your remote branch with the new tips;
  @~ @cmd{git merge} and @cmd{git rebase} are used to update one branch with
     commits on another;
  @~ @cmd{git pull} is, roughly speaking, a convenient way to do a fetch
     followed by a merge (or a rebase, when used with @cmd{--rebase}).}
@~
  There are several git tools that are relevant here.  These are @em{not}
  commands that you need to know for everyday use — so you can ignore this
  part.  It's only relevant if you want to see more of the low level structure
  (or maybe if you want to write code that interfaces with a repository at this
  level).}
@dl*{
@~ @cmd{git show @i{SHA1}}
@~ Show the object, in some appropriate way based on the type of the object.
   (For blobs it shows the contents, for trees you get a listing of its
   contents, and for commits it shows the log and the patch.)
@~ @cmd{git cat-file {-t | -s | @i{type} | -p} @i{SHA1}}
@~ A more low-level command that tells you the type/size of an object (@cmd{-t}
   and @cmd{-s}), or shows the contents of an object as-is when given a type.
   @cmd{-p} will “pretty-print” the object, eg, showing the contents of a tree
   object instead of dumping its binary encoding.
@~ @cmd{git gc}
@~ Starts from a rootset holding all known references (branches, tags, etc),
   and collects dangling objects.  Such objects are generated due to various
   reasons — for example, rebasing means that new commits are generated, and
   the old ones are kept around.  Actually, this will not remove recently
   referenced objects — there is a protection mechanism that keeps them around
   for a while, so if you somehow mess things up there is still a way to
   recover.
@~ @cmd{git fsck}
@~ Does a “file system check” on the repository.
@~ @cmd{git rev-parse @i{symbolic-name}}
@~ Prints out the full SHA1 of a symbolic name (eg, a branch name or a tag
   name).  Will also print out the SHA1 given a possibly short prefix of one.
   (Actually, this command can also show other information about a repository,
   which makes it an important entry point for programs that deal with a
   repository.)}

@section{Clone the PLT repository}
@p*{
  As you probably know by now, in git you don't checkout a repository — you
  clone it, getting a copy of the complete repository you cloned.  This
  includes the object store and the various references (branches and tags).
  There are several ways to get the PLT repository, but the one that is
  relevant to work on it is to do so through ssh — since this allows pushing
  changes back to the server.  (It is also possible to clone from one place and
  push to another, but if you start with cloning through ssh your clone will be
  already set up to push changes back.)  The information that gitolite gives
  you (with @cmd{ssh pltgit info}, assuming the above ssh setup) includes two
  repositories that you have write access to: @cmd{plt} is the main repository,
  and @cmd{play} is setup similarly (intended to try things out, see the
  “Fooling around” section below).  To get the main repository, run
  @pre{git clone pltgit:plt}
  which will create a @path{plt} directory with your new clone.  You can now
  start working in this directory.
@~
  The repository is also available from other sources, some can be used for
  read-only cloning:
  @ul*{
  @~ @cmd{git clone git://@|git-host|/plt.git}@br
     cloning the repository using git's own network protocol
  @~ @cmd{git clone http://@|git-host|/plt.git}@br
     clone the repository over http
  @~ @cmd{git clone http://github.com/plt/racket.git}@br
     this uses the repository mirror on github, which is automatically kept in
     sync (you can also use @cmd{https://...})}
  and some present a web interface for additional information:
  @ul*{
  @~ @cmd{@selflink{http://@|git-host|/plt}}@br
     a web interface to inspect the repository
  @~ @cmd{@selflink{https://github.com/plt/racket}}@br
     github's fancier web interface}}

@section{Start working: git commits vs subversion commits}
@p*{
  As seen in the previous section, you start with
  @pre{git clone pltgit:plt@";" cd plt}
  And now you get to actually do some work.
@~
  For the normal cycle of operations, working with git is not all that
  different from working with subversion — you would change some files, and
  then:
  @pre{git commit some/paths}
  or
  @pre{git commit some/paths -m "add some feature" -m "requires another"}
  only now the commit lives only in @strong{your clone only}, not in the server
  (which is why committing is blindingly fast, not requiring a network
  connection).  To push your commits to the server, run @cmd{git push}, and to
  pull updates from the server run @cmd{git pull}.  This is obviously very much
  oversimplifying the process: mainly neglecting to talk about updates on the
  server when you already have local changes.  (See below for a more detailed
  explanation.)  Note that in these examples I'm explicitly specifying the
  paths to commit, either the files that you want to commit or a directory
  where you want to commit all changes.  See the section below on the “staging
  area” for more details.
@~
  One major difference to keep in mind is that git commits are @strong{not}
  like subversion commits.  (This is confusing since many places that discuss
  the difference between the two and/or try to teach git to subversion users
  almost always work under the assumption that commits in the two systems are
  the same.)  The thing is that git commits are done at a finer level than
  subversion commits — since a commit is done locally and not on the server.
  To really imitate how subversion works, you would push all commits right
  after you create them — essentially equating commits with pushes, which is
  how you work with a subversion repository.  But by just @em{not} doing this,
  you will immediately get some of the benefits that git gives you.  So a
  better way to think about it is: in git you commit at points that make sense
  for the respective changes, usually at a finer level than subversion commits.
  Then, you push back a bunch of commits to the server — whether one or a
  hundred.  The point where you push your changes to the server is effectively
  the point where you decide that you're in a good enough state to make your
  work public.
@~
  Incidentally, following this intuition, drdr is running a build for every
  push to the server — not for every commit.  When you push to the server, it
  will tell you which push number this is — these numbers are going to be used
  by drdr, and they (very!) roughly correspond to subversion commits.
  (Currently, every push gets a number, but in the future this might be used
  only for pushes to the master branch.)  There's no plan at the moment to use
  these numbers for anything else.}

@section{Fooling around with git}
@p*{
  Experimenting with git is easy to do, and the server is set up to make it
  even easier.  You can use one of the following ways to experiment safely with
  the main repository:
  @ul*{
  @~ There is a @cmd{play} repository on the server.  This repository is very
     similar to the @cmd{plt} repository, and it is set up in the same way that
     @cmd{plt} is.  Feel free to destroy it in any way you want, even if it
     becomes unusable, it's easy to just recreate it.
  @~ You can create (and later delete) your own repositories — including making
     your own copy of the main repository, an operation that is known as
     “fork”.  Your fork will be created efficiently (ie, creating a fork of the
     @cmd{plt} repository is cheap), but any changes made to it will not affect
     the main repository.  A fork is created with a gitolite command, and once
     it's there you can clone it and eventually delete it.  Here are the
     relevant commands — use your actual username in place of @cmd{$user} (or
     have @cmd{$user} set to your username):
     @ol*{@~ @cmd{ssh pltgit fork plt $user/myplt}
          @~ @cmd{git clone pltgit:$user/myplt}
          @~ ...play with this clone, push, pull, etc...
          @~ @cmd{ssh pltgit delete $user/myplt}}
     More on user repositories below.}}

@section{The staging area}
@p*{
  Something that tends to confuse people is git's “staging area”.  This is a
  concept that is unique to git — roughly speaking, you can have three versions
  of a tree:
  @ul*{
  @~ the files that you actually see (and edit) — the working directory,
  @~ another is the staging area which you add stuff to from the working tree
     using @cmd{git add},
  @~ and then there is the tree that is in the HEAD with all prior versions.}
@~
  The thing that can confuse here is that when you @cmd{git add some/file} for
  a file that you edited (or created) and then edit it further, then the
  version will get committed by a plain @cmd{git commit} will be the one that
  was added.  Note that @cmd{git status} will tell you which modifications are
  in the staging area waiting to be committed, and which modifications are in
  your working directory — in the given example, it will tell you that
  @path{some/file} is in both.
@~
  The staging area can be useful at times, but most likely at the beginning
  stages you will want to just avoid it.  The good news is that it is easy to
  do so.}
@dl*{
@~ @strong{Avoid the @cmd{-a} flag}.
@~ @p*{
     Before we see how to ignore it, note that there are many web pages that
     will tell you to use @cmd{-a} with the commit command.  This will make git
     commit all changes to tracked files — @strong{including tracked files that
       are outside of your current directory}, and this can make you commit
     changes that you didn't intend to commit.}
@~ @strong{Always specify a path to @cmd{git commit}}.
@~ @p*{
     The easiest way to avoid the staging area is to specify the path(s) to
     what you want to commit, possibly @path{.} for all changes in the current
     directory (and below).  Specifying a path this way will make
     @cmd{git commit} behave very similarly to subversion: tracked files that
     were modified will get committed, and added files (with @cmd{git add})
     that are listed in the paths-to-be-committed are also committed.  Tracked
     and added files that are not listed (and not in a specified subdirectory)
     are left as-is.  So, if you had a habit of doing this with subversion
     (@cmd{svn commit .}), then git will essentially do the same.  You will
     still need to use @cmd{git add} for newly created files, but this is
     essentially the same as with subversion.
   @~
     It is also possible to “make up new git commands” for yourself.  See the
     following section on the subject: it adds a new @cmd{git ci} command that
     passes @path{.} to @cmd{git commit}, similarly to what @cmd{svn ci} does
     by default.}
@~ It is a good idea to @strong{avoid using the @cmd{-m} flag}, until you're
   more comfortable with git.
@~ @p*{
     Let git pop up an editor to write the commit log: the file that you will
     edit will list the changes that you are about to commit as well as changes
     that you are not going to commit.  Glancing through it, you will see
     changes that you missed, furthermore, the paths are relative which makes
     it easy to quickly distinguish paths in the current directory and outside
     of it (the latter will begin with @path{../}).  If you see any problems,
     just make sure that you have no commit message in the editor and when you
     quit it git will abort the commit (same as subversion).}
@~ @strong{Don't push out all commits to the server immediately}.
@~ @p*{
     Even if you did commit something by mistake, it is possible to undo the
     commit — run @cmd{git reset HEAD^}, which will undo the last commit (it
     moves the branch to the parent commit), and the changes that are no longer
     committed will be left in your working directory, so you get to try the
     commit again.  Note that this is possible @strong{only if you didn't push
       out the commit} that you're undoing — if you did, then the server will
     later not allow you to push changes that are not strict extensions of what
     it has (since this is likely to confuse other people who already got your
     commit).
   @~
     So in general, remember that you can commit often, and commit when it
     makes sense to do so, and push commits out only when you're done with
     whatever you were working on.  Consider your local git history as
     something that you have full control over: you can undo commits and redo
     them (in fact, @cmd{git commit --amend} does just that: undo the last
     commit, and combine it with new changes — it's a solution for “oops
     commits”), you can rebase them, and you can just throw away everything and
     start from scratch.  But when you do push your history out, the party is
     over, and any mistakes will need to be rectified in further commits (eg,
     you can no longer use that @cmd{--amend} flag, you have to do an “oops
     commit”).
   @~
     (BTW, strictly speaking, it is only the policy on our server that forbids
     such rewritten history — since this is likely to be a mistake for now, and
     if it happens most people will be confused about what needs to be done.)
   @~
     Also, as said above, pushing all commits immediately means that you're
     essentially restricting yourself to the same mode of operation as
     subversion.  Same mode, but more complicated — and you won't enjoy any of
     the benefits, which will guarantee that you will suffer.}}

@section{Configuring and extending git}
@p*{
  As mentioned above, git uses several configuration files that customize
  various aspects.  The two important ones are your global file
  (@path{~/.gitconfig}) and a per-repository file in @path{.git/config} at the
  repository root — whenever a value is needed, git will first consult the
  repository configuration, and if the option is not set there it will try your
  global version.  (It will also look at a system-wide configuration, but this
  is irrelevant here.)  Configuration options names are separated by a
  @path{.}, and configuration files have a simple syntax, with @cmd{foo.bar}
  option listed as a @cmd{bar = some value} line in a @cmd{[foo]} section.
  Note that you can set @em{any} configuration you want to, no restrictions.
  This can be useful for customizing various extensions, including scripts that
  you may want to write (for example, the git server has a script that checks
  the @cmd{hooks.counter} option to know if it should keep track of pushes).
  This is facilitated by several options to @cmd{git config} which makes it
  easy to query configurations from scripts etc.
@~
  To edit your configuration options you can either use the @cmd{git config}
  command, or you can edit the file directly.  When @cmd{git config} changes
  the file it rewrites only part of the file and leaves the rest untouched,
  which conveniently leaves your own format and any comments you might want to
  include.  To set a value through the command and then get it:
  @pre{git config foo.bar some-value
       git config foo.bar}
  and you can add a @cmd{--global} flag to either form to use only your global
  configuration file.  There are many other options — for dealing with keys
  that must be booleans or integers, for keys with multiple values, etc.  The
  @man{git-config} man page will tell you much more on this.
@~
  The man page also lists the configuration options that customize various
  functionalities.  Here are some important ones that you should consider
  setting (each listed as a command that sets it globally):
  @ul*{
  @~ @npre{git config --global user.name "My Name"
           git config --global user.email "foo@at-git-racket"}
     @p*{
       As said in the beginning of this text, you will likely want to set a
       default username and email for yourself.  But note that if you do set
       this globally, it will be your default identity for all repositories.
       This makes sense only if you commit to PLT-related repositories, but it
       can be confusing if you're also committing to some other non-PLT-related
       repositories and want to commit under a different email (or name) — for
       example, you may want to commit to a public project with a gmail
       address, and to a departmental repository with your
       @cmd|{foo@cs.bar.edu}| email.
     @~
       You could set the racket-lang.org identity locally in your PLT clone or
       you could set your other identity in the other repository, but in any
       case you should be aware of this and avoid letting git guess your name
       and email.  (Some confusion is likely to happen anyway, and git has a
       way to “map” some name/email to another when mistakes happen.)}
  @~ @npre{git config --global push.default simple}
     @p*{
       By default, when you run @cmd{git push}, git will push all branches that
       correspond to branches in the remote repository.  This can be surprising
       if you're working on several branches since it will push them all out.
       Setting this option to @cmd{upstream} will make git push the current
       branch to the branch it is tracking, and on newer git versions setting
       it to @cmd{simple} is similar except that it will refuse to create a
       remote branch.  (And on really old systems, you should use a value of
       @cmd{tracking}, which is the old and now-deprecated synonym for
       @cmd{upstream}.)
     @~
       Another option for this is @cmd{current}, which makes @cmd{git push}
       always push the current branch to the remote it was cloned from.  This
       is convenient in that you never need to set up how local branches track
       remote ones — it's as if all local branches @em{always} track all remote
       branches under the same name.  For example, after you clone an empty
       repository (see the user repositories section below), a @cmd{git push}
       will push a master branch remotely — whereas with @cmd{tracking} you
       need to have the first push explicitly specify the branch to push,
       usually @cmd{git push origin master} (this sets things up so later you
       can just run @cmd{git push}).  However, using @cmd{current} you can no
       longer push from one local branch to another local branch it is set to
       track.
     @~
       So a possible conclusion here is that you should use @cmd{tracking},
       unless you plan on branches to always track remote branches by the same
       name.  @cmd{tracking} is often preferred over @cmd{current}.}
  @~ @npre{git config --global core.excludesfile "~/.gitignore"}
     @p*{
       You'll probably want to always ignore a number of common patterns, like
       backup files or OSX @path{.DS_Store} files.  To do this, you first set a
       default file as shown here (note that @path{~} is quoted, and git will
       expand it to whatever your home directory is).  If you have this
       setting, you can then create a file at this path with patterns for files
       that you always want to ignore.  This file has shell-patterns (and
       possibly @cmd{#}-comments) — for example:
       @pre{# backups
            *~
            # autosaves (note the #-quoting)
            \#*
            # OSX junk
            .DS_Store}
       (See the @man{gitignore} man page for a few more details.)
     @~
       In addition to this, git repositories can have their own
       @path{.gitignore} files (unlike @path{.git/config} files), which are
       combined hierarchically together with this global option.  In fact, you
       don't really need to set the above ignores for the PLT repository since
       they're already included in its toplevel @path{.gitignore} file — but
       doing so is still a good idea since you're likely to work on other
       repositories too.}
  @~ @npre{git config --global core.editor emacs
           git config --global core.pager less}
     @p*{
       These two settings are used to tell git which command to use for editing
       log messages, and which command is used to paginate output.  (The former
       might already be set in your environment as the value of the
       @cmd{EDITOR} variable.)  If you set the latter to @cmd{cat}, git will
       just spill all output directly.  In addition to these, you can also
       control which individual commands use a pager, for example, to disable
       the pager for @cmd{git log}, you can do this:
       @pre{git config --global pager.log false}}
  @~ @npre{git config --global color.ui auto
           @i{...}
           git config --global color.branch.current yellow red bold
           git config --global color.branch.local   yellow
           git config --global color.branch.remote  green
           @i{...}}
     @p*{
       These settings control how git uses colors: whether it shows them, and
       which colors it will use for various outputs.  There are many of these
       settings, which you can find in the @man{git-config} man page.}
  @~ @p*{
       @cmd{remote.origin.*}, @cmd{branch.master.*}
     @~
       Git keeps track of what the @cmd{origin} repository and how branches
       track other branches in the configuration file too.  (You will have such
       entries for all known remote repositories and branches.)  Usually you
       set these values (often implicitly) via various git commands — but you
       might want to look in your configuration file if you want to tweak
       things yourself.  Note that for configuration names with more than two
       parts, the section name will something like @cmd{[remote "origin"]}.}
  @~ @p*{
       @cmd{sendemail.identity}, @cmd{sendemail.from},
       @cmd{sendemail.bcc}, @cmd{sendemail.suppresscc}, ...
     @~
       These settings configure @cmd{git send-email}, which is used to send
       patches from your repository elsewhere.  You will probably want to
       customize them if/when you get to use this facility often.  (See
       below.)}}
@~
  In addition to these settings, you can extend git with your own aliases and
  commands.  Aliases are stored in your git configuration — so you can use
  @cmd{git config} to create an alias, for example, @pre{git config --global
  alias.up "pull --stat --all"} creates a global @cmd{git up} command which is
  actually a short alias for running @cmd{git pull --stat --all}.
@~
  @strong{Notes about aliases:}
  @ul*{
  @~ To edit aliases, it is more convenient to edit your configuration file
     directly.
  @~ Since aliases are stored in git configuration files, they can be made
     local to each repository.
  @~ When command-line arguments are given to the alias, they will be appended
     to the alias text.
  @~ Aliases @em{cannot} override git commands; this is intentional, to avoid
     scripts breaking due to modified commands.
  @~ An alias that starts with a @cmd{!} character will be run as a shell
     command.  For example, you can use
     @pre{k = "!gitk -d"}
     to make @cmd{git k} run the gitk program with the @cmd{-d} flag.
  @~ Some aliases that I find useful are:
     @pre{# satisfy the "up instinct"
          up = pull --ff-only --stat --all
          # quick status, similar to what subversion shows
          st = status -s
          # we will be dealing more with branches
          br = branch}}
@~
  In addition to aliases, you can create new git commands using a script that
  is called @cmd{git-@i{something}} somewhere in your path.  (Note that these
  cannot override known git commands either.)  Such commands will be available
  as @cmd{git @i{something}}.  One use for this is using our facility for
  managing file properties — the @path{collects/meta/props} program.  To do
  this put this in a file called @cmd{git-prop} somewhere in your PATH, for
  example, @path{~/bin/git-prop}:
  @pre|{#!/bin/sh
        top="$(git rev-parse --show-toplevel)" || exit 1
        exec "$top/collects/meta/props" "$@"}|
  then run @cmd{chmod +x ~/bin/git-prop}, and you can now use it as a git
  command (try @cmd{git prop -h}).  Note the use of the @cmd{rev-parse}
  command: it will display the repository root, which means that you will get
  to run the props script of the repository you're @em{currently} using.
  (There are many git commands that are useful for such scripts.)
@~
  Another useful script is @cmd{git-ci} which mimics the behavior of
  @cmd{svn commit} (avoiding confusion with the staging area).  As said above,
  a good way to achieve this is to specify the current directory (@path{.}) if
  you don't specify any other path.  If you save this as @path{git-ci}, you
  will get a @cmd{git ci} that does just that:
  @pre|{
    #!/bin/sh
    add_dot=yes; for p; do if [ -e "$p" ]; then add_dot=no; fi; done
    if [ -e "$(git rev-parse --git-dir)/MERGE_HEAD" ]; then add_dot=no; fi
    if [ -d "$(git rev-parse --git-dir)/rebase-apply" ]; then add_dot=no; fi
    if [ $add_dot = yes ]; then git commit . "$@"; else git commit "$@"; fi
    }|
  This small script will basically check all arguments and see if one is an
  existing path (or if you're resolving a merge).  If none are, it adds
  @path{.} as a first argument (this avoids confusing it as a value for some
  flag).  Note that this is not completely foolproof: for example, if you'll
  use the @cmd{-m .} hack, it will assume that you did specify a path.  (But
  you should really avoid such log messages.)}

@section{User repositories}
@p*{
  As mentioned above, the PLT server allows you to create your own
  repositories.  Repositories on the server can be organized in a nested
  directory structure, and you “own” all repositories that are in a directory
  with your username.  The gitolite @cmd{info} command that was mentioned above
  shows you this with a @cmd{C CREATER/.*} line: this means that you can create
  any repository if it is in a subdirectory with your username.  (In this
  discussion, “the server” is actually the gitolite script that runs on the
  server.)
@~
  Any git operation that you do on a repository that you own which does not
  exist will make the server create it for you — for example, if you clone such
  a repository.  To run these examples use your git username instead of
  @cmd{$user}, or simply set the @cmd{user} variable in your shell (as this
  example shows) — this is only to make copy-pasting easy, of course.
  @pre{user=eli # your own username here
       git clone pltgit:$user/foo}
  (You are encouraged to run these commands — at the end of this section you'll
  see how you can clean things up.)
@~
  What will happen now is (a) git will initialize a @path{foo} repository for
  you, (b) it will connect to the server to clone its contents, (c) the server
  will notice that it doesn't exist so it will create it, (d) your git process
  will clone the empty result.  Because the remote repository is empty, git
  will complain that “You appear to have cloned an empty repository” — this is
  expected, so you shouldn't worry about it.  Once you have your (empty) clone,
  you can populate it as usual, then push the new content back to the server's
  copy:
  @pre{cd foo
       @i{...create some files...}
       git add .
       git commit -m "initial content"
       git push origin master}
  Note that the last command explicitly names the branch to push over — once
  this push is done, git will remember this relation and further pushes can be
  done with just @cmd{git push}.  If you happen to forget this and use
  @cmd{git push}, then git will not push anything, and it will tell you about
  it and suggest specifying a branch.  On the other hand, if you set the
  @cmd{push.default} configuration option to @cmd{current} (as described in the
  customization section above), then even in this first push you can just run
  @cmd{git push} since git assumes that you always want local branches to
  correspond to remote branches by the same name.
@~
  Instead of cloning a repository to create a new one, you could also start
  with an existing repository and simply push it to a yet-to-exist repository
  on the server.  Again, the server will see that it doesn't exist and will
  create it for you (provided that it is in your directory).  To continue the
  above example, I could now create a new repository from @cmd{foo}:
  @pre{# (still in the foo directory)
       git push pltgit:$user/bar}
@~
  There is, however, an issue of efficiency here: with this last command I just
  created a second copy of it all.  This could be problematic if you have a
  large repository (eg, a copy of the @cmd{plt} repository).  (Note that with
  subversion this is the only way to do things, but there you would create
  copies inside the tree, which subversion optimizes.)  One nice feature of git
  is that creating a clone of a repository on the same filesystem will use
  hard-links for the clone, which makes the clone use very little additional
  space.  But the problem is that you have no access to the PLT server.  The
  solution here is in the form of a gitolite @cmd{fork} command (this is
  actually our own extension) — this command will create a clone on the server,
  starting from a specified repository.  I could therefore create my @path{bar}
  repository as a copy of @path{foo} with the following:
  @pre{ssh pltgit fork $user/foo $user/bar}
  (Note that if you follow these examples and you already have @path{bar}, the
  server will tell you about it.)  The result is a @path{$user/bar} repository
  that was cloned from @path{$user/foo}, and the two share their store using
  hard links.  If the two repositories are updated with identical content, the
  new content will not be shared, but for a large repository like the @cmd{plt}
  repository you still get the benefit of having the bulk of the data shared
  (the complete store, at the time of forking).  To get a feeling for how fast
  this is, you can now clone the @cmd{plt} repository to your own private copy:
  @pre{ssh pltgit fork plt $user/myplt}
  This would seem suspiciously fast for such a large repository — but this
  repository has most of the data packed (objects in the store are put in large
  “pack files”), so there are not too many files, and the server-side cloning
  basically created hard links to these files.  The result is fast, efficient
  (even in speed: when you interact with your clone, files are likely to be
  paged in memory), and cheap.
@~
  As we've seen above, the gitolite @cmd{info} command lists the permissions
  that you have, but it doesn't show you the actual repositories.  For this,
  there is an @cmd{expand} command.  (Yes, this is not a great name; it's
  related to how gitolite was intended to be used.  Remember that there is also
  a @cmd{help} command that describes the available gitolite commands.)  When
  you run the expand command — @cmd{ssh pltgit expand} — you get a listing of
  all of the repositories that you can access, each with an indication of read
  permissions (@cmd{R}) or write permission (@cmd{W}).  A @cmd|{@}| indicator
  means that you have the respective permissions because it is allowed for all
  users.  Each repository is also listed with its owner, or @cmd{<gitolite>} in
  case it is a globally configured repository.
@~
  Some of the gitolite commands are used to configure your repositories — you
  can only use these with repositories that you own.
  @ul*{
  @~ @cmd{getperms} and @cmd{setperms} — these are used to get or set
     permissions for your repositories.  The first will print the current
     permissions (which will be initially empty), and the second will read the
     permissions on its input and set them.  The format of the permissions is
     simple: each line begins with an @cmd{R} or @cmd{RW}, and then the
     usernames that this permission applies to.  You can use the magic username
     @cmd|{@all}| to grant access to everyone in the system.  For example, to
     grant read permissions to everyone, and write permissions to user1, create
     a file with:
     @pre|{R @all
           RW user1 user2}|
     and then run the setperms commands with this as its input:
     @pre{ssh pltgit setperms $user/foo < the-file}
     You can also just run the @cmd{setperms} command and type in the
     permissions directly.  Note that these permissions are not cumulative:
     every use of @cmd{setperms} specifies all permissions.  (We might have a
     more convenient interface for all of this in the future.)
  @~ @cmd{config} — this command can be used to set known configuration options
     in your repositories.  It works with sub-verbs:
     @dl*{@~ @cmd{ssh pltgit config list}
          @~ Displays the known configuration options
          @~ @cmd{ssh pltgit config get @i{repo} @i{config}}
          @~ Displays the configuration value of a specific repository
          @~ @cmd{ssh pltgit config set @i{repo} @i{config} @i{value}}
          @~ Sets the configuration value of a specific repository}
     These configuration options can customize aspects of the scripts that run
     after every push — currently, you can use this to set an email address to
     send notification emails to.  Other configurations may be added in the
     future.  (Note that this does not let you set any configuration, since
     some of these can execute arbitrary commands.)
  @~ @cmd{delete} — finally, this command can be used to delete repositories.
     For example, to clean up the above, you can now run:
     @pre{ssh pltgit delete $user/foo
          ssh pltgit delete $user/bar}
     The repositories are moved to a temporary holding directory, and will
     eventually be removed.  The bottom line here is that if you lost anything
     by mistake recently, chances are there's a backup of your repository.}}

@section{Working with git}

@subsection[#:newpage? #f]{Basics}
@p*{
  The above description is much simplified in that it doesn't deal with
  development that happens outside of your own work — and such development
  obviously changes the story.  Overall, this is not too different from working
  with subversion: if there were any changes on the server you need to update
  your working copy first, and this implies dealing with conflicts if there are
  any.  But the way to deal with such things in git is significantly different
  than dealing with them in subversion, and this difference is at the technical
  level (different commands) and at the workflow level (you will likely branch
  much more, and you're likely to push less frequently than you commit with
  subversion).
@~
  To start working, you first need to get a repository clone.  Usually you will
  clone the PLT repository (or a private copy that you do your work in), but
  remember that to experiment with git you have the @cmd{play} repository or
  you could make a fork of the PLT repository to play with and remove it when
  you're done.  Either way, be sure to try these things out — it will make your
  life much easier in the future.
@~
  In the following examples I will use an empty repository to demonstrate
  things and I will list the exact commands that I'm using (this means that I
  will use unix commands to create and edit files, and use @cmd{-m} when
  committing).  Lines that I enter are displayed with a @cmd{$} prompt, most
  output lines are omitted, comments start with @cmd{#}, and @cmd{$user} is set
  to my username.  Note that if you try this yourself, the SHA1s of commits
  will be different (the reason for that is that a commit object includes the
  author name and email and the date).  Note also that in some places I will
  “jump to an earlier continuation”: start from an earlier state and do
  something different — so if you want to try these things out it will be
  convenient to put the commands in a shell script so you can re-run it to get
  to the earlier state.
@~
  First, I create a private empty repository, populate it, and update the
  remote repository:
  @pre{$ user=eli                    # your own username here
       $ mkdir /tmp/sandbox; cd /tmp/sandbox
       $ ssh pltgit delete $user/foo # delete previous repository, if any
       $ git clone pltgit:$user/foo
       $ cd foo
       $ echo "foo" > foo; echo "bar" > bar
       $ git add .
       $ git st                      # uses the `st' alias as shown above
       A  bar
       A  foo
       $ git commit -m "initial content" .
       [master (root-commit) 87f1f02] initial content
       @i{...}
       # git tells us the branch we committed to, the new commit SHA1 and
       # that this is the first commit, and the log message; we can verify
       # this now with `git log'
       $ git log
       commit 87f1f02c23b32e7f9b...  # this is the commit object I created
       @i{...}
       $ git push                    # (or `git push origin master' if needed)
       To pltgit:eli/foo             # where we pushed to, and the branch
        * [new branch]      master -> master}
@~
  @small{[A quick note on commit messages: several git command consider the
    first paragraph of your commit message as a short description for it.  This
    is all of the text up to the first blank line if you write a commit message
    in an editor, or the first @cmd{-m} message if you use it with
    @cmd{git commit} (it accepts multiple @cmd{-m} arguments, for multiple
    paragraphs).  Keep this in mind when composing such messages.]}
@~
  To see what happens when multiple people commit to the repository, we create
  a second clone of our repository now in a @path{foo2} directory:
  @pre{$ cd ..
       $ git clone pltgit:$user/foo foo2
       $ cd foo                      # go back to foo now}
@~
  Lets make two new commits now:
  @pre{$ echo "more foo" >> foo; echo "more bar" >> bar
       $ git ci -m "more stuff"      # uses the `git-ci' script from above
       [master b7d3c41] more stuff
       $ echo "even more foo" >> foo
       $ git ci -m "even more stuff"
       [master 18bc0e6] even more stuff}
@~
  At this point, instead of blindly pushing these commits, lets look around
  first.  One useful tool for inspecting the history is @cmd{gitk} — if you run
  it now, you will see the simple 3-commit graph, and two of them are marked as
  branches — clearly showing that your local master branch is 2 commits ahead
  of the remote one.  This could be different at this point: someone else might
  have pushed more commits to the remote — remember that your remote master
  branch (@cmd{remotes/origin/master}) is not really what's on the remote, but
  rather what you know about it last time you pulled from it.
@~
  Another useful command to examine the history is @cmd{git log}, which can
  show commit history in many ways.  As things stand in the current repository,
  if you just run @cmd{git log}, you will see a listing of the same three
  commits that gitk shows.  To get a more condensed format with
  one-line-per-commit, use @cmd{--oneline}.  Another thing that you can do is
  show a specific range of commits — you can do this by specifying two
  revisions separated with @cmd{..}, where the revisions can be written
  explicitly using the (short prefix) SHA1 form, or more conveniently using a
  symbolic name (eg, branch, tag, HEAD):
  @pre{$ git log origin/master..master}
@~
  Since we @em{are} currently on the master branch, we could use @cmd{HEAD} for
  the second one (@cmd{origin/master..HEAD}), but this is also the default, so
  an even shorter form is @cmd{origin/master..}.  In addition to @cmd{git log},
  you can also use @cmd{git diff} in a similar way, but instead of a commit
  listing, you get the diff between the two specified points, so
  @pre{$ git diff origin/master..}
  will show you the changes that you did not yet push.  Note that there are a
  number of places where git will guess the full name of branches, for example,
  @cmd{origin/master} is actually a short name for @cmd{remotes/origin/master}.
  In a similar way, just @cmd{origin} will make git guess that you're talking
  about @cmd{origin/master}.
@~
  In these cases, the revision specification for @cmd{log} and @cmd{diff} are
  the same, but this is a little misleading: @cmd{git diff} usually works by
  comparing two specific end points in your history, but @cmd{git log} actually
  works on a @em{set of commits} rather than on a range.  The @cmd{R1..R2}
  notation is actually shorthand for @cmd{^R1 R2} — specifying a commit means
  “the commit and all of its parents”, and a @cmd{^} prefix negates a set, so
  @cmd{^R1 R2} means “include the set of commits leading to R2 (inclusive), but
  exclude the ones leading to R1 (inclusive)”.
@~
  In addition to this range/set specification, there is a lot to specifying a
  revision set.  As mentioned, you can use a SHA1 (or a shorter unique prefix),
  or you can use a symbolic name.  You can also use @cmd{R^} for the parent of
  @cmd{R} (the first parent in the case of merge commits, which have more than
  one parent), @cmd{R^^} would be the grand parent, @cmd{R~3} is the
  3rd-generation parent commit.  There is also @cmd|{R@{yesterday}}|,
  @cmd|{R@{1 month 2 weeks go}}| etc for a symbolic name R — which refers to
  the branch/HEAD at that point in time (this refers to @em{your own} version
  of it at that time; there are @cmd{--since} and @cmd{--until} flags to filter
  commits by the time they were made at).  You can also use a -N flag (where N
  is an integer) to show only N commits.  Finally, you can use a branch name R
  with @cmd|{R@{upstream}}| (short: @cmd|{R@{u}}|) to refer to the “upstream”
  version of the branch — the branch that R is set to follow.  This is
  particularly convenient for things like @pre|{$ git log --oneline @{u}..}|
  which will always show the commits that you have over the branch your current
  one follows.  (For example, you could set up an alias to use this.)
@~
  We now continue by pushing our two commits to the remote server.  Since we
  already did a push, a plain @cmd{git push} works fine.
  @pre{$ git push                    # no need to specify a branch now
       To pltgit:eli/foo
          87f1f02..18bc0e6  master -> master}
@~
  As you can see in the last line, git tells us that we pushed from our local
  @cmd{master} branch to the remote @cmd{master} branch, and that this made it
  advance from the first commit we pushed (87f1f02) to the last one we created
  now (18bc0e6).
@~
  Since we've made some progress in one place, we can go to the @path{foo2}
  clone to see what happens when we update a repository that did not have these
  changes:
  @pre{$ cd ../foo2
       $ git pull
       @i{...}
       From pltgit:eli/foo
          87f1f02..18bc0e6  master     -> origin/master
       Updating 87f1f02..18bc0e6
       Fast-forward
        bar |    1 +
        foo |    2 ++
        2 files changed, 3 insertions(+), 0 deletions(-)}
@~
  This looks expected — git shows the new commits that we received, and they're
  the same as what we pushed earlier.  Using @cmd{git pull} is actually doing
  two things: it's running @cmd{git fetch} first to update your remote branch
  from the server, and then it uses @cmd{git merge} to merge it into your
  master branch.  The point where @cmd{get merge} starts is the “Updating” line
  — and there's an important thing to note here: the next line says
  “Fast-forward”, which is a special kind of a merge.  When you merge some
  branch into your branch, and this branch is a proper superset of your branch
  (it has commits that your branch doesn't, and all commits in your branch are
  included in it), git will simply “move your branch forward” to the other: it
  will update your branch to the tip of the merged one, and then your working
  directory will be update accordingly.
@~
  It is often better to do the fetch first, so you can see the changes that
  happened remotely before you merge them.  To do this we're going to use
  @cmd{git fetch}, avoiding the merge step that @cmd{git pull} does.  In fact,
  since creating a merge commit is something that you might want to always do,
  @cmd{git pull --ff-only} will only do the merge if it will be a fast-forward
  merge.
@~
  Assuming we start again from the @path{foo2} repository before the above
  pull, we get the same output up to the point where the merge started:
  @pre|{$ git fetch
        @i{...}
        From pltgit:eli/foo
           87f1f02..18bc0e6  master     -> origin/master
        $ git log --oneline @{u}..
        # nothing
        $ git log --oneline ..@{u}
        # the same two commits}|
  The first log doesn't show anything, since we have no commits over the ones
  in the remote (the “upstream” of our current branch).  To see this, consider
  that after expanding empty names to @cmd{HEAD}, and the @cmd|{@{u}}| to the
  remote branch name we get @cmd{remotes/origin/master..master}, and this is
  short for @cmd{^remotes/origin/master master} — the set of commits made of
  our master branch and all parents, minus the set of commits from the remote
  branch and up — since it's ahead of that, we get an empty set.  The second
  log command reverses the two, giving us the set of commits that the remote
  has and the local branch doesn't.  In addition to @cmd{git log}, you can use
  @cmd{gitk} to inspect the repository: use the @cmd{--all} flag to make it
  show all branches.  Either way you'll be able to see that a fast-forward
  merge is possible.}

@subsection{Concurrent development}
@p*{
  Again, we'll assume starting with the @path{foo2} repository before the pull.
  We will now create a new commit before we get changes.  This makes it similar
  to commits pushed to the server while you do your work — so let's see how
  this common story goes and try to push this change:
  @pre{
    $ echo "blah blah" > blah
    $ git add blah
    $ git ci -m "blah"
    $ git push
    To pltgit:eli/foo
     ! [rejected]        master -> master (non-fast-forward)
    error: failed to push some refs to 'pltgit:eli/foo'
    To prevent you from losing history, non-fast-forward updates were rejected
    Merge the remote changes before pushing again.  See the 'Note about
    fast-forwards' section of 'git push --help' for details.}
@~
  As expected, git refuses to push our change.  The terminology in the error
  message is a little confusing — what it basically says is that the commit(s)
  that we are trying to push are not an extension of the tip of the master
  branch on the server.  A “non-fast-forward update” in this case would mean
  that we'd set the master branch on the remote to be the same as our branch —
  but this means that whatever commit that were pushed to the server (the two
  commits we pushed out from the @path{foo} clone in this case) will be lost.
@~
  To merge in the remote changes, we need to pull them in.  We'll now look at
  three different ways to do this.}
@h3{1. Playing it safe}
@p*{
  First, as we've seen above, doing a separate @cmd{git fetch} step would allow
  you to see where things stand before you do anything.  Alternatively, we can
  use the @cmd{--ff-only} variant of @cmd{git pull}, which will do a merge only
  if it's a fast-forward one, covering the trivial cases.  If a fast-forward
  merge cannot be done, it will tell you about it and then stop:
  @pre{$ git pull --ff-only
       @i{...}
       From pltgit:eli/foo
          87f1f02..18bc0e6  master     -> origin/master
       fatal: Not possible to fast-forward, aborting.}
@~
  We can now use the usual tools to see where things stand.  The following are
  all useful here:
  @ul*{
  @~ @cmd{gitk --all}@br
     This visualizes the commit graph.  If you do this, you will see the four
     commits that we have so far: the initial commit at the root, the commit
     that we did in this clone, and the two commit that we retrieved and are
     waiting on @cmd{remotes/origin/master}.
  @~ (a) @cmd|{git log @{u}..}|@br
     (b) @cmd|{git log ..@{u}}|@br
     (c) @cmd|{git diff @{u}..}|@br
     (c) @cmd|{git diff ..@{u}}|@br
     Inspect the commit that the local branch has over the remote (a), and the
     two that the remote has over the local one (b); look at the difference
     between the local branch and the remote either way (c).
  @~ (a) @cmd|{git log --left-right --oneline @{u}...}|@br
     (b) @cmd|{git log --left-right --oneline ...@{u}}|@br
     (c) @cmd|{git log --graph --oneline --all}|@br
     An alternative notation for specifying commit sets for @cmd{git log} is
     @cmd{R1...R2} (with @em{three} dots) — this stands for all commits from
     both R1 and R2 and their parents, but excluding commits from their “merge
     base” — the parent commit that both descend from.  As demonstrated in (a),
     this is especially useful with the @cmd{--left-right} flag: you'll see the
     commits that are new on the remote branch and the ones that are new on the
     local one, with @cmd{<} or @cmd{>} indicating which side each commit is
     coming from.  Yet another way that @cmd{git log} can be used is (c) with a
     @cmd{--graph} flag, which makes it render the commit graph in ASCII-art.
  @~ @cmd{git show-branch -a}@br
     This is another potentially useful commands that shows how commits are
     distributed over branches in your repository.  In this case you will see a
     matrix with the four commits in separate rows, and each will have a
     @cmd{+} or @cmd{*} indicating whether it is included in a branch.
  @~ (a) @cmd|{git diff @{u}...}|@br
     (b) @cmd|{git diff ...@{u}}|@br
     The three-dots notation is also used by @cmd{git diff}, with a slightly
     different semantics than in @cmd{git log} (remember that @cmd{git log}
     talks about commit sets, and @cmd{git diff} compares two specific points).
     In the @cmd{diff} case, these compare a specific branch tip to the merge
     base of this branch and another, which means that you see a diff with the
     work done on one branch that is not included in the other (this is unlike
     the two-dot syntax where you get the diff between the two branch tips).
     In the first (a) example you will see all changes that you did locally,
     and in (b) the changes that were done remotely.}}
@h3{2. Merging (not the fast-forward variant)}
@p*{
  Now that we did a @cmd{git fetch} or a @cmd{git pull --ff-only} to update the
  remote branch, we can proceed with merging it into our branch — which we can
  do in one of two ways:
  @pre{$ git merge origin            # merge origin/master into master
       Merge made by recursive.
       @i{...}
       # -or-
       $ git pull                    # will do a merge as usual}
@~
  Using @cmd{merge} is better for the usual reason: @cmd{git pull} can bring in
  more updates that were pushed by others since you fetched, and include them
  in the merge.  Note that if instead of running a separate fetch or pulling
  with @cmd{--ff-only} you were using @cmd{git pull}, then you'd essentially
  get to the same point we are now at.
@~
  Either way, the merge tells us “Merge made by recursive” — which is important
  here: if we see “Fast-forward” it means that no new commits were made, but
  “Merge made ...” means that a merge commit was created (“by recursive” refers
  to the merge strategy, git has several of them).  If we look at the commit
  graph now with gitk or with @cmd{git log}, we'll see the new commit that was
  created:
  @pre{$ git log --oneline --graph --date-order
       *   12bf7ee Merge remote branch 'origin'
       |\
       * | a40a45f blah
       | * 18bc0e6 even more stuff
       | * b7d3c41 more stuff
       |/
       * 87f1f02 initial content}
  The merge was successful without manual intervention (you didn't need to
  resolve any conflicts), so it proceeded immediately to create the commit
  that connects the two lines of development, and used some standard
  template for the commit log.  This is the safe thing to do as the
  default for git, but in this case it makes the history complicated with
  no good reason — it just happened that while we were working on @cmd{blah}
  someone else pushed an unrelated change.  (It could be related: perhaps
  one of the remote commits was referring to the file that I've added, but
  especially in the PLT case this would be very rare.)  If you use gitk to
  look at the recent history of the repository, you'll see many such
  commits, and they can make it harder to figure out how the development
  went on.}
@h3{3. Rebasing}
@p*{
  To get a simple/readable history, the goal is to have a more linear history:
  just have the two remote commits and move our commit to follow them (which
  would be the history that you get with subversion under similar conditions).
  But our commit object already points to its parent, so it @em{cannot} move:
  in the above graph, @cmd{a40a45f} is a hash that was computed based on
  @cmd{87f1f02} being its parent, and changing a parent means getting a new
  hash and therefore a new commit object.
@~
  This is where @cmd{git rebase} gets into the picture.  Assuming that we
  didn't merge as described above, we would just use @cmd{rebase} instead:
  @pre{$ git log --graph --all --oneline
       * a40a45f blah                # \
       | * 18bc0e6 even more stuff   #  \
       | * b7d3c41 more stuff        #   > same tree as before the merge
       |/                            #  /
       * 87f1f02 initial content     # /
       $ git rebase origin
       First, rewinding head to replay your work on top of it...
       Applying: blah                # git tells us that it re-applies this
       $ git log --graph --all --oneline
       * 6ebd1fb blah                # \
       * 18bc0e6 even more stuff     #  \ the resulting history
       * b7d3c41 more stuff          #  / is linear
       * 87f1f02 initial content     # /}
@~
  Here's what git did in this rebase: it (1) moved the HEAD to the merge base
  between your local branch and the remote one — the @cmd{87f1f02} commit; (2)
  did a fast-forward merge, which just moves your local branch to the tip of
  the remote one; (3) it now @em{replays the same changes} that you had in your
  commits (only @cmd{a40a45f} in this case) on top of the new tip, leading to
  @em{new} commit objects (@cmd{6ebd1fb} in here).  So we end up with a fresh
  commit object, and the old one (@cmd{a40a45f}) is gone.  (It's not really
  gone — it's kept in your repository store for a while, to protect you from
  losing work.)  If you look at the complete details of the new commit using
  gitk or @cmd{git log}, you will see that this commit has different dates for
  the author and the committer date:
  @pre{$ git log --format=fuller -1 6ebd1fb # all details, show only one commit
       @i{...}
       AuthorDate: 2010-05-02 10:26:00 -0500
       @i{...}
       CommitDate: 2010-05-02 10:30:00 -0500}
  This is because rebasing just created a new commit — but the author time is
  still considered the same.  In any case, you now have linear history that is
  a proper descendant of the remote branch, and you can now push your changes
  out.
@~
  Practically every place where you read about rebasing in git will warn you
  about not doing it for public history.  The problem is that if someone had a
  copy of your previous branch (@cmd{a40a45f}), then next time they will
  update, if their copy of the branch is updated, then things will change in a
  nasty way: even more so if they've committed more changes on that branch.
  (This “someone” can also be yourself, of course.)  This also explains why
  @cmd{git pull} does not rebase by default.
@~
  As far as the PLT repository goes, server will never allow pushes that are
  not strict extensions of what's on it (in other words, it only allows
  fast-forward pushes) — so you won't be able to mess things up for others.
  But as long as your change is something that you work on privately, there is
  absolutely no problem in doing this.  Note that this is the same thing as
  with re-doing commits because of mistakes: as long as a commit did not go
  out, you can fix it in multiple ways; when it does get pushed to the server,
  the only practical way to fix it is by pushing another commit.  (In some
  extreme cases we may do such a thing: for example, if you commit a passwords
  file, then there is no other way to remove it completely from the repository
  — but these are very rare, and such fixes affect everyone.)
@~
  It is therefore best to get one of two habits when you do a @cmd{git pull}:
  either use @cmd{--ff-only} or @cmd{--rebase}.  The latter is a little more
  convenient but you might not feel comfortable about doing a rebase
  automatically — it @em{might} just be that someone worked on the same set of
  files, and you really prefer a plain merge.  For this, you might prefer using
  @cmd{--ff-only} which will automatically work in the trivial cases, and
  otherwise leave you in a state where you can look at things and decide how to
  proceed yourself.}

@subsection{Additional forms of history tweaking}
@p*{
  As described in the previous section, rebasing is not some kind of a magical
  operation: it is really just an expected by-product of the way git works — of
  the fact that commits can be created as descendants of any commit (not just
  tip commits).  You could perform a rebase manually by starting from some
  commit, then inspect each of the changesets that the rebased history
  contains, and play them back on the new commit.  (Lumping this tediousness
  lead to a script, which lead to the rebase command.)  This means that you
  don't really have to limit yourself to replaying these commits exactly as
  they were — for example, you could write new commit log messages, combine two
  commits into one, drop some commits, or reorder their order.
@~
  The rebase command has a flag that makes doing all of these things easy:
  @cmd{--interactive}.  Continuing the above example, we now have four commits
  in our history — and say that we want to tweak the last two.  If we now run
  @pre{$ git rebase HEAD~2}
  we ask git to rebase our current head off of its grandparent commit (remember
  that @cmd{HEAD^} is the parent, @cmd{HEAD^^} is the grandparent, and
  @cmd{HEAD~2} is an alternative syntax for @cmd{HEAD^^}).  Since it @em{is}
  already based there, @cmd{git rebase} does nothing, and tells us that the
  branch is up to date.  But if we add @cmd{--interactive}, we get something
  different: git pops up an editor with this text:
  @pre{pick 18bc0e6 even more stuff
       pick 6ebd1fb blah
       # Rebase b7d3c41..6ebd1fb onto b7d3c41
       # @i{...}}
  This is a listing of the last two commits with their one-line log messages.
  As the text that is below these lines say, you can replace the @cmd{pick}
  before a commit with a different command: you can use @cmd{reword} to get to
  write a different log message (you will get another editor window to do the
  editing), @cmd{squash} to combine a commit with the previous one (it will let
  you edit the log message for the combination), @cmd{fixup} which does the
  same but discards the log message, and finally @cmd{edit} will make the
  rebasing process stop at the relevant commit and let you tweak it before it
  continues.  In addition, removing a commit line means that the commit will be
  skipped, and reordering lines will replay the commits in a different order.
  If any of these lead to conflicts, the rebasing will stop for a manual
  resolution, and you'll need to @cmd{git rebase --continue} when resolved, or
  @cmd{git rebase --abort} to get back to the original state.
@~
  Note that since our @path{foo2} clone tracks a public @path{foo} repository,
  this particular rebase is bad: we intend to edit the last two commits, but
  only one is local to @path{foo2} — the other is a commit that we got from
  @path{foo}, and changing it means that we will get a rebased commit based on
  its parent, and the server will forbid pushing it later.  If you see that you
  went too far when you see the rebase editor, all you need to do is keep those
  lines untouched: in the trivial case of leaving an initial set of commits in
  unmodified, they will be “rebased” by leaving them in as is.  (If you inspect
  the history later, you will see that they have the same SHA1s.)
@~
  A useful case of using @cmd{squash} (or @cmd{fixup}) with interactive
  rebasing is doing @cmd{checkpoint} commits frequently, and eventually
  combining them to a single commit.  This demonstrates one a popular git
  principle: keep commits as logical units that correspond with the changes
  done, since there is no central server that dictates a
  public-commit-or-nothing.  Doing these will make it easier in the future to
  deal with the history: inspect the changeset as a whole, undo it, and it also
  works well with finding bugs in the history — you can have checkpoints for
  intermediate states of the code even if it doesn't work, since this state
  will eventually be hidden.
@~
  A particularly common case for editing history is “oops commits”: you just
  made some change, committed it, and then realized that something is wrong —
  you forgot to change some related reference, to remove some debugging
  printout, or to describe some new aspect of the commit.  You could use
  @cmd{git rebase HEAD^} in these cases to rebase just the last commit while
  editing it, but there is a much more convenient way to do this: @cmd{git
  commit --amend}.  Usually, @cmd{git commit} creates a new commit based on the
  current branch tip and a given commit log message, but with @cmd{--amend} it
  does something different: it takes a snapshot of the tree as usual, but it
  makes the commit be a descendant of the tip's parent commit.  For example,
  assuming we didn't really change anything with the rebase above, our history
  and recent change is now:
  @pre|{$ git log --oneline
        6ebd1fb blah
        18bc0e6 even more stuff
        b7d3c41 more stuff
        87f1f02 initial content
        $ git log --oneline -p -1     # -p => show patch, -1 => only one
        6ebd1fb blah
        diff --git a/blah b/blah
        @i{...}
        --- /dev/null
        +++ b/blah
        @@ -0,0 +1 @@
        +blah blah                    # this is the recent change}|
  This is obviously wrong — we need to have three “blah”s there.  With
  subversion we would now need to perform an “oops, forgot a blah” commit, and
  in fact, we would need to do the same with git if we push these change out
  now.  But as long as we didn't, we can fix it without an additional commit
  using @cmd{--amend}:
  @pre{$ echo "blah blah blah" > blah
       $ git add blah
       $ git ci --amend -m "blah^3"
       [master 5cf863d] blah^3       # the re-made commit
       $ git log --oneline
       5cf863d blah^3
       18bc0e6 even more stuff
       b7d3c41 more stuff
       87f1f02 initial content}
  As you can see, the last commit is gone (remember that it is still backed up,
  in case of problems), and there is a completely new commit instead.  Usually,
  @cmd{--amend} is used without @cmd{-m} — the log message editor will be
  initiated with the previous log message, so you can edit it instead of
  rewriting it from scratch.  If there were no modifications to be committed, a
  @cmd{git commit --amend} is a convenient way to edit the last commit message
  only.  If you leave the message untouched, a new commit will still be made —
  one with a new commit time; and if you delete the text completely, the
  re-commit will be aborted, and you will be left with the old one intact.}

@subsection{Resetting the tree}
@p*{
  Both the @cmd{commit --amend} feature and rebasing build on the ability to
  “move” the current branch tip to some earlier commit in its history.  To do
  this directly, git provides a @cmd{git reset} command, which can move the
  current branch tip to a specified commit, and adjust the working directory
  and/or the staging area accordingly.  For example, for the @cmd{--amend}
  functionality, you will use @cmd{HEAD^} to move the branch tip to its parent
  commit.  You can of course specify any other commit to move to, and since git
  branches are effectively short bookmarks, you can create branches to be able
  to move to them later on (or as targets for rebasing, merging, etc).  In
  addition, you can use @cmd{HEAD} (or just omit the target, since @cmd{HEAD}
  is the default) to only change the working directory (or staging area).
@~
  The reset command has three major modes for its work, specified with a flag.
  (See the @man{git-reset} man page for a more thorough explanation, with lots
  of usage examples, some have evolved into their own functionality — like the
  @cmd{--amend} feature.)  Using @cmd{HEAD^} as the target commit, here are
  some summaries of how it can be used:
  @ul*{
  @~ @cmd{git reset --hard HEAD^}@br
     This will move the branch tip to the previous commit, and will change the
     working tree and the staging area to match.  Translation: completely
     forget the last commit and any work in the working directory.
  @~ @cmd{git reset --soft HEAD^}@br
     Moves the branch tip, but does not change the working directory or the
     staging area.  Translation: undo the last commit, and leave your working
     directory in a state where @cmd{git commit} will get the same change in.
     (Note: not the @cmd{git ci} script — this will add changes in the working
     directory, if any.)
  @~ @cmd{git reset --mixed HEAD^}@br
     Moves the branch tip and the staging area to the parent commit, and any
     modifications done by the commit that are going to be lost are put in your
     working directory.  Translation: similar to the @cmd{--soft} version,
     except that the staging area is cleared, so to recommit the changes you
     will need to add files again (or use @cmd{git ci} as usual).
     @br
     (Note: This is the default mode.)}
@~
  When using @cmd{HEAD} (which is the also default when nothing is mentioned),
  the branch tip is not moved, and we get:
  @ul*{
  @~ @cmd{git reset --hard}@br
     Get rid of all changes to the working directory and the staging area.
     Translation: lose all work that was not committed, getting back to the
     content on the branch (a convenient way to do something similar to an
     @cmd{svn revert -R .} in the root of a subversion working directory).
  @~ @cmd{git reset --mixed}@br
     Get rid of all changes to the staging area, leaving your working directory
     intact.  Translation: lose everything that was added to the staging area.
     If you're avoiding it (for example, if you only use the @cmd{git ci}
     script), then this would be a no-op other than new files that were
     @cmd{git add}ed.
  @~ (@cmd{git reset --soft} is a no-op.)}
@~
  When @cmd{git reset} changes the HEAD, it creates another toplevel reference
  name called @cmd{ORIG_HEAD} that points to the previous commit that
  @cmd{HEAD} pointed at, so if you happen to @cmd{git reset --hard HEAD^} by
  mistake, you can immediately get back to it with
  @cmd{git reset --hard ORIG_HEAD} (but changes in the working tree would still
  be lost).  (@cmd{git merge} is another command that changes the @cmd{HEAD},
  and it also saves the previous value in @cmd{ORIG_HEAD}.)  Finally, note that
  @cmd{git reset} can be restricted to make it work only on a specific set of
  paths, not on the whole repository.}

@subsection{Other forms of reverting}
@p*{
  While we're on the topic of reverting files, there are three more things
  worth mentioning:
  @ul*{
  @~ @cmd{git checkout -- @i{path ...}}@br
     @p*{
       When @cmd{git checkout} is given some paths, it will only check out the
       relevant files from their state in the staging area.  This is a more
       popular way to revert changes to a specific file.  If you avoid using
       the staging area, then this is roughly the same as using reset with the
       @cmd{--hard} flag on the paths, since your staging area will usually be
       the same as your @cmd{HEAD}.  (Note that the @cmd{--} is optional, and
       needed only when a path name can be confused with a branch name.)
     @~
       As with @cmd{reset}, you can also specify a branch to check the path(s)
       from — which is useful to try some files from a different branch
       selectively.  However, note that unlike subversion, git does not
       remember the association of the branch and the paths that were checked
       out of it (the branch is not “sticky”) — the files will simply be
       considered as modified (and they will not be updated when the branch is,
       unless you do the same checkout).}
  @~ @cmd{git show HEAD:@i{path}}@br
     @p*{
       This shows the file as it exists in the @cmd{HEAD}, making it useful to
       inspect the file before you made some additional modifications (similar
       to @cmd{svn cat @i{path}}).  You can also omit the @cmd{HEAD} — using
       @cmd{:@i{path}} will show the file in the staging area, which will
       usually be the same as the @cmd{HEAD}.  One caveat to note here is that
       the path should be the full path relative to the repository root.
       (Note: I have a wrapper @cmd{git cat} script that emulates
       @cmd{svn cat}, I'll add it if anyone wants.)}
  @~ @cmd{git revert @i{commit}}@br
     @p*{
       The @cmd{git revert} command is used to revert the changes introduced by
       the given commit.  It will basically apply the change in that commit
       in reverse, then ask you for a log message for a new commit where the
       message is initially populated with text indicating the commit that
       was applied in reverse.
     @~
       Note that this is very different from @cmd{svn revert} — it is more like
       @pre{svn merge -c-123@";" svn commit "Revert revision 123"}
       Since this is a frequent source of confusion, the @man{git-revert} man
       page mentions it at the top, and it refers readers to @cmd{git reset}
       and @cmd{git checkout} as the way to do the equivalent of @cmd{svn
       revert} (which are described above.)}}}

@subsection{Dealing with conflicts}
@p*{
  We'll now see how to deal with merge conflicts.  First, we'll set up the
  repository for a conflict.  Continuing with the @path{foo2} clone, we'll
  first create a file (which I'll do here using shell commands, to make it easy
  to play with), commit, and push the new history (which includes the blah
  work) back to the server.  Note the use of @cmd{git branch -v} which shows
  the local @cmd{master} branch and the fact that there's two commits that we
  haven't pushed out yet.
  @pre{$ echo "#lang racket"    > foo
       $ echo "(define (foo x)" >> foo
       $ echo "  (* x x))"      >> foo
       $ git ci -m "turn foo into a library"
       [master fd856ef] turn foo into a library
       $ git branch -v
       * master fd856ef [ahead 2] turn foo into a library
       $ git push
       To pltgit:eli/foo
          18bc0e6..fd856ef  master -> master}
@~
  Now hop over to the @path{foo} clone, get the changes (the relevant bits of
  the output are shown), edit the file (using sed, to make it a command line),
  inspect the change, commit it, and push.
  @pre|{$ cd ../foo
        $ git pull
        From pltgit:eli/foo
           18bc0e6..fd856ef  master     -> origin/master
        Updating 18bc0e6..fd856ef
        Fast-forward
         blah |    1 +
         foo  |    6 +++---
         2 files changed, 4 insertions(+), 3 deletions(-)
         create mode 100644 blah
        $ sed -i '2s/x/[x 0]/' foo
        $ git diff
        diff --git a/foo b/foo
        index 78d9889..b81de80 100644
        --- a/foo
        +++ b/foo
        @@ -1,3 +1,3 @@
         #lang racket
        -(define (foo x)
        +(define (foo [x 0])
           (* x x))
        $ git ci -m 'add a default value'
        [master 5035c9a] add a default value
        $ git push
        To pltgit:eli/foo
           fd856ef..5035c9a  master -> master}|
@~
  And now get back to @path{foo2}, and before we pull, modify the same line by
  adding a comment and commit, then do a @cmd{--ff-only} pull and watch it
  refuse to merge as expected, then look at the history so far.
  @pre{$ cd ../foo2
       $ sed -i '2s/$/ ; int->int/' foo
       $ git ci -m 'document the type of foo'
       [master 21a78df] document the type of foo
       $ git pull --ff-only
       From pltgit:eli/foo
          fd856ef..5035c9a  master     -> origin/master
       fatal: Not possible to fast-forward, aborting.
       $ git log --graph --all --oneline -4
       * 21a78df document the type of foo # ← our change
       | * 5035c9a add a default value    # ← the conflicting change we pulled
       |/
       * fd856ef turn foo into a library
       * 5cf863d blah^3}
@~
  Rebasing is the common thing to do, but let's see what happens with a plain
  @cmd{merge} first:
  @pre{$ git merge origin
       Auto-merging foo
       CONFLICT (content): Merge conflict in foo
       Automatic merge failed; fix conflicts and then commit the result.}
@~
  We now have a conflict that needs to be resolved before we can finish the
  merge.  Using @cmd{git st} (the alias listed above for the svn-like status
  that @cmd{git status -s} produces) shows a new @cmd{UU} status for @path{foo}
  — this indicates an “unmerged” (conflicted) file.  To investigate further, we
  use a plain @cmd{git status}, which tells us that our history diverged from
  the remote (we already know that since @cmd{pull --ff-only} failed) and count
  the diverging commits, and it also tells us that @path{foo} is unmerged and
  hints at using @cmd{git add} to resolve it:
  @pre{$ git st
       UU foo
       $ git status
       # On branch master
       # Your branch and 'origin/master' have diverged,
       # and have 1 and 1 different commit(s) each, respectively.
       # Unmerged paths:
       #   (use "git add/rm <file>..." as appropriate to mark resolution)
       #       both modified:      foo}
  You can also see that git knows about the conflict and refuses to do a
  commit:
  @pre{
    $ git commit
    fatal: 'commit' is not possible because you have unmerged files.
    Please, fix them up in the work tree, and then use 'git add/rm <file>' as
    appropriate to mark resolution and make a commit, or use 'git commit -a'.}
@~
  In most cases the way to continue is simple: open the conflicted file in your
  editor, look for the conflict markers and fix the code.  Then, as suggested
  above, use @cmd{git add @i{file}} which tells git that the file is resolved,
  and finally use @cmd{git commit} to commit the result.  (Note that using
  @cmd{git commit @i{file}} will not work, which is why the @cmd{git-ci} script
  avoids adding a @path{.} if the tree requires resolving a merge.)  I'll
  simulate the editing part with echos, and then mark it resolved:
  @pre{$ echo "#lang racket"                   > foo
       $ echo "(define (foo [x 0]) ; int->int" >> foo
       $ echo "  (* x x))"                     >> foo
       $ git add foo                 # ← tell git that it's resolved}
  And now the last step is to run @cmd{git commit}, which will start your
  editor to edit the log message — it will be populated by text that indicates
  the merge and the file that had conflicts, which you can commit as is, or add
  some text regarding the way it was resolved.
@~
  At this point (or before we started working on resolving the conflict), we
  can get back to the original state using @cmd{reset}:
  @pre{$ git reset --hard
       HEAD is now at 21a78df document the type of foo}
@~
  This kind of reset is generally useful if you had some problematic conflict
  to resolve and you want to back up completely and re-try.  But now that we've
  at the start, we will see what happens when we try to rebase with the
  conflict instead:
  @pre{
    $ git rebase origin
    First, rewinding head to replay your work on top of it...
    Applying: document the type of foo
    @i{...}
    CONFLICT (content): Merge conflict in foo
    Failed to merge in the changes.
    @i{...}
    When you have resolved this problem run "git rebase --continue".
    If you would prefer to skip this patch, instead run "git rebase --skip".
    To restore the original branch and stop rebasing run "git rebase --abort".}
@~
  Obviously, we get a different message (note that @cmd{git status} will now
  tell you that you're not currently on any branch — a result of being in the
  middle of a rebase).  The process that follows is very similar to the merge
  case: edit the conflict away, then @cmd{git add} the file.  There are two
  differences: (1) after you @cmd{git add} the resolved files, you should use
  @cmd{git rebase --continue} instead of committing[*]; (2) if you want to
  abort the merge, use @cmd{git rebase --abort} instead of using reset.
  @small{([*] If you did commit, then it means that you wrote a new log message
    for the replayed commit, and you can just as well use the @cmd{--skip} flag
    so rebasing continues with the rest, or you can use @cmd{reset} to undo
    your commit and let rebase do it for you.)}
@~
  When you're in a conflicted state, there are a few git tools that help you in
  the resolution work.  The first useful utility is @cmd{git diff}: when
  there's a conflict, all files that were automatically merged are already
  going to be in your staging area, and parts of conflicted files that could be
  merged merged will be there too.  This leaves only the conflict regions in
  your working directory, which means that @cmd{git diff} will show you only
  the conflicts (since by default it shows differences between the working
  directory and the staging area).  Also, the diff output itself is not a
  standard one.  At the current point of conflict during the rebase that we
  started, this is what we'll see:
  @pre|{$ git diff
        diff --cc foo
        index b81de80,86a4c54..0000000
        --- a/foo
        +++ b/foo
        @@@ -1,3 -1,3 +1,7 @@@
          #lang racket
        ++<<<<<<< HEAD
         +(define (foo [x 0])
        ++=======
        + (define (foo x) ; int->int
        ++>>>>>>> document the type of foo
            (* x x))}|
@~
  The diff header uses @cmd{--cc} which indicates git's “combined diff format”,
  used to represent merge commits (any commit with more than one parent).  The
  next line has the two SHA1s of the two files that are merged.  The diff
  itself starts with three @cmd|{@}|s, and instead of a single indicator
  character (@cmd{+}, @cmd{-}, or @cmd{ }), there are two — indicating a
  three-way diff between the two versions and their common ancestor version.
  In the above you can see that the line with the optional argument is coming
  from @cmd{HEAD}, and the type-annotated one is coming from its commit.  You
  might notice that this look backwards, since we're in the repository where we
  committed the type annotation to the HEAD — but we're now rebasing, which
  means that we start from the remote branch and merge our local changes into
  it, essentially making the rebase perform merges in the other way than plain
  merges.  The conflict markers themselves are marked as new in both versions,
  and the labels that follow them depend on available information (in a
  @cmd{merge}, we would see @cmd{HEAD} and @cmd{origin}).
@~
  During a conflict resolution, the staging area actually holds three versions
  of each file: the common ancestor, our version, and the merged version.
  These things are called “file stages”, and they can be accessed using a
  special syntax:
  @pre{$ git show :1:foo     # the common ancestor of both versions
       $ git show :2:foo     # our version (optional argument)
       $ git show :3:foo     # merged version (type-annotated)}
  (Again, remember that this is a rebase, so the last two are swapped.)  You
  can also checkout one of these versions using @cmd{git checkout foo}, giving
  it an @cmd{--ours} or @cmd{--theirs} flag to specify which version you want
  to use; and you can use @cmd{git diff} to compare against them.  For example,
  we resolve the file (as above) and then try the different diffs (before we
  mark it as resolved) — these examples only show the changed lines from each
  of the diffs:
  @pre{$ echo "#lang racket"                   > foo
       $ echo "(define (foo [x 0]) ; int->int" >> foo
       $ echo "  (* x x))"                     >> foo
       $ git diff -1 foo               # can also use --base
       -(define (foo x)                # original version
       +(define (foo [x 0]) ; int->int # new version
       $ git diff -2 foo               # can also use --ours
       -(define (foo [x 0])
       +(define (foo [x 0]) ; int->int
       $ git diff -3 foo               # can also use --theirs
       -(define (foo x) ; int->int
       +(define (foo [x 0]) ; int->int}
@~
  Finally, @cmd{git log} and @cmd{gitk} accept a @cmd{--merge} flag which shows
  commits relevant to a merge.  With @cmd{git log} the @cmd{--left-right} flag
  is useful here, since you'll see which side the relevant commits are on.
  (But this works only in @cmd{git merge}, not in rebasing.)
@~
  Again, when you're happy with the resolution, you @cmd{git add} the file, and
  because we're doing a @cmd{rebase} rather than a @cmd{merge}, use use it to
  continue:
  @pre{$ git add foo
       $ git rebase --continue
       Applying: document the type of foo
       $ git log --graph --all --oneline -4
       @i{...linear history...}}
  Note that @cmd{git rebase --continue} did the commit of the resolved content
  for you, and it used the previous commit message you've written.  This is a
  good rule-of-thumb for deciding whether you should rebase or merge: if the
  commit message are still fine as a description of the modifications, then a
  rebase is fine; otherwise you might want to @cmd{merge} instead.}

@subsection{Copying/renaming files}
@p*{
  Git is, by design, tracking snapshots of the complete repository tree.
  Specifically, it does @em{not} keep explicit track of file/directory copies
  and renames.  Instead, it provides ways to infer such changes in the
  repository based on the content.  As a result of this, there are almost no
  git commands that deal with file movements:
  @ul*{
  @~ There is no @cmd{git copy} command: you just copy the file and add the new
     one as usual.
  @~ There @em{is} a @cmd{git rm} command, but its purpose is mostly to remove
     a file from the staging area.  You could also just remove the file outside
     of git, and then use either @cmd{git commit @i{removed-file}} or
     @cmd{git commit @i{containing-directory}} to remove it (or using the above
     script — @cmd{git ci} in the same directory).  @cmd{git rm} will delete
     the file from the staging area so you can do a plain @cmd{git commit}
     without naming any paths.
  @~ For the same reason, there is a @cmd{git mv} command — it uses
     @cmd{git rm} as above to update the staging area, and if you're fine with
     ignoring it, then you can just rename the file outside of git, and
     @cmd{git add} the new version — but as we will soon see, it's really best
     to use @cmd{git mv} to avoid the possible confusion if you want the file's
     history to be visible.}
@~
  To try things out, let's properly name the @path{foo} library:
  @pre{$ mv foo foo.rkt
       $ git st
        D foo
       ?? foo.rkt}
  As you can see, we forgot to @cmd{git add} the new file, so if we commit now
  we'll only be committing the deletion.  An important thing to note here is
  that when git infers file copying and renaming, it does so only when the
  operations appear in a @em{single} commit.  So if we commit this change and
  later commit a new version with the new file will make it lose connection to
  its history.  As long as you didn't push the new commits out, you can still
  fix it: simply use @cmd{git rebase --interactive}, and squash the file
  addition together with the deletion.  But let's start over and do the rename
  the easy way:
  @pre{$ rm foo.rkt
       $ git reset --hard
       $ git mv foo foo.rkt
       $ git ci -m "properly name the foo library"}
  to see this commit, we can use @cmd{git show} (which can show arbitrary
  objects, but with no arguments it shows the @cmd{HEAD}).  @cmd{git diff} can
  also be used to show only the diff part — using the @cmd{HEAD^!} syntax that
  roughly means the range from the previous HEAD to the current one:
  @pre{$ git show
       @i{...log message...}
       @i{...addition+deletion...}
       $ git diff HEAD^!
       @i{...addition+deletion...}
       $ git diff --stat HEAD^!      # shows an overview of the changes
        foo     |    3 ---
        foo.rkt |    3 +++
        2 files changed, 3 insertions(+), 3 deletions(-)
       $ git log --oneline foo.rkt
       599b3b6 properly name the foo library}
  All of these show the two operations as disconnected, and the log doesn't
  show any of the prior history.  The thing is that you need to ask git to look
  for file operations, and the @cmd{-M} and @cmd{-C} flags do that.  In
  addition, @cmd{git log} needs a @cmd{--follow} flag to make it follow history
  beyond renames (but note that it can do that only when given a single file
  path).  For example:
  @pre{$ git diff -M --stat HEAD^!
        foo => foo.rkt |    0
        1 files changed, 0 insertions(+), 0 deletions(-)
       $ git log --oneline --follow foo.rkt
       599b3b6 properly name the foo library
       0fb8291 document the type of foo
       5035c9a add a default value
       @i{...}}
  In this case the rename was a trivial one as were no other changes.  This
  makes it especially easy to find renames since the SHA1 of the file would be
  the same.  But git considers such operations as renames as long as they're
  “similar enough” — for example, if you just rename some files and change some
  @cmd{require}s as a result, it will be detected as renames.  (The usual claim
  is that when the content is not similar enough, you can just as well claim
  that the file is new.)  If you think that you might be doing too many changes
  to some files, and you want to preserve the connection, you can do only the
  rename in one commit, and then the modifications in the next.
@~
  An added benefit of this mode of work is that @cmd{git blame} can find lines
  in files that were copied from other files, and deal naturally with a file
  that is split into two files etc.  Like @cmd{log} and @cmd{diff}, it needs
  some flags to do the extra work (see @cmd{-M} and @cmd{-C}).}

@subsection{Managing branches}
@p*{
  As seen in various places above, a branch in git is basically just a SHA1
  pointer to a commit (and therefore to the whole line of commits in its
  development line), with a naming hierarchy that follows some conventions
  (@path{/}-separated, @cmd{master} as the main one, @cmd{remotes} prefix for
  remote branches, @cmd{origin} for the default remote server name, etc).  You
  can see all of this in the toplevel @path{.git} meta directory — there is a
  @path{HEAD} file which represents the head, its content will be a line that
  looks like @cmd{ref: refs/heads/master}, and there will be a
  @cmd{refs/heads/master} file with a content that is the actual SHA1.  There
  are, of course, various other bits of meta-data, so it's not a good idea to
  change such files directly (for example, when there are many names git will
  create a “packed” reference file with many references for efficiency) — but
  overall this is the basic idea.
@~
  Branches come in two main kinds: local branches and remote ones, with remote
  branches having a name that begins with @cmd{remotes/origin/}.  (Later we'll
  see how to add new remote repositories — remote branches from these will have
  names that start with @cmd{remotes/@i{remote-name}/} instead.)  The
  difference between the two is that a remote branch is a way to mirror a
  branch on a remote repository — it is not intended for local work.  For
  example, if you try to check out a remote branch, git will check out a
  “detached head” (details on this below).  If you do that, you'll see that the
  @path{HEAD} file will have an explicit SHA1 rather than the usual
  @cmd{ref: @i{branch-name}}.
@~
  The @cmd{git branch} command is the main way to manage branches.  With no
  flags, it will just print out the list of local branches, marking the current
  branch with a @cmd{*}.  You can add flags to show remote branches instead
  (@cmd{-r}), both kinds (@cmd{-a}), and also to list more information on the
  branches (@cmd{-v}):
  @pre{$ git branch
       * master
       $ git branch -r
         origin/HEAD -> origin/master # (this one is symbolic too)
         origin/master
       $ git branch -av
       * master                599b3b6 [ahead 2] properly name the foo library
         remotes/origin/HEAD   -> origin/master
         remotes/origin/master 5035c9a add a default value}
@~
  When given a single name argument, a branch by that name will be created, and
  it will point to where the @cmd{HEAD} currently points to; a second argument
  can be a name of an existing branch (or any commit) that the new branch will
  start at.  In addition to creating branches starting from the current head,
  this can be useful in creating branches that start from elsewhere, even from
  a “detached head”.  For example, say that in our current repository we want
  to try out some work based on the state of things before the last commit
  (which renamed the @path{foo} file).  We can check out @cmd{HEAD^} (which
  will lead to a detached HEAD), and then create a branch for it:
  @pre{$ git checkout HEAD^
       @i{...}
       You are in 'detached HEAD' state.
       @i{...}
       HEAD is now at 0fb8291... document the type of foo
       $ cat .git/HEAD
       0fb8291...                     # doesn't point to a branch
       $ git branch
       * (no branch)                  # you can see it here too
         master
       $ git status
       # Not currently on any branch. # and here
       $ git branch pre-rename        # create a branch here
       $ git branch
       * (no branch)                  # we're still detached
         master
         pre-rename
       $ git checkout pre-rename
       Switched to branch 'pre-rename'}
  As you can see, creating a branch doesn't check it out — even when the new
  branch is exactly where we already are.  The difference is related to the
  nature of @cmd{HEAD}: it is usually an indirect reference to a branch name,
  and when a commit is made, the branch that @cmd{HEAD} points to is updated.
  But when we are using a detached HEAD, it points directly at a SHA1 —
  committing in this state will work, and the HEAD will point at the newly made
  commit — but there will be no branch that will be updated, so if you checkout
  a different branch (or a different commit) now, the commits you made are
  “lost”.
@~
  The main reason that such commits will be lost is that git branches don't
  live inside the repository store — and dealing with branches is not something
  that gets recorded as part of the history.  To make things safer, git
  maintains something that is known as the “reflog”, which keeps track of where
  your branches have been — those are kept for a while (usually around a
  month), which means that you can easily go back to a previous commit if it
  seems that you lost one (eg, as a result of committing on a detached HEAD).
  (You can see these files in the @path{.git/logs} directory.)
@~
  Since creating a new branch and checking it out is a common combination, the
  @cmd{checkout} command can create a branch before checking it out.  Use the
  @cmd{-b} flag for this:
  @pre{$ git checkout -b also-pre-rename
       Switched to a new branch 'also-pre-rename'
       $ git checkout -b post-rename master
       Switched to a new branch 'post-rename'
       $ ls
       bar  blah  foo.rkt
       $ echo "one more line" >> foo.rkt
       $ git ci -m "one more line"}
@~
  Finally, you use the @cmd{-d} flag to delete branches.
  @pre{
    $ git branch -d post-rename   # won't allow it
    error: Cannot delete the branch 'post-rename' which you are currently on.
    $ git checkout master
    Switched to branch 'master'
    Your branch is ahead of 'origin/master' by 2 commits.
    $ git branch -d post-rename
    error: The branch 'post-rename' is not fully merged.
    If you are sure you want to delete it, run 'git branch -D post-rename'.}
  As you can see, git refuses to delete a branch that has unmerged work, since
  this can lead to losing that unmerged work — so you need to use @cmd{-D} for
  that.  In addition, you usually don't delete remote branches, when you do,
  you need to use the @cmd{-r} flag too.}

@subsection{Using branches}
@p*{
  Since git branches are so light weight, they fit any kind of parallel work
  you need to do on several different topics.  A result of that is that it is
  possible to start a new branches for any work you'd want to do — and this is
  common enough that there's a name for such branches, they're called “topic
  branches”.  Such branches are created from the master branch (usually) and
  worked on in parallel.  At any point where you want to work on something new,
  you would create a new branch for it and switch to it (committing any work
  you might have on your current branch before you do so):
  @pre{$ git checkout -b improve-bar master # switch to a fresh topic branch
       Switched to a new branch 'improve-bar'
       $ echo "even more bar" >> bar        # work there
       $ git ci -m "improved bar"           # save that work
       $ git checkout post-rename           # go back to where we were}
@~
  If you need to commit changes before you create the new branch, you shouldn't
  have any problems doing so — because you can change where a branch points to,
  you can just commit whatever you have and then get back to it:
  @pre{
    $ echo "another line" >> foo.rkt
    # at this point you remember that you need to do something else in the
    # `improve-bar' line of work.
    $ git ci -m "checkpoint"
    $ git checkout improve-bar
    # ...work...
    $ git checkout post-rename
    $ git log --oneline -2
    e9a4fcd checkpoint            # this is our temporary checkpoint commit
    d92fb0a one more line
    $ git reset HEAD^             # undo it
    Unstaged changes after reset:
    M       foo.rkt               # git tells us that this is now uncommitted
    $ git st
     M foo.rkt                    # ... as does `status'
    $ git log --oneline -2
    d92fb0a one more line         # the temporary commit is gone
    599b3b6 properly name the foo library}
@~
  You can even decide on some convention to use in some cases, then create new
  git commands as scripts that will do the work for you.  In this case, you
  could write a command that will do a “checkpoint” commit if needed, switch to
  another branch, and if the first commit there has only @cmd{checkpoint} as
  its log message, undo it as above.  There are several git convenience
  commands that started out this way — in this case, checkout the @cmd{git
  stash} command which allows you to save the current work by pushing it on a
  “work in progress” stack, and later pop it back out (possibly on a different
  branch).
@~
  Earlier we've seen how to merge or rebase your master branch from the remote
  master branch, but the full story is that you can merge and rebase @em{any}
  two branches.  This makes branches very flexible: you can create a branch A
  from an existing branch B, eventually merging/rebasing it back into A, or
  directly into master and dump A.  At any point you can run @cmd{gitk --all}
  to see where things stand — in our current repository, this shows us that
  there are redundant @cmd{pre-rename} and @cmd{also-pre-rename} branches, that
  out @cmd{master} branch is two commits ahead of the remote one, and that we
  have @cmd{improve-bar} and @cmd{post-rename} branches with 1 and 2 commits
  over our @cmd{master} branch.  If we're done with these two branches, we can
  now merge/rebase them to our @cmd{master}, or merge/rebase one to the other
  and the result to @cmd{master}, and then push everything out.
@~
  To make working with branches even easier, git has a notion of an “upstream
  branch” — this is a per-branch setting that tells git which branch the
  current one is based on.  By default, any branch that is created with a
  remote branch as its starting point will have that remote branch set as its
  upstream.  We've seen how git treats that information in various places so
  far: @cmd{git status} and @cmd{git branch -v} both use it, and using a second
  @cmd{-v} with the latter shows also the upstream branch:
  @pre{
    $ git reset --hard            # dump the above uncommitted change
    $ git checkout master
    Switched to branch 'master'
    Your branch is ahead of 'origin/master' by 2 commits.
    $ git status
    # On branch master
    # Your branch is ahead of 'origin/master' by 2 commits.
    @i{...}
    $ git branch -v
    @i{...}
    * master    599b3b6 [ahead 2] properly name the foo library
    @i{...}
    $ git branch -vv
    * master    599b3b6 [origin/master: ahead 2] properly name the foo library}
@~
  In addition to that, we've seen the @cmd|{@{upstream}}| and @cmd|{@{u}}|
  notation that refers to the upstream branch, making it convenient to further
  examine pending changes that weren't incorporated upstream:
  @pre|{$ git log --oneline @{upstream}..
        599b3b6 properly name the foo library
        0fb8291 document the type of foo}|
@~
  And finally, @cmd{git pull} and @cmd{git push} know where to pull from and
  push to based on this setting.  Overall, this is a very useful feature to
  have when you have many branches, therefore it is possible to use it between
  local branches too.  There are two ways to do this: when a branch is created
  with either @cmd{git branch B} or @cmd{git checkout -b B}, you can use the
  @cmd{--track} flag to set up tracking to the initial branch it's based on.
  @pre{$ git branch -t b1 master
       Branch b1 set up to track local branch master.
       $ git checkout -tb b2 master
       Branch b2 set up to track local branch master.
       Switched to a new branch 'b2'}
  (Note: if you're using @cmd{checkout}, then the @cmd{--track} flag should
  precede the @cmd{-b} flag, as done above.)  If a branch already exists, you
  can use @cmd{git branch --set-upstream} to set the upstream information.
  @pre{$ git branch --set-upstream post-rename
       Branch post-rename set up to track local branch b2.
       $ git branch --set-upstream improve-bar master
       Branch improve-bar set up to track local branch master.}
  As seen here, if it is given just a branch name, the current branch is set as
  its upstream.  @cmd{git branch} can also change the upstream branch, for
  example, if the above tracking of @cmd{b2} was a mistake:
  @pre{$ git branch --set-upstream post-rename master
       Branch post-rename set up to track local branch master.}
  Either way, we can now see this information in the git commands that do so,
  as well as use @cmd|{@{upstream}}|:
  @pre|{$ git branch -vv
          b1          599b3b6 [master] properly name the foo library
        * b2          599b3b6 [master] properly name the foo library
          improve-bar e60c168 [master: ahead 1] improved bar
          master      599b3b6 [origin/master: ahead 2] properly name the foo @;
                                                       library
          post-rename d92fb0a [master: ahead 1] one more line
        $ git checkout improve-bar
        Switched to branch 'improve-bar'
        Your branch is ahead of 'master' by 1 commit.
        $ git log --oneline @{upstream}..
        e60c168 improved bar}|
@~
  In addition, we can use @cmd{git pull} to get changes on the upstream branch
  merged or rebased on the current one:
  @pre{$ git pull
       From .
        * branch            master     -> FETCH_HEAD
       Already up-to-date.}
  Nothing actually happened here, because the current branch
  (@cmd{improve-bar}) already contains all of the commits on the master branch.
  You can see that this is a local pull since git says @cmd{From .}, which
  stands for “our own repository”.  You can also do a @cmd{push} now, which
  will make the current additional commit (listed with @cmd|{@{upstream}..}|)
  appear on the @cmd{master} branch:
  @pre{$ git push
       To .
          599b3b6..e60c168  improve-bar -> master}
@~
  Since the @cmd{improve-bar} line of development is unrelated to the one in
  @cmd{post-rename}, it is now one commit behind the @cmd{master} branch, and
  cannot be pushed as is:
  @pre{
    $ git checkout post-rename
    Switched to branch 'post-rename'
    Your branch and 'master' have diverged,
    and have 1 and 1 different commit(s) each, respectively.
    $ git branch -vv
      improve-bar     e60c168 [master] improved bar
      master          e60c168 [origin/master: ahead 3] improved bar
    * post-rename     d92fb0a [master: ahead 1, behind 1] one more line
    $ git push
    To .
     ! [rejected]        post-rename -> master (non-fast-forward)
    error: failed to push some refs to '.'
    To prevent you from losing history, non-fast-forward updates were rejected
    @i{...}}
  Dealing with this is similar to dealing with updates on the remote server —
  for example, we can rebase the branch before pushing it:
  @pre{$ git pull --rebase
       From .
        * branch            master     -> FETCH_HEAD
       First, rewinding head to replay your work on top of it...
       Applying: one more line
       $ git push
       To .
          e60c168..7bdec0c  post-rename -> master}
@~
  When you use @cmd{git push} to push changes when you have no upstream branch
  set, or when you push to a different branch than the one set, you can use
  @cmd{--set-upstream} to make git remember the push target as the upstream.
  Therefore, an easy way to create a new branch that tracks a possibly new
  remote branch by the same name is:
  @pre{$ git checkout -b my-branch
       Switched to a new branch 'my-branch'
       $ git push origin my-branch --set-upstream
       To pltgit:eli/foo
        * [new branch]      my-branch -> my-branch
       Branch my-branch set up to track remote branch my-branch from origin.}
  And when you deal with remote branches this way, you might want to have a
  local branch that tracks a remote one with a different name.  To do this, you
  use a syntax for the branch to push that specifies the local branch to push
  and the remote one to push to:
  @pre{$ git push origin my-branch:different-branch --set-upstream
       To pltgit:eli/foo
        * [new branch]      my-branch -> different-branch
       Branch my-branch set up to track remote branch different-branch from @;
       origin.}
@~
  Finally, note that git stores the upstream information in the
  repository-local configuration file.  If we look at it now, we will see the
  various upstreams that we have set:
  @pre{$ cat .git/config
       @i{...}
       [remote "origin"]
               fetch = +refs/heads/*:refs/remotes/origin/*
               url = pltgit:eli/foo
       [branch "master"]
               remote = origin
               merge = refs/heads/master
       @i{...}}
  this is the upstream that was made by default when we first checked out our
  clone, together with the information of where the @cmd{origin} repository is.
  Following that are the ones we've setup later:
  @pre{@i{...}
       [branch "b1"]
               remote = .
               merge = refs/heads/master
       [branch "b2"]
               remote = .
               merge = refs/heads/master
       [branch "post-rename"]
               remote = .
               merge = refs/heads/master
       [branch "improve-bar"]
               remote = .
               merge = refs/heads/master
       [branch "my-branch"]
               remote = origin
               merge = refs/heads/different-branch}
@~
  Note that there are branches that track local branches (ones with a
  @cmd{remote = .} setting), and ones that track remote ones; and also note
  that the @cmd{my-branch} branch tracks a remote branch with a different name.
  Since the settings are stored as configurations, it is possible to inspect
  and change them using @cmd{git config}, or even edit the config file
  directly.
  @pre{$ git config branch.my-branch.remote
       origin
       $ git config branch.my-branch.merge
       refs/heads/different-branch}}

@subsection{Managing remotes}
@p*{
  The distributed nature of git means that you can interact with multiple
  remote repositories.  You could have work done with other people done
  locally, where people push/pull from each other's clones (possibly by sending
  around patches, as described below), and eventually when the changes are
  ready push them back to the main repository.  You can even have your
  repository track multiple unrelated remote repositories, essentially giving
  you branches that have @em{unrelated} histories.
@~
  By default, when you clone a remote repository git names it @cmd{origin} —
  and that name appears in many places, most notably in remote branch names.
  As seen in the above config, git remember where the origin repository is via
  a configuration:
  @pre{$ git config remote.origin.url
       pltgit:eli/foo
       $ git config remote.origin.fetch
       +refs/heads/*:refs/remotes/origin/*}
  The first one is the url of the remote repository, and the second one is
  which branches we want to get from it.  As with branches, you can use
  @cmd{git config} to change this information, or you can edit the file
  directly, but there is a command that does this more conveniently, keeping
  things consistent:
  @pre{$ git remote                  # lists all known remotes
       origin
       $ git remote -v               # remotes and push/pull specs
       origin  pltgit:eli/foo (fetch)
       origin  pltgit:eli/foo (push)
       $ git remote show origin      # see a detailed description
       * remote origin
         Fetch URL: pltgit:eli/foo
         Push  URL: pltgit:eli/foo
         HEAD branch: master
         Remote branches:
           different-branch tracked
           master           tracked
           my-branch        tracked
         Local branches configured for 'git pull':
           master    merges with remote master
           my-branch merges with remote different-branch
         Local refs configured for 'git push':
           master    pushes to master    (fast-forwardable)
           my-branch pushes to my-branch (up to date)}
@~
  The @cmd{git remote show} variant will actually query the remote repository
  for its state (using @cmd{git ls-remote}) by default, and tell you when a
  local branch that tracks a remote one is out-of-date.
@~
  There are a few more sub-verbs for the @cmd{git remote} command which you can
  see on the @man{git-remote} man page, the most important one is for adding a
  remote: @cmd{git remote add @i{short-name} @i{url}}.  This is especially
  convenient if you want to have a fork of the @cmd{plt} repository, with most
  interaction happening against it, but occasionally pull/push updates from/to
  the main repository.
@~
  Of course, remember that you don't need to add remotes to push and pull from
  them.  You could do the same by explicitly specifying a url for the
  repository you want to interact with.  For example, you could have
  repositories in different accounts on different machines, and synchronize
  your work between them by pushing and pulling directly from one repository to
  another.  (Reminder: if you do this, then you're likely to have “checkpoint
  commits” — when you're done with the work, you can do an interactive rebase,
  and squash these checkpoint changes back into logical commit.)  But if you do
  this often enough, you will likely find it more convenient to add a named
  remote.}

@subsection{Using private repositories}
@p*{
  A particularly useful use-case for adding a new remote is when you want to
  have private work done in your own fork of the @cmd{plt} repository.  Such a
  mode of work is not strictly necessary — you could just do your work in your
  repository in a long-lived branch, but there are certain cases where working
  with a private repository on the server might be more convenient.  For
  example, you might want to collaborate with someone else (that has access)
  via the server, or you might use a private fork of the @cmd{plt} repository
  as a central point for synchronizing work from clones on different
  filesystems as described at the end of the last section (the difference from
  that is that you basically use the PLT server as your synchronization point).
  Other than having the main repository reside on the PLT server, working with
  a private repository is not different than working with any other repository.
@~
  There are two facts that are worth reminding when you deal with a private
  repository.  First, remember that creating a private fork is cheap: creating
  a new git clone of a repository will use hard links to the repository store
  object, most of which will be contained in large packed files.  The cost in
  terms of space and time for creating a new clone is therefore minimal when
  done on the same filesystem — and using the gitolite @cmd{fork} command is
  doing just that.  Please use the @cmd{fork} command to create a private clone
  — gitolite has a feature where it creates any repository that you refer to
  (as long as it has a name that you're allowed to create — starts with your
  username); this means that you could clone the main @cmd{plt} repository and
  push from it into a private repository that doesn't exist: it will be
  created, but such a copy will not share storage with the main repository — it
  will require a new copy, and it will be slow to create.
@~
  The second thing to remember is that due to the nature of the git store, any
  object, including commits, is stored exactly once.  Since commits contain
  their parents, having a specific commit means that you have its complete
  history — therefore, pulling in any branch from any repository will always
  require getting only commits that you don't already have.  As a result,
  pulling and pushing to/from any repository will be efficient and move around
  only those commits that are missing on the other side.
@~
  You can choose one of two basic approaches to working with a private fork:
  you can have the public repository cloned but have branches pushed to your
  private one, or you can have your private fork cloned and occasionally push
  updates to the public one.  A way to use these two approaches are described
  and explained now.  These examples use the @cmd{play} repository as an
  example (which you are encouraged to experiment with).  Note that you can use
  a hybrid approach: you can think about a repository as a container for commit
  histories, pushing and pulling from any other repository, including the copy
  you're working with, the main @cmd{plt} repository, or a private fork (your
  own or another).  Note also that since forks are cheap, you can keep several
  of them around, for example, you can have a fork for each long-lived branch —
  it's up to you to settle on a layout that is convenient for your work.}
@h3{Using a clone of the public repository, pushing branches to your private
    one:}
@ol*{
@~ Setting up:
   @ul*{@~ Create a fork:
           @pre{ssh pltgit fork play $user/play}
        @~ Get a local copy of the main repository:
           @pre{git clone pltgit:play}
           (or continue working in an existing one)
        @~ Set up a convenient name for your private repository:
           @pre{cd play
                git remote add my-fork pltgit:$user/play}}
@~ To start working on a private branch, create one, and push it to your
   private repository:
   @pre{git checkout -b my-branch}
   Then use @cmd{push} to create this branch in your private repository, with
   @cmd{--set-upstream} so git will remember this setting:
   @pre{git push --set-upstream my-fork my-branch}
   You can also have your branch named differently in your fork, for example:
   @pre{git push --set-upstream my-fork my-branch:master}
   will save your branch as the @cmd{master} branch in your fork.  This might
   be convenient if you want to clone your private repository elsewhere and
   work only on this branch.
@~ You can now work as usual in your repository, pushing/pulling changes
   to/from the master branch will go to the public repository, and doing so
   from @cmd{my-branch} will go to your private fork.  You can merge changes on
   the @cmd{master} branch to your private one, or rebase your branch onto it.
   However, note that the server will not allow pushing a rebased history to
   your clone.  (More details at the end of this section.)  You can bypass that
   by pushing to a new branch while keeping your local branch name:
   @pre{git push --set-upstream my-fork my-branch:my-branch-2}
@~ When you're done merge your branch (possibly rebasing it first) to the
   master branch, and push as usual.
@~ If you want to delete branches on your fork (either because you pushed a
   rebased version under a new name, or because you're done with that line of
   work), use
   @pre{git push my-fork :my-branch}
   Using an empty branch name for the local branch that you push is the way to
   delete remote branches.  (As with local branches, this might lead to losing
   commits, so be careful.  As always, git has a few safety mechanisms in
   place, so even if did this by mistake, it is very likely recoverable.)}
@h3{Using a clone of your private repository, pushing changes to the public
    one:}
@ol*{
@~ Setting up:
   @ul*{@~ Create a fork:
           @pre{ssh pltgit fork play $user/play}
        @~ Get a local copy:
           @pre{git clone pltgit:$user/play}
        @~ Set up a convenient name for the main repository:
           @pre{cd play
                git remote add -t master main pltgit:play}
           (@cmd{-t master} tells git to have only the @cmd{master} branch
           retrieved.)}
@~ Now you can work in this repository as usual — edit, commit, push, etc.
@~ To push changes to the main repository, first make sure that you're on the
   branch with the changes that you want to push, and then:
   @ul*{@~ Get the recent tree from the main repository
           @pre{git fetch main}
        @~ Rebase or merge your changes with this:
           @pre{git rebase main/master}
           or
           @pre{git merge main/master}
        @~ Push the changes back:
           @pre{git push main}}
   Note that rebasing your branch on top of main/master means that it will be
   rewritten, which means that you will not be able to push your branch back to
   your clone.  This is because rewritten histories are currently forbidden by
   the configuration, but this will probably change in the future.  Still, even
   if the server would allow pushing a rebased history it (you will need to use
   @cmd{-f} to force such a push), you would need to deal with the rebased
   branch in other clones you might have.  Because of this, a rebase is fine if
   you're done with the work that you're pushing, otherwise, a merge is more
   convenient.}

@section{Collaborating with others}
@p*{
  Git makes it very easy to collaborate with anyone, anywhere.  You should
  think about repositories as being parts a network which can be synced in any
  topology that is convenient for you.  In the case of the PLT repository, the
  main repository on the git server is the central point where the official
  repository lives, and people who can push are directly syncing content into
  it.  People who cannot push directly do so through someone who can, by
  sending out patches or “pull requests”.  The same applies for any repository,
  of course, including private repositories, even ones that you maintain
  yourself independently of the PLT server.
@~
  In the case of a patch-based workflow, the two sides that are involved are
  the patch author, and the receiver that integrates it into his/her own clone
  (and from there it goes to the main repository as usual).  The work that each
  side does is described in the next two subsections.
@~
  Following that there is a description for making your repository public,
  which you will need if you're working on a private repository, but it is also
  useful for your collaborator to do so you can use a pull-request workflow.
  In this mode there is no need to email patches; instead, both people make
  their repositories readable to each other, and when some work is ready on one
  person's repository, the other pulls the commits.  This is described in the
  last subsection.}

@subsection{Patch-based workflow@br
            — instructions for the patch sender side}
@h4{Executive summary:}
@ol*{@~ Work in a @cmd{plt} repository clone (possibly in your own branch)
     @~ @npre{$ git send-email origin/master}
     @~ You're done — thanks!
     @~ When the patch is applied, you will get the changes through
        @cmd{origin/master}, so if you worked on your master branch, make sure
        to use @cmd{git pull --rebase} which will notice that your changes were
        applied; if you worked on a branch, then you can now delete it (the
        commit objects will be different from the ones you've made).}
@h4{Longer version:}
@ol*{
@~ Work & commit as usual.  In general, it is a good idea to use
   @cmd{Signed-off-by: @i{Your Name} <@i|{your@email}|>} in commit messages,
   which is a conventional way to declare that you agree for your work to be
   released as part of the PLT project, under the terms of the LGPL.
   @cmd{git commit} will add that for you if you use the @cmd{-s} flag.  You
   can also make git do this later, when you send the patches out.
@~ Make sure that you're working with a relatively recent clone, and that
   you're on the branch where you did your work.  In most cases, this would be
   the master branch, but you can do your work in your own branch too, of
   course.
@~ @p*{
     Verify that your commits are all in your history.  You can see the commits
     that you have over the @cmd{plt} history with
     @pre{git log --oneline origin/master..}
     these are the commits that you're going to send over now.  (You can use
     the usual git toolset to tweak them further, or specify only some commits,
     etc.)
   @~
     A relevant point to consider here is that git takes the first paragraph of
     each commit message as a subject line.  When sending out a patch, this is
     made concrete by actually using it as the emails's subject — so it is a
     good idea to make sure that this log looks fine, since the @cmd{--oneline}
     option will make it show those subjects.
   @~
     Obviously, you should also make sure that the commits have clear
     descriptions of your work.  People who in the core group often have some
     general context that they are aware of, so some commit messages can be
     cryptic or even worse (eg, you might find @cmd{.} as a commit message) —
     don't mimic this...  As a more occasional contributor, you should explain
     your work in more details.  (There's no policy on commit messages, but you
     do need to go through some person on the team.)}
@~ @p*{
     At this point you should decide how to send your patches.  Emailing them
     is be the most convenient way to do this — to do this, you would use the
     @cmd{send-email} command:
     @pre{git send-email origin/master..}
     or if you send only some commits, use a different specification.  To make
     things even easier, a single commit specification is considered as the
     starting point and all of the following commits (up to your branch's tip)
     will be included in the emails (in contrast to other git commands like
     @cmd{log}, where a single commit name is considered as the set of commits
     leading up to it) — so you can do this:
     @pre{git send-email origin/master}
     This will ask you a bunch of questions — it's easy to answer but you can
     also specify them as command-line options.  if you intend to do this
     frequently it might be a good idea to make it easier with some settings in
     your global .gitconfig file.  For example, I have these settings:
     @pre|{[sendemail]
             from = Eli Barzilay <eli@barzilay.org>
             bcc = eli@barzilay.org
             suppresscc = self}|
     and you can see more in the @man{git-config} and @man{git-send-email} man
     pages.  The address to send the patches to is also configurable — you can
     use something like
     @pre{to = dev@at-racket}
     or
     @pre{to = someone@at-racket}
     depending on who you send your patches to — but this is better done as a
     repository-local configuration option (or just use the @cmd{--to} flag).
   @~
     You can add a @cmd{-s} flag to the command, to make git add
     @cmd{Signed-off-by} lines to commit messages.  (See above for what this
     means.)
   @~
     If you want to send the files in some other way (eg, send them all
     packaged in an archive as attachment[*]), then just use @cmd{format-patch}
     instead of @cmd{send-email} — git will create a number of patch files in
     your current directory, which will be named @path{NNNN-text.patch} where
     the text is made out of the subject lines of the commit messages (the
     first line).  You can even run
     @pre{git format-patch origin/master --stdout > my-patch}
     to concatenate them all and send the resulting file over.
   @~
     @small{[*] Note that doing this means that it is not as easy to read your
       patch, so avoid doing this if you want to make it easier to read and
       accept it.  On the other hand, if you're working with someone specific,
       they might prefer attachments (for example, it's easier to save the
       attached file from gmail).}}
@~ Once the commits have been pushed to the main repository, you would get
   them when you pull to update.  The commits will now be different objects
   than the ones you have — since the information changed (at least the
   committer information will be different, the log message might have been
   edited, etc).  If you made your commits on your master branch which is set
   to track the @cmd{plt} master branch (the usual setup), then make sure that
   you run @cmd{git pull --rebase} to update — this will identify the commits
   as already included and will not include them in the rebased master.  But if
   you made your commits in a private branch, and assuming that you didn't do
   any additional work there, then you can now just delete that branch.  (If
   you did do more work there, then you should rebase it, to avoid resending
   the same patches again.)}

@subsection{Patch-based workflow@br
            — instructions for the patch receiver side}
@p*{
  Accepting patches that were sent via email (on any other way), is also
  simple.  The command to do this is @cmd{git am}, which expects an argument
  that is a mailbox file holding the patch emails, or you could run it and pipe
  a patch email into it.}
@h4{Executive summary:}
@ol*{@~ @npre{$ git checkout master}
     @~ Save (unmodified) patch emails into a mail folder file.
     @~ @npre{$ git am -3 @i{the-mail-folder}}
     @~ Push the changes up to the server}
@h4{Longer version:}
@ol*{
@~ While you will not be author of the commits, you will be their committed, so
   you should of course be aware of the changes, and be willing to maintain the
   new code and other work that is implied.  So the first step that you should
   do is review the patch and make sure that you are willing to accept
   responsibility for it.
@~ Save the patch emails to a mail folder (usually a file).  You must take care
   to save the emails @em{as is}, including the date, author, and subject
   headers, and avoiding text that could have been butchered by your email
   client.  For example, if you're using gmail, then use the “show original”
   option to view the raw email text, and save that to a file (even in this
   format gmail will have a first line with a bunch of spaces — it's best to
   remove that).  Otherwise, gmail does things like wrap lines, replaces spaces
   by non-breaking spaces, or remove spaces.  Alternatively, extract patch
   files from an archive if that's what you received, or save a single
   attachment file etc.
@~ In your repository clone, make sure that you're on the branch that you want
   to integrate the changes into.  You could do this in your master branch, or
   in a new topic branch (especially if there is more than one patch).
@~ Run @cmd{git am -3 mail-folder} (@cmd{am} stands for “apply-mail”) with the
   mail folder that you've created above.  It will apply the patches and commit
   them one by one.  Like @cmd{git rebase}, if there are conflicts the process
   will stop so you can resolve it — and then run the @cmd{am} command with
   @cmd{--continue}, or @cmd{--skip} this commit and continue with the rest, or
   @cmd{--abort} to go back to the start.  The @cmd{-3} flags tells git that if
   a conflicting patch comes from the above @cmd{format-patch}, and it
   specifies files that we have, then try a 3-way merge — this will make things
   generally better (and it can identify more patches that were applied,
   instead of showing them as conflicts).
   @br
   You can also use an @cmd{-i} flag to the command to get an interactive
   version — for each commit it will ask you what to do with it, and let you
   edit the log message.
@~ Finally push the commits as usual.}

@subsection{Making a private repository publicly available}
@p*{
  If you're working with “outside people” (people with no accounts on the PLT
  server, and no direct file-system access etc) on a private repository, you
  will need to find some way to make your repository available for cloning.  An
  easy way to do so is to put it on a filesystem that those people can access —
  eg, if you're all in the same department.  Another easy way to make a
  repository available is to find a hosting service like github and others —
  there are many options here, some are free but limited, and some cost money;
  if you prefer this easy solution, keep in mind that you can pay for the
  duration of the collaboration and at the end you can simply keep your
  repository clone to yourself (eg, if you're working on a paper then there's
  no need to pay once all work is done).
@~
  But if you want to do it yourself, the quickest and most convenient way to
  make a repository public is to put it in a directory that is available on the
  web.  Such repositories can be cloned directly from the URL the repository is
  available at — there's no need to setup a server in a special way, and no
  need to run cgi scripts.}
@h4{Executive summary:}
@ol*{@~ @npre{$ git clone --bare @i{your-repo} ~/public_html/@i{repo}.git}
     @~ @npre{$ cd ~/public_html/@i{repo}.git/hooks;
              mv post-update.sample post-update;
              chmod +x post-update}
     @~ @npre{$ git remote add public ~/public_html/@i{repo}.git}
     @~ Tell people to clone from @cmd{http://some.where/~you/@i{repo}.git}
     @~ Work, apply email patches, and: @cmd{git push public}}
@h4{Longer version:}
@ol*{
@~ Make a “bare” repository — this is a repository that has no working
   directory:
   @pre{git clone --bare @i{your-repo} @i{repo}.git}
   This will create a @path{@i{repo}.git} directory holding the bare
   repository.  You should use some path in a directory where you have web
   pages published.
@~ The URL where the directory is found at is what other people should use when
   cloning.
@~ You can now push to this repository, and other people will see it too.  To
   make things easier, you can set a remote name for this repository, so it's
   easy for you to push changes to it.
   @pre{git remote add public ~/public_html/@i{repo}.git}
   And now you can use @cmd{git push public}.  (You can also pull from it, but
   since you're going to be the only one who pushes into it, that will not be
   necessary.)
@~ One thing to be aware of is that while a repository can be published through
   HTTP this way, git considers that a “dumb protocol” (because there is no
   proper interaction between the two sides).  To still make cloning possible,
   you will need to maintain some meta-files that hold entry points to the
   objects in your repository — to get this, run:
   @pre{git update-server-info}
   You need to run this after each update to the repository — and to automate
   this you can have a hook do it for you.  In the bare repository you will
   find a @path{hooks} directory with a file called @path{post-update.sample} —
   simply rename this file to @path{post-update}, and make it executable with
   @cmd{chmod +x post-update}.  From now on every push to this repository will
   run the hook and keep the meta files updated.}

@subsection{Pull-request workflow}
@p*{
  A possibly easier way for people to contribute work is to make their
  repositories available somehow.  In the case of a private repository, the two
  sides can be in a shared file system, with read permissions for each other;
  or achieved as described in the previous subsection.  In the case of
  contributing to the @cmd{plt} repository, the contributor can maintain a
  public fork of the @cmd{plt} repository (eg, by forking the @cmd{plt} github
  mirror at @selflink{https://github.com/plt/racket} directly on github).
@~
  In this workflow there is no need to mail patches — instead, the receiver
  simply pulls them directly from the sender's repository.  For example,
  someone tells you that they have some new commits in a @cmd{foo} branch of
  their repository.  Since this is a repository that you can access, and since
  it shares history with yours, you can just pull that branch in, for example:
  @pre{git checkout -b someones-work
       git pull @i{someones-repository-url}}
  Note that the @cmd{pull} will merge the changes, creating a merge
  commit if your @cmd{master} branch cannot be fast-forwarded.  To avoid
  this, you can use @cmd{fetch} instead:
  @pre{git checkout -b someones-work
       git fetch @i{someones-repository-url}}
  Either way, this fetches the remote repository's HEAD.  You can create the
  branch in a single fetch command by specifying the remote branch name, and
  the local branch to fetch into, for example:
  @pre{git fetch @i{someones-repository-url} master:someone}
@~
  If you expect to do this often (eg, you're going to suggest fixes for the
  work and get new work in), then you can add a @cmd{someone} remote to be used
  more conveniently:
  @pre{git remote add someone @i{someones-repository-url}
       git fetch someone
       git checkout -b some-branch someone/some-branch}
  possibly using -t to make the branch track the remote one:
  @pre{git checkout -tb some-branch someone/some-branch}
  Note that there is no need to create a branch before the @cmd{fetch}, since
  it will be fetched to a @cmd{remotes/someone/master} branch.
@~
  Once you pulled in the branch, you can inspect the changes, merge them,
  rebase them, etc.  The important point here is that you have a copy of the
  contributed line of work, which you can use with the usual git toolset.
@~
  When/if you're happy with the changes, you can simply integrate them to your
  master branch, and if this is in a clone of the @cmd{plt} repository, then at
  this point you can simply push these commits to the main server.  Once that
  happens, the contributor can update their own clone, and continue working as
  usual.
@~
  Git has a tool that makes this mode of work a little more organized and
  robust for the contributor: @cmd{git request-pull}.  This simple command
  (surprisingly, it has no flags) expects a commit that marks the start of the
  new work (actually, the last one before it, eg, @cmd{origin/master}), and the
  url of the repository.  For example:
  @pre{git request-pull origin git://github.com/someone/somefork.git}
@~
  Of course, the contributor doesn't have to work directly in the available
  repository — in the case of github or with an over-the-web setup like the one
  described in the previous subsection the public repository is a bare one, and
  no work can be done directly on it.  So what actually happens is: the
  contributor works on his/her own repository, pushes changes to the public
  one, and then requests a pull.
@~
  The @cmd{request-pull} command will therefore check that the new commits are
  indeed available at that location, and find out the branch that they're on
  (in case it's different than the branch that someone is working on).  It then
  prints out a “pull request” text with a description of the changes, the url
  that was specified, the branch name with the new work, and a summary of the
  files that were changed.  In short, all the relevant information is there,
  and it even verified that the commits are indeed available — merging them in
  is now easy.
@~
  (As a sidenote, you can use @cmd{.} as the url:
  @cmd{git request-pull origin .}, and get a condensed summary of your
  changes.)}

@subsection{Pull-request workflow@br
            — recipe for the sender side}
@ol*{@~ Clone the @cmd{plt} repository and work with it as usual, commit your
        work
     @~ Make your repository publicly available
     @~ @npre{$ git request-pull origin @i{your-repository-url}}
     @~ Send the resulting text to @cmd{dev@at-racket}
     @~ You're done — thanks!}
@p{Alternatively, you can fork the @cmd{plt} repository on github:
   @cmd{https://github.com/plt/racket}, commit, then do a pull request.  Note:
   it is better to send a note about your pull request to @cmd{dev@at-racket},
   or you can do the pull request directly with git as listed above (using
   github to have a public repository).}

@subsection{Pull-request workflow@br
            — recipe for the receiver side}
@p{This recipe is for getting some remote work in as a one-time job.  If you
   need to cooperate more closely with someone, you will want to add the remote
   repository with @cmd{git remote} as shown above.}
@ol*{
@~ Get a @cmd{plt} clone, or use your own (it's safe to do the latter, no need
   for a new clone unless you're paranoid):
   @pre{git clone pltgit:plt
        cd plt}
@~ Get the foreign repository's master branch (or any other branch) into a
   local branch:
   @pre{git fetch @i{remote-repository-url} master:foo}
   This pulls the @cmd{master} branch of the remote repository into a local
   @cmd{foo} branch (you can use other names, of course).
@~ Inspect the changes as usual
   @pre{git log master..foo    # new commits
        git diff master...foo  # changes
        git log -p master..foo # both}
   (See above for more details on these.)
@~ If you're happy with the change and want to get it as-is, you can simply
   @cmd{merge} the branch:
   @pre{git merge foo}
   But unless the remote work was done from the point your @cmd{master} points
   at (i.e., there were no new commits), this will generate a merge commit that
   might not be desired.  To avoid it, you can rebase the branch against your
   @cmd{master} and then do the @cmd{merge} (which will now be a fast-forward)
   merge:
   @pre{git checkout foo
        git rebase master
        git checkout master
        git merge foo}
@~ You no longer need the @cmd{foo} branch, so delete it with:
   @pre{git branch -d foo}
@~ Push things back as usual}

@subsection{Merging github pull-requests}
@p*{
  Github is popular enough that some people prefer to work with a github fork
  of the PLT repository, and then send a pull request.  Merging these pull
  requests can be done as with any other repository, as explained in the
  previous section.  However, with github there is an easy way to deal with
  it.
@~
  A pull request has a URL like @cmd{https://github.com/plt/racket/pull/123}
  which you can use in your browser to inspect the changes.  To apply the
  changes locally, a convenient feature is that you can add a @cmd{.patch}
  suffix to every pull request URL which will have a text version of the patch.
  This means that applying the patch is particularly easy on the command line,
  for example:
  @pre{curl https://github.com/plt/racket/pull/123.patch | git am}
  will fetch the patch text and apply it (and you can now push as usual, or
  locally inspect the ptach and possibly edit it in the usual ways).}

@section{Additional Resources}
@dl*{
@~ @strong{Quick and short:}
@~ @dl*{
   @~ @selflink{http://eagain.net/articles/git-for-computer-scientists/}
   @~ Basic description of what makes a git repository
   @~ Cheat sheets:
   @~ @dl*{
      @~ @selflink{http://gitref.org/}
      @~ Quick reference thing, with links to the git man pages and the progit
         book
      @~ @selflink{http://jonas.nitro.dk/git/quick-reference.html}
      @~ Really short
      @~ @selflink{http://cheat.errtheblog.com/s/git}
      @~ Explains some more
      @~ @selflink{http://ktown.kde.org/~zrusin/git/git-cheat-sheet.svg}
      @~ Short, intended for printing
      @~ @selflink{http://help.github.com/git-cheat-sheets/}
      @~ Similar}
   @~ @selflink{http://git.or.cz/course/svn.html}
   @~ subversion->git crash course
   @~ @selflink{http://www.kernel.org/pub/software/scm/git/docs/everyday.html}
   @~ Nice summary of a few things, but too verbose or too advanced in some
      places, and also a little outdated.}
@~ @strong{Books:}
@~ @dl*{
   @~ @selflink{http://book.git-scm.com/}
   @~ The git community book.  Also, there are a bunch of videos
      linked, and some tutorial links in the “Welcome” part.
   @~ @selflink{http://progit.org/book/}
   @~ A frequently recommended book.  (Also some good blog entries.)
   @~ @selflink{http://www-cs-students.stanford.edu/~blynn/gitmagic/}
   @~ Another good book (a bit more verbose than the previous one)}
@~ @strong{Misc:}
@~ @dl*{
   @~ @selflink{http://www.kernel.org/pub/software/scm/git/docs/@;
                gittutorial.html}
   @~ The git tutorial, also available as the @man{gittutorial} man page.
   @~ @selflink{http://help.github.com/}
   @~ Some github guides, well-organized by levels.
   @~ @selflink{http://www.gitready.com/}
   @~ A kind of a collection of small tips; didn't change in a while though.
   @~ @selflink{http://marklodato.github.com/visual-git-guide/}
   @~ This is a short visual document about git.  But it goes a little fast, so
      it would be useful after you're comfortable with the basics.}}

}}))
