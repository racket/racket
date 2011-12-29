#lang meta/web

(require (prefix-in www: (only-in "../www/resources.rkt" the-resources))
         racket/port)

(define-context "stubs/blog" #:resources www:the-resources)

(define racket-css
  @text{
    @;{
    Instead of hiding the blogger navbar (and eliminate what it is used for),
    just make it appear below the Racket bar, and make it transparent.  The
    next one is supposed to make it get back to being opaque when the mouse
    hovers on it, but it doesn't seem to work on IE.
    ;}
    #navbar-iframe {
      position: absolute;
      top: 160px; right: 0px;
      opacity: 0.33; filter: alpha(opacity=33);
    }
    #navbar-iframe:hover {
      opacity: 1.0; filter: alpha(opacity=100);
    }
    /* --- navbar styles --- */
    @navbar-style
  })

(define (get-resource-text . args)
  (let ([str (xml->string (apply www:the-resources args))])
    ;; due to some obscure xml issue the `nbsp' entity is not recognized
    ;; in blogger pages
    (regexp-replace* #rx"&nbsp;" str "\\&#160;")))

(define (racket-navbar)  (get-resource-text 'navbar 'community))
(define (racket-favicon) (get-resource-text 'favicon-headers))

(provide blog)
(define blog
  @plain[#:file "" #:referrer (λ (u) @a[href: u]{Blog})]{
@; This is the blogger style template file, with one hole for the CSS and one
@; for the navbar, and a few more tweaks (first by soegaard and then by eli).
@;
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:b="http://www.google.com/2005/gml/b" xmlns:data="http://www.google.com/2005/gml/data" xmlns:expr="http://www.google.com/2005/gml/expr">
<head>
<b:include data="blog" name="all-head-content"/>
<title><data:blog.pageTitle/></title>
<b:skin><![CDATA[/*
-----------------------------------------------
Blogger Template Style
Name:     Minima
Designer: Douglas Bowman
URL:      www.stopdesign.com
Date:     26 Feb 2004
Updated by: Blogger Team
----------------------------------------------- */

/* Variable definitions
   ====================
   <Variable name="bgcolor"
             description="Page Background Color"
             type="color"
             default="#fff"
             value="#ffffff">
   <Variable name="textcolor"
             description="Text Color"
             type="color"
             default="#333"
             value="#333333">
   <Variable name="linkcolor"
             description="Link Color"
             type="color"
             default="#58a"
             value="#5588aa">
   <Variable name="pagetitlecolor"
             description="Blog Title Color"
             type="color"
             default="#666"
             value="#ffffff">
   <Variable name="descriptioncolor"
             description="Blog Description Color"
             type="color"
             default="#999"
             value="#999999">
   <Variable name="titlecolor"
             description="Post Title Color"
             type="color"
             default="#c60"
             value="#cc6600">
   <Variable name="bordercolor"
             description="Border Color"
             type="color"
             default="#ccc"
             value="#cccccc">
   <Variable name="sidebarcolor"
             description="Sidebar Title Color"
             type="color"
             default="#999"
             value="#999999">
   <Variable name="sidebartextcolor"
             description="Sidebar Text Color"
             type="color"
             default="#666"
             value="#666666">
   <Variable name="visitedlinkcolor"
             description="Visited Link Color"
             type="color"
             default="#999"
             value="#999999">
   <Variable name="bodyfont"
             description="Text Font"
             type="font"
             default="normal normal 100% Georgia, Serif"
             value="normal normal 100% @font-family">
   <Variable name="headerfont"
             description="Sidebar Title Font"
             type="font"
             default="normal normal 78% 'Trebuchet MS',Trebuchet,Arial,Verdana,Sans-serif"
             value="normal normal 78% @font-family">
   <Variable name="pagetitlefont"
             description="Blog Title Font"
             type="font"
             default="normal normal 200% Georgia, Serif"
             value="normal normal 256% @font-family">
   <Variable name="descriptionfont"
             description="Blog Description Font"
             type="font"
             default="normal normal 78% 'Trebuchet MS', Trebuchet, Arial, Verdana, Sans-serif"
             value="normal normal 78% @font-family">
   <Variable name="postfooterfont"
             description="Post Footer Font"
             type="font"
             default="normal normal 78% 'Trebuchet MS', Trebuchet, Arial, Verdana, Sans-serif"
             value="normal normal 78% @font-family">
*/

/* Use this with templates/template-twocol.html */

body {
  background: $bgcolor;
  margin: 0;
  color: $textcolor;
  // ELI: I don't know why this was here, it was butchering the whole layout
  // font: x-small Georgia Serif;
  // font-size/* */:/**/small;
  // font-size: /**/small;
  // text-align: center;
  }
a:link {
  color: $linkcolor;
  text-decoration: none;
}
a:visited {
  color: $visitedlinkcolor;
  text-decoration: none;
}
a:hover {
  color: $titlecolor;
  text-decoration: underline;
}
a img {
  border-width: 0;
}

/* Header
----------------------------------------------- */

/* ELI: seems like this has no effect on anything */

#header-wrapper {
  width: 800px;        /* this used to be 660 [soegaard] */
  margin: 0 auto 10px;
  border: 1px solid $bordercolor;
}

#header-inner {
  background-position: center;
  margin-left: auto;
  margin-right: auto;
}

#header {
  margin: 5px;
  border: 1px solid $bordercolor;
  text-align: center;
  color: $pagetitlecolor;
}

#header h1 {
  margin: 5px 5px 0;
  padding:15px 20px .25em;
  line-height: 1.2em;
  text-transform: uppercase;
  letter-spacing: .2em;
  font: $pagetitlefont;
}

#header a {
  color: $pagetitlecolor;
  text-decoration: none;
}

#header a:hover {
  color: $pagetitlecolor;
}

#header .description {
  margin: 0 5px 5px;
  padding: 0 20px 15px;
  max-width: 840px;           /* this used to be 700 [soegaard] */
  text-transform: uppercase;
  letter-spacing: .2em;
  line-height: 1.4em;
  font: $descriptionfont;
  color: $descriptioncolor;
}

#header img {
  margin-left: auto;
  margin-right: auto;
}

/* Wrappers
----------------------------------------------- */

/* The whole main area */
#outer-wrapper {
  // ELI:
  margin-top: 0;
  margin-bottom: 0;
  @page-sizes
  padding: 10px;
  text-align: left;
  font: $bodyfont;
  }

/* The text area */
#main-wrapper {
  width: 63%;            /* used to be 410px [soegaard,ELI] */
  float: left;
  word-wrap: break-word; /* fix for long text breaking sidebar float in IE */
  overflow: hidden;      /* fix for long non-text content breaking IE sidebar float */
  }

/* Sidebar stuff */
#sidebar-wrapper {
  width: 33%;            /* used to be 220px [ELI] */
  font-size: 87.5%;      /* ELI */
  float: right;
  word-wrap: break-word; /* fix for long text breaking sidebar float in IE */
  overflow: hidden;      /* fix for long non-text content breaking IE sidebar float */
}

/* Headings
----------------------------------------------- */

h2 {
  margin: 1.5em 0 .75em;
  font: $headerfont;
  line-height: 1.4em;
  text-transform: uppercase;
  letter-spacing: .2em;
  color: $sidebarcolor;
}


/* Posts
----------------------------------------------- */
h2.date-header {
  margin: 1.5em 0 .5em;
}

.post {
  margin: .5em 0 1.5em;
  border-bottom: 1px dotted $bordercolor;
  padding-bottom: 1.5em;
}
.post h3 {
  margin: .25em 0 0;
  padding: 0 0 4px;
  font-size: 140%;
  font-weight: normal;
  line-height: 1.4em;
  color: $titlecolor;
}

.post h3 a, .post h3 a:visited, .post h3 strong {
  display: block;
  text-decoration: none;
  color: $titlecolor;
  font-weight: normal;
}

.post h3 strong, .post h3 a:hover {
  color: $textcolor;
}

.post p {
  margin: 0 0 .75em;
  line-height: 1.6em;
}

.post-footer {
  margin: .75em 0;
  color: $sidebarcolor;
  text-transform: uppercase;
  letter-spacing: .1em;
  font: $postfooterfont;
  line-height: 1.4em;
}

.comment-link {
  margin-left: .6em;
}
.post img {
  padding: 4px;
  border: 1px solid $bordercolor;
}
.post blockquote {
  margin: 1em 20px;
}
.post blockquote p {
  margin: .75em 0;
}

.post-header-line-1 {    /* add some vertical space after author [soegaard] */
  margin-bottom: .6em;
  font-style: italic;
}

/* Comments
----------------------------------------------- */
#comments h4 {
  margin: 1em 0;
  font-weight: bold;
  line-height: 1.4em;
  text-transform: uppercase;
  letter-spacing: .2em;
  color: $sidebarcolor;
}

#comments-block {
  margin: 1em 0 1.5em;
  line-height: 1.6em;
}
#comments-block .comment-author {
  margin: .5em 0;
}
#comments-block .comment-body {
  margin: .25em 0 0;
}
#comments-block .comment-footer {
  margin: -.25em 0 2em;
  line-height: 1.4em;
  text-transform: uppercase;
  letter-spacing: .1em;
}
#comments-block .comment-body p {
  margin: 0 0 .75em;
}
.deleted-comment {
  font-style: italic;
  color: gray;
}

#blog-pager-newer-link {
  float: left;
}

#blog-pager-older-link {
  float: right;
}

#blog-pager {
  text-align: center;
}

.feed-links {
  clear: both;
  line-height: 2.5em;
}

/* Sidebar Content
----------------------------------------------- */
.sidebar {
  color: $sidebartextcolor;
  line-height: 1.5em;
}

.sidebar ul {
  list-style: none;
  margin: 0 0 0;
  padding: 0 0 0;
}
.sidebar li {
  margin: 0;
  padding: 0 0 .25em 15px;
  text-indent: -15px;
  line-height: 1.5em;
}

.sidebar .widget, .main .widget {
  border-bottom: 1px dotted $bordercolor;
  margin: 0 0 1.5em;
  padding: 0 0 1.5em;
}

.main .Blog {
  border-bottom-width: 0;
}


/* Profile
----------------------------------------------- */
.profile-img {
  float: left;
  margin: 0 5px 5px 0;
  padding: 4px;
  border: 1px solid $bordercolor;
}

.profile-data {
  margin: 0;
  text-transform: uppercase;
  letter-spacing: .1em;
  font: $postfooterfont;
  color: $sidebarcolor;
  font-weight: bold;
  line-height: 1.6em;
}

.profile-datablock {
  margin: .5em 0 .5em;
}

.profile-textblock {
  margin: 0.5em 0;
  line-height: 1.6em;
}

.profile-link {
  font: $postfooterfont;
  text-transform: uppercase;
  letter-spacing: .1em;
}

/* Colors for highlighting via http://www.scheme.dk/paste/ */

.scheme           { color: brown; margin: 4pt; }  /* background punctuation */
.scheme .keyword  { color: rgb(68,0,203); font-weight: bold; }
.scheme .builtin  { color: navy;   }
.scheme .variable { color: black;  }
.scheme .global   { color: purple; }
.scheme .selfeval { color: green;  }
.scheme .comment  { color: teal;   }

/* Footer
----------------------------------------------- */
#footer {
  width: 660px;
  clear: both;
  margin: 0 auto;
  padding-top: 15px;
  line-height: 1.6em;
  text-transform: uppercase;
  letter-spacing: .1em;
  text-align: center;
}

/** Page structure tweaks for layout editor wireframe */
body#layout #header {
  margin-left: 0px;
  margin-right: 0px;
}

/*** Racket CSS begin ***/
@racket-css
/*** Racket CSS end ***/

]]></b:skin>

@racket-favicon

</head>

<body>

<!-- Racket navbar begin -->
@racket-navbar
<!-- Racket navbar end -->

<div id="outer-wrapper"><div id="wrap2">

  <!-- skip links for text browsers -->
  <span id="skiplinks" style="display: none;">
    <a href="#main">skip to main </a> |
    <a href="#sidebar">skip to sidebar</a>
  </span>

  <div id="content-wrapper">

    <div id="crosscol-wrapper" style="text-align: center;">
      <b:section class="crosscol" id="crosscol" showaddelement="no"/>
    </div>

    <div id="main-wrapper">
      <b:section class="main" id="main" showaddelement="no">
        <b:widget id="Blog1" locked="true" title="Blog Posts" type="Blog">
          <b:includable id="nextprev">
            <div class="blog-pager" id="blog-pager">
              <b:if cond="data:newerPageUrl">
                <span id="blog-pager-newer-link">
                  <a class="blog-pager-newer-link" expr:href="data:newerPageUrl" expr:id="data:widget.instanceId + &quot;_blog-pager-newer-link&quot;" expr:title="data:newerPageTitle"><data:newerPageTitle/></a>
                </span>
              </b:if>
              <b:if cond="data:olderPageUrl">
                <span id="blog-pager-older-link">
                  <a class="blog-pager-older-link" expr:href="data:olderPageUrl" expr:id="data:widget.instanceId + &quot;_blog-pager-older-link&quot;" expr:title="data:olderPageTitle"><data:olderPageTitle/></a>
                </span>
              </b:if>
              <a class="home-link" expr:href="data:blog.homepageUrl"><data:homeMsg/></a>
            </div>
            <div class="clear"/>
          </b:includable>
          <b:includable id="shareButtons" var="post">
            <b:if cond="data:post.sharePostUrl">
              <a class="share-button sb-email" expr:href="data:post.sharePostUrl + &quot;&amp;target=email&quot;" expr:title="data:top.emailThisMsg" target="_blank"><span class="share-button-link-text"><data:top.emailThisMsg/></span></a>
              <a class="share-button sb-blog" expr:href="data:post.sharePostUrl + &quot;&amp;target=blog&quot;" expr:onclick="&quot;window.open(this.href, \&quot;_blank\&quot;, \&quot;height=270,width=475\&quot;); return false;&quot;" expr:title="data:top.blogThisMsg" target="_blank"><span class="share-button-link-text"><data:top.blogThisMsg/></span></a>
              <a class="share-button sb-twitter" expr:href="data:post.sharePostUrl + &quot;&amp;target=twitter&quot;" expr:title="data:top.shareToTwitterMsg" target="_blank"><span class="share-button-link-text"><data:top.shareToTwitterMsg/></span></a>
              <a class="share-button sb-facebook" expr:href="data:post.sharePostUrl + &quot;&amp;target=facebook&quot;" expr:onclick="&quot;window.open(this.href, \&quot;_blank\&quot;, \&quot;height=430,width=640\&quot;); return false;&quot;" expr:title="data:top.shareToFacebookMsg" target="_blank"><span class="share-button-link-text"><data:top.shareToFacebookMsg/></span></a>
              <a class="share-button sb-buzz" expr:href="data:post.sharePostUrl + &quot;&amp;target=buzz&quot;" expr:onclick="&quot;window.open(this.href, \&quot;_blank\&quot;, \&quot;height=415,width=690\&quot;); return false;&quot;" expr:title="data:top.shareToBuzzMsg" target="_blank"><span class="share-button-link-text"><data:top.shareToBuzzMsg/></span></a>
            </b:if>
          </b:includable>
          <b:includable id="backlinks" var="post">
            <a name="links"/><h4><data:post.backlinksLabel/></h4>
            <b:if cond="data:post.numBacklinks != 0">
              <dl class="comments-block" id="comments-block">
                <b:loop values="data:post.backlinks" var="backlink">
                  <div class="collapsed-backlink backlink-control">
                    <dt class="comment-title">
                      <span class="backlink-toggle-zippy">&#160;</span>
                      <a expr:href="data:backlink.url" rel="nofollow"><data:backlink.title/></a>
                      <b:include data="backlink" name="backlinkDeleteIcon"/>
                    </dt>
                    <dd class="comment-body collapseable">
                      <data:backlink.snippet/>
                    </dd>
                    <dd class="comment-footer collapseable">
                      <span class="comment-author"><data:post.authorLabel/> <data:backlink.author/></span>
                      <span class="comment-timestamp"><data:post.timestampLabel/> <data:backlink.timestamp/></span>
                    </dd>
                  </div>
                </b:loop>
              </dl>
            </b:if>
            <p class="comment-footer">
              <a class="comment-link" expr:href="data:post.createLinkUrl" expr:id="data:widget.instanceId + &quot;_backlinks-create-link&quot;" target="_blank"><data:post.createLinkLabel/></a>
            </p>
          </b:includable>
          <b:includable id="post" var="post">
            <div class="post">
              <a expr:name="data:post.id"/>
              <b:if cond="data:post.title">
                <h3 class="post-title">
                  <b:if cond="data:post.link">
                    <a expr:href="data:post.link"><data:post.title/></a>
                    <b:else/>
                    <b:if cond="data:post.url">
                      <a expr:href="data:post.url"><data:post.title/></a>
                      <b:else/>
                      <data:post.title/>
                    </b:if>
                  </b:if>
                </h3>
              </b:if>

              <div class="post-header-line-1"><span class="post-author">
                  <b:if cond="data:top.showAuthor">
                    <data:top.authorLabel/> <data:post.author/>
                  </b:if>
                </span> <span class="post-timestamp">
                  <b:if cond="data:top.showTimestamp">
                    <data:top.timestampLabel/>
                    <b:if cond="data:post.url">
                      <a class="timestamp-link" expr:href="data:post.url" title="permanent link"><data:post.timestamp/></a>
                    </b:if>
                  </b:if>
              </span> </div>

              <div class="post-body">
                <p><data:post.body/></p>
                <div style="clear: both;"/> <!-- clear for photos floats -->
              </div>
              <div class="post-footer">
                <p class="post-footer-line post-footer-line-1"><span class="post-comment-link">
                    <b:if cond="data:blog.pageType != &quot;item&quot;">

                      <b:if cond="data:post.allowComments">
                        <a class="comment-link" expr:href="data:post.addCommentUrl" expr:onclick="data:post.addCommentOnclick"><b:if cond="data:post.numComments == 1">1 <data:top.commentLabel/><b:else/><data:post.numComments/> <data:top.commentLabelPlural/></b:if></a>
                      </b:if>
                    </b:if>
                  </span> <span class="post-icons">
                    <!-- email post links -->
                    <b:if cond="data:post.emailPostUrl">
                      <span class="item-action">
                        <a expr:href="data:post.emailPostUrl" expr:title="data:top.emailPostMsg">
                          <span class="email-post-icon">&#160;</span>
                        </a>
                      </span>
                    </b:if>

                    <!-- quickedit pencil -->
                    <b:include data="post" name="postQuickEdit"/>
                </span> </p>

                <p class="post-footer-line post-footer-line-2"><span class="post-labels">
                    <b:if cond="data:post.labels">
                      <data:postLabelsLabel/>
                      <b:loop values="data:post.labels" var="label">
                        <a expr:href="data:label.url" rel="tag"><data:label.name/></a><b:if cond="data:label.isLast != &quot;true&quot;">,</b:if>
                      </b:loop>
                    </b:if>
                </span></p>

                <p class="post-footer-line post-footer-line-3"/>
              </div>
            </div>
          </b:includable>
          <b:includable id="status-message">
            <b:if cond="data:navMessage">
              <div class="status-msg-wrap">
                <div class="status-msg-body">
                  <data:navMessage/>
                </div>
                <div class="status-msg-border">
                  <div class="status-msg-bg">
                    <div class="status-msg-hidden"><data:navMessage/></div>
                  </div>
                </div>
              </div>
              <div style="clear: both;"/>
            </b:if>
          </b:includable>
          <b:includable id="comment-form" var="post">
            <div class="comment-form">
              <a name="comment-form"/>
              <h4 id="comment-post-message"><data:postCommentMsg/></h4>
              <p><data:blogCommentMessage/></p>
              <data:blogTeamBlogMessage/>
              <a expr:href="data:post.commentFormIframeSrc" id="comment-editor-src"/>
              <iframe allowtransparency="true" class="blogger-iframe-colorize blogger-comment-from-post" frameborder="0" height="410" id="comment-editor" name="comment-editor" src="" width="100%"/>
              <data:post.friendConnectJs/>
              <data:post.cmtfpIframe/>
              <script type="text/javascript">
                BLOG_CMT_createIframe(&#39;<data:post.appRpcRelayPath/>&#39;, &#39;<data:post.communityId/>&#39;);
              </script>
            </div>
          </b:includable>
          <b:includable id="backlinkDeleteIcon" var="backlink">
            <span expr:class="&quot;item-control &quot; + data:backlink.adminClass">
              <a expr:href="data:backlink.deleteUrl" expr:title="data:top.deleteBacklinkMsg">
                <img src="//www.blogger.com/img/icon_delete13.gif"/>
              </a>
            </span>
          </b:includable>
          <b:includable id="postQuickEdit" var="post">
            <b:if cond="data:post.editUrl">
              <span expr:class="&quot;item-control &quot; + data:post.adminClass">
                <a expr:href="data:post.editUrl" expr:title="data:top.editPostMsg">
                  <img alt="" class="icon-action" height="18" src="http://img2.blogblog.com/img/icon18_edit_allbkg.gif" width="18"/>
                </a>
              </span>
            </b:if>
          </b:includable>
          <b:includable id="main" var="top">
            <!-- posts -->
            <div class="blog-posts hfeed">

              <b:include data="top" name="status-message"/>

              <data:defaultAdStart/>
              <b:loop values="data:posts" var="post">
                <b:if cond="data:post.isDateStart">
                  <b:if cond="data:post.isFirstPost == &quot;false&quot;">
                    &lt;/div&gt;&lt;/div&gt;
                  </b:if>
                </b:if>
                <b:if cond="data:post.isDateStart">
                  &lt;div class=&quot;date-outer&quot;&gt;
                </b:if>
                <b:if cond="data:post.dateHeader">
                  <h2 class="date-header"><span><data:post.dateHeader/></span></h2>
                </b:if>
                <b:if cond="data:post.isDateStart">
                  &lt;div class=&quot;date-posts&quot;&gt;
                </b:if>
                <div class="post-outer">
                  <b:include data="post" name="post"/>
                  <b:if cond="data:blog.pageType == &quot;static_page&quot;">
                    <b:include data="post" name="comments"/>
                  </b:if>
                  <b:if cond="data:blog.pageType == &quot;item&quot;">
                    <b:include data="post" name="comments"/>
                  </b:if>
                </div>
                <b:if cond="data:post.includeAd">
                  <b:if cond="data:post.isFirstPost">
                    <data:defaultAdEnd/>
                    <b:else/>
                    <data:adEnd/>
                  </b:if>
                  <div class="inline-ad">
                    <data:adCode/>
                  </div>
                  <data:adStart/>
                </b:if>
                <b:if cond="data:post.trackLatency">
                  <data:post.latencyJs/>
                </b:if>
              </b:loop>
              <b:if cond="data:numPosts != 0">
                &lt;/div&gt;&lt;/div&gt;
              </b:if>
              <data:adEnd/>
            </div>

            <!-- navigation -->
            <b:include name="nextprev"/>

            <!-- feed links -->
            <b:include name="feedLinks"/>

            <b:if cond="data:top.showStars">
              <script src="//www.google.com/jsapi" type="text/javascript"/>
              <script type="text/javascript">
                google.load(&quot;annotations&quot;, &quot;1&quot;, {&quot;locale&quot;: &quot;<data:top.languageCode/>&quot;});
                function initialize() {
                google.annotations.setApplicationId(<data:top.blogspotReviews/>);
                google.annotations.createAll();
                google.annotations.fetch();
                }
                google.setOnLoadCallback(initialize);
              </script>
            </b:if>
          </b:includable>
          <b:includable id="commentDeleteIcon" var="comment">
            <span expr:class="&quot;item-control &quot; + data:comment.adminClass">
              <a expr:href="data:comment.deleteUrl" expr:title="data:top.deleteCommentMsg">
                <img src="//www.blogger.com/img/icon_delete13.gif"/>
              </a>
            </span>
          </b:includable>
          <b:includable id="feedLinks">
            <b:if cond="data:blog.pageType != &quot;item&quot;"> <!-- Blog feed links -->
              <b:if cond="data:feedLinks">
                <div class="blog-feeds">
                  <b:include data="feedLinks" name="feedLinksBody"/>
                </div>
              </b:if>
              <b:else/> <!--Post feed links -->
              <div class="post-feeds">
                <b:loop values="data:posts" var="post">
                  <b:if cond="data:post.allowComments">
                    <b:if cond="data:post.feedLinks">
                      <b:include data="post.feedLinks" name="feedLinksBody"/>
                    </b:if>
                  </b:if>
                </b:loop>
              </div>
            </b:if>
          </b:includable>
          <b:includable id="feedLinksBody" var="links">
            <div class="feed-links">
              <data:feedLinksMsg/>
              <b:loop values="data:links" var="f">
                <a class="feed-link" expr:href="data:f.url" expr:type="data:f.mimeType" target="_blank"><data:f.name/> (<data:f.feedType/>)</a>
              </b:loop>
            </div>
          </b:includable>
          <b:includable id="comments" var="post">
            <div class="comments" id="comments">
              <a name="comments"/>
              <b:if cond="data:post.allowComments">
                <h4>
                  <b:if cond="data:post.numComments == 1">
                    1 <data:commentLabel/>:
                    <b:else/>
                    <data:post.numComments/> <data:commentLabelPlural/>:
                  </b:if>
                </h4>

                <b:if cond="data:post.commentPagingRequired">
                  <span class="paging-control-container">
                    <a expr:class="data:post.oldLinkClass" expr:href="data:post.oldestLinkUrl"><data:post.oldestLinkText/></a>
                    &#160;
                    <a expr:class="data:post.oldLinkClass" expr:href="data:post.olderLinkUrl"><data:post.olderLinkText/></a>
                    &#160;
                    <data:post.commentRangeText/>
                    &#160;
                    <a expr:class="data:post.newLinkClass" expr:href="data:post.newerLinkUrl"><data:post.newerLinkText/></a>
                    &#160;
                    <a expr:class="data:post.newLinkClass" expr:href="data:post.newestLinkUrl"><data:post.newestLinkText/></a>
                  </span>
                </b:if>

                <div expr:class="data:post.postAuthorClass" expr:id="data:widget.instanceId + &quot;_comments-block-wrapper&quot;">
                  <dl expr:class="data:post.avatarIndentClass" id="comments-block">
                    <b:loop values="data:post.comments" var="comment">
                      <dt expr:class="&quot;comment-author &quot; + data:comment.authorClass" expr:id="data:comment.anchorName">
                        <b:if cond="data:comment.favicon">
                          <img expr:src="data:comment.favicon" height="16px" style="margin-bottom:-2px;" width="16px"/>
                        </b:if>
                        <a expr:name="data:comment.anchorName"/>
                        <b:if cond="data:blog.enabledCommentProfileImages">
                          <data:comment.authorAvatarImage/>
                        </b:if>
                        <b:if cond="data:comment.authorUrl">
                          <a expr:href="data:comment.authorUrl" rel="nofollow"><data:comment.author/></a>
                          <b:else/>
                          <data:comment.author/>
                        </b:if>
                        <data:commentPostedByMsg/>
                      </dt>
                      <dd expr:class="&quot;comment-body &quot; + data:comment.commentAuthorClass" expr:id="data:widget.instanceId + data:comment.cmtBodyIdPostfix">
                        <b:if cond="data:comment.isDeleted">
                          <span class="deleted-comment"><data:comment.body/></span>
                          <b:else/>
                          <p>
                            <data:comment.body/>
                            <span class="interaction-iframe-guide"/>
                          </p>
                        </b:if>
                      </dd>
                      <dd class="comment-footer">
                        <span class="comment-timestamp">
                          <a expr:href="data:comment.url" title="comment permalink">
                            <data:comment.timestamp/>
                          </a>
                          <b:include data="comment" name="commentDeleteIcon"/>
                        </span>
                      </dd>
                    </b:loop>
                  </dl>
                </div>

                <b:if cond="data:post.commentPagingRequired">
                  <span class="paging-control-container">
                    <a expr:class="data:post.oldLinkClass" expr:href="data:post.oldestLinkUrl">
                      <data:post.oldestLinkText/>
                    </a>
                    <a expr:class="data:post.oldLinkClass" expr:href="data:post.olderLinkUrl">
                      <data:post.olderLinkText/>
                    </a>
                    &#160;
                    <data:post.commentRangeText/>
                    &#160;
                    <a expr:class="data:post.newLinkClass" expr:href="data:post.newerLinkUrl">
                      <data:post.newerLinkText/>
                    </a>
                    <a expr:class="data:post.newLinkClass" expr:href="data:post.newestLinkUrl">
                      <data:post.newestLinkText/>
                    </a>
                  </span>
                </b:if>

                <p class="comment-footer">
                  <b:if cond="data:post.embedCommentForm">
                    <b:if cond="data:post.allowNewComments">
                      <b:include data="post" name="comment-form"/>
                      <b:else/>
                      <data:post.noNewCommentsText/>
                    </b:if>
                    <b:else/>
                    <b:if cond="data:post.allowComments">
                      <a expr:href="data:post.addCommentUrl" expr:onclick="data:post.addCommentOnclick"><data:postCommentMsg/></a>
                    </b:if>
                  </b:if>

                </p>
              </b:if>

              <div id="backlinks-container">
                <div expr:id="data:widget.instanceId + &quot;_backlinks-container&quot;">
                  <b:if cond="data:post.showBacklinks">
                    <b:include data="post" name="backlinks"/>
                  </b:if>
                </div>
              </div>
            </div>
          </b:includable>
        </b:widget>
      </b:section>
    </div>

    <div id="sidebar-wrapper">
      <b:section class="sidebar" id="sidebar" preferred="yes">
        <b:widget id="BlogArchive1" locked="false" title="Blog Archive" type="BlogArchive">
          <b:includable id="main">
            <b:if cond="data:title">
              <h2><data:title/></h2>
            </b:if>
            <div class="widget-content">
              <div id="ArchiveList">
                <div expr:id="data:widget.instanceId + &quot;_ArchiveList&quot;">
                  <b:if cond="data:style == &quot;HIERARCHY&quot;">
                    <b:include data="data" name="interval"/>
                  </b:if>
                  <b:if cond="data:style == &quot;FLAT&quot;">
                    <b:include data="data" name="flat"/>
                  </b:if>
                  <b:if cond="data:style == &quot;MENU&quot;">
                    <b:include data="data" name="menu"/>
                  </b:if>
                </div>
              </div>
              <b:include name="quickedit"/>
            </div>
          </b:includable>
          <b:includable id="flat" var="data">
            <ul class="flat">
              <b:loop values="data:data" var="i">
                <li class="archivedate">
                  <a expr:href="data:i.url"><data:i.name/></a> (<data:i.post-count/>)
                </li>
              </b:loop>
            </ul>
          </b:includable>
          <b:includable id="menu" var="data">
            <select expr:id="data:widget.instanceId + &quot;_ArchiveMenu&quot;">
              <option value=""><data:title/></option>
              <b:loop values="data:data" var="i">
                <option expr:value="data:i.url"><data:i.name/> (<data:i.post-count/>)</option>
              </b:loop>
            </select>
          </b:includable>
          <b:includable id="interval" var="intervalData">
            <b:loop values="data:intervalData" var="i">
              <ul class="hierarchy">
                <li expr:class="&quot;archivedate &quot; + data:i.expclass">
                  <b:include data="i" name="toggle"/>
                  <a class="post-count-link" expr:href="data:i.url"><data:i.name/></a>
                  <span class="post-count" dir="ltr">(<data:i.post-count/>)</span>
                  <b:if cond="data:i.data">
                    <b:include data="i.data" name="interval"/>
                  </b:if>
                  <b:if cond="data:i.posts">
                    <b:include data="i.posts" name="posts"/>
                  </b:if>
                </li>
              </ul>
            </b:loop>
          </b:includable>
          <b:includable id="toggle" var="interval">
            <b:if cond="data:interval.toggleId">
              <b:if cond="data:interval.expclass == &quot;expanded&quot;">
                <a class="toggle" href="javascript:void(0)">
                  <span class="zippy toggle-open">&#9660;&#160;</span>
                </a>
                <b:else/>
                <a class="toggle" href="javascript:void(0)">
                  <span class="zippy">
                    <b:if cond="data:blog.languageDirection == &quot;rtl&quot;">
                      &#9668;&#160;
                      <b:else/>
                      &#9658;&#160;
                    </b:if>
                  </span>
                </a>
              </b:if>
            </b:if>
          </b:includable>
          <b:includable id="posts" var="posts">
            <ul class="posts">
              <b:loop values="data:posts" var="i">
                <li><a expr:href="data:i.url"><data:i.title/></a></li>
              </b:loop>
            </ul>
          </b:includable>
        </b:widget>
        <b:widget id="LinkList1" locked="false" title="Racket Resources" type="LinkList">
          <b:includable id="main">

            <b:if cond="data:title"><h2><data:title/></h2></b:if>
            <div class="widget-content">
              <ul>
                <b:loop values="data:links" var="link">
                  <li><a expr:href="data:link.target"><data:link.name/></a></li>
                </b:loop>
              </ul>
              <b:include name="quickedit"/>
            </div>
          </b:includable>
        </b:widget>
        <b:widget id="Label1" locked="false" title="Labels" type="Label">
          <b:includable id="main">
            <b:if cond="data:title">
              <h2><data:title/></h2>
            </b:if>
            <div expr:class="&quot;widget-content &quot; + data:display + &quot;-label-widget-content&quot;">
              <b:if cond="data:display == &quot;list&quot;">
                <ul>
                  <b:loop values="data:labels" var="label">
                    <li>
                      <b:if cond="data:blog.url == data:label.url">
                        <span expr:dir="data:blog.languageDirection"><data:label.name/></span>
                        <b:else/>
                        <a expr:dir="data:blog.languageDirection" expr:href="data:label.url"><data:label.name/></a>
                      </b:if>
                      <b:if cond="data:showFreqNumbers">
                        <span dir="ltr">(<data:label.count/>)</span>
                      </b:if>
                    </li>
                  </b:loop>
                </ul>
                <b:else/>
                <b:loop values="data:labels" var="label">
                  <span expr:class="&quot;label-size label-size-&quot; + data:label.cssSize">
                    <b:if cond="data:blog.url == data:label.url">
                      <span expr:dir="data:blog.languageDirection"><data:label.name/></span>
                      <b:else/>
                      <a expr:dir="data:blog.languageDirection" expr:href="data:label.url"><data:label.name/></a>
                    </b:if>
                    <b:if cond="data:showFreqNumbers">
                      <span class="label-count" dir="ltr">(<data:label.count/>)</span>
                    </b:if>
                  </span>
                </b:loop>
              </b:if>
              <b:include name="quickedit"/>
            </div>
          </b:includable>
        </b:widget>
        <b:widget id="Profile1" locked="false" title="Contributors" type="Profile">
          <b:includable id="main">
            <b:if cond="data:title != &quot;&quot;">
              <h2><data:title/></h2>
            </b:if>
            <div class="widget-content">
              <b:if cond="data:team == &quot;true&quot;"> <!-- team blog profile -->
                <ul>
                  <b:loop values="data:authors" var="i">
                    <li><a expr:href="data:i.userUrl"><data:i.display-name/></a></li>
                  </b:loop>
                </ul>

                <b:else/> <!-- normal blog profile -->

                <b:if cond="data:photo.url != &quot;&quot;">
                  <a expr:href="data:userUrl"><img class="profile-img" expr:alt="data:photo.alt" expr:height="data:photo.height" expr:src="data:photo.url" expr:width="data:photo.width"/></a>
                </b:if>

                <dl class="profile-datablock">
                  <dt class="profile-data"><data:displayname/></dt>

                  <b:if cond="data:showlocation == &quot;true&quot;">
                    <dd class="profile-data"><data:location/></dd>
                  </b:if>

                  <b:if cond="data:aboutme != &quot;&quot;"><dd class="profile-textblock"><data:aboutme/></dd></b:if>
                </dl>
                <a class="profile-link" expr:href="data:userUrl"><data:viewProfileMsg/></a>
              </b:if>

              <b:include name="quickedit"/>
            </div>
          </b:includable>
        </b:widget>
      </b:section>
    </div>

    <!-- spacer for skins that want sidebar and main to be the same height-->
    <div class="clear">&#160;</div>

  </div> <!-- end content-wrapper -->

  <div id="footer-wrapper">
    <b:section class="footer" id="footer"/>
  </div>

</div></div>
</body>
</html>
})
