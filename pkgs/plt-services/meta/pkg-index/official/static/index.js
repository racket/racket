// xxx change these to the real thing
var dynamic_host = "localhost";
var dynamic_port = 9004;

function dynamic_url ( u ) {
    return "https://" + dynamic_host + ":" + dynamic_port + u + "?callback=?"; }

$( document ).ready(function() {
    $("#package_info").dialog({
        autoOpen: false,
        minWidth: 600,
        minHeight: 600,
        position: { my: "center", at: "center", of: "#search_menu" },
        modal: true });

    function format_time( t ) {
        var d = new Date( t * 1000 );
        return d.toLocaleString(); }

    var active_info = false;
    var target_pkg = false;
    function update_info( pkgi ) {
        console.log( pkgi );
        change_hash( "[" + pkgi['name'] + "]" );
        $( "#pi_name" ).text( pkgi['name'] );
        $( "#pi_name_inst" ).text( pkgi['name'] );
        $( "#pi_ring" ).text( pkgi['ring'] );
        $( "#pi_authors" ).html("")
            .append( $.map( pkgi['authors'],
                            function ( author, i ) {
                                return [author, " "]; } ) )

        $( "#pi_source" ).html( $('<a>', { text: pkgi['source'],
                                           href: pkgi['source_url']  } ));

        $( "#pi_checksum" ).text( pkgi['checksum'] );
        $( "#pi_last_updated" ).text( format_time(pkgi['last-updated']) );
        $( "#pi_last_checked" ).text( format_time(pkgi['last-checked']) );
        $( "#pi_last_edit" ).text( format_time(pkgi['last-edit']) );
        $( "#pi_description" ).text( pkgi['description'] );
        // xxx show delete tag buttons
        $( "#pi_tags" ).html("").append( $.map( pkgi['tags'], function ( tag, i ) {
            return [tag, " "]; } ) )
        // xxx show add and delete buttons
        $( "#pi_versions" ).html("").append( $.map( pkgi['versions'], function ( vo, v ) {
            return [ $('<tr>').append( $('<td>').html(v),
                                       $('<td>').html(vo['source']) ),
                     $('<tr>').append( $('<td>').html(""),
                                       $('<td>').html(vo['checksum']) ),
                     " "]; } ) )
        active_info = pkgi; };

    function submit_add_tag () {
        var it = $( "#pi_add_tag_text" );
        // XXX really add tag
        active_info['tags'].push( it.val() );
        update_info( active_info );

        it.val("");}
    $( "#pi_add_tag_text" ).keypress( function (e) {
        if (e.which == 13) { submit_add_tag (); } } );
    $( "#pi_add_tag_button" ).click( function (e) { submit_add_tag (); } );

    var search_terms = { };

    function clear_terms () {
        $.each(search_terms,
               function ( term, termv ) {
                   delete search_terms[term];} ); }

    function parse_hash ( h ) {
        while ( h != "" ) {
            if ( h.charAt(0) == "(" ) {
                var end = 1;
                while ( h.charAt(end) != ")" ) {
                    end++;
                    if ( ! h.charAt(end) ) { break; } }
                search_terms[ h.substring(1, end) ] = true;
                h = h.substring(end+1); }
            else if ( h.charAt(0) == "[" ) {
                target_pkg = h.substring(1, h.length - 1 );
                h = ""; }
            else {
                h = ""; } } }

    { var h = window.location.hash;
      if ( h == "" ) {
          search_terms["!main-tests"] = true;
          search_terms["!main-distribution"] = true; }
      else {
          h = h.substring(1);
          parse_hash(h); } }

    var expected_hash = "";
    function change_hash ( v ) {
        expected_hash = v;
        window.location.hash = v; }

    $(window).bind( 'hashchange', function(e) {
        var actual_hash = window.location.hash;
        if ( expected_hash != actual_hash ) {
            // xxx Do something here. It is hard to do the right
            // thing, particularly with the Back button because we
            // don't add the tags in the same order the user add them
            // in. We could do that though.
            console.log("hash changed beneath me!"); } });

    function filterlink ( text, tclass, f ) {
        return [$('<a>', { text: text,
                           class: tclass,
                           href: "javascript:void(0)",
                           click: f } ),
                " " ]; };

    function addfilterlink ( text, term, tclass ) {
        return filterlink( text, tclass, function () {
            search_terms[term] = true;
            evaluate_search(); } ); };
    function removefilterlink ( text, term, tclass ) {
        return filterlink( text, tclass, function () {
            delete search_terms[term];
            evaluate_search(); } ); };
    function changefilterlink ( text, term, nterm, tclass ) {
        return filterlink( text, tclass, function () {
            delete search_terms[term];
            search_terms["!" + term] = true;
            evaluate_search(); } ); };

    function evaluate_search () {
        var shown_terms = {};

        $.each( $('#packages_table tr'), function (key, dom) {
            var value = $(dom).data("obj");
            var show = true;
            var vterms = value['search-terms'];

            $.each(search_terms,
                   function ( term, termv ) {
                       if ( term.charAt(0) == "!" ) {
                           if ( vterms[term.substring(1)] ) {
                               show = false; } }
                       else {
                           if ( ! vterms[term] ) {
                               show = false; } } });

            if ( show ) {
                $(dom).show();

                $.each(vterms, function ( term, termv ) {
                    if ( term.substring(0,7) != "author:") { shown_terms[term]++; } }); }
            else {
                $(dom).hide();

                $.each(vterms, function ( term, termv ) {
                    if ( term.substring(0,7) != "author:") {
                        if ( ! shown_terms[term]) { shown_terms[term] = 0; } } });} });

        $.each(search_terms,
               function ( term, termv ) {
                   if ( term.charAt(0) == "!" ) {
                       shown_terms[ term.substring(1) ] = -2; }
                   else {
                       shown_terms[ term ] = -1; } });

        var shown_terms_keys = object_keys(shown_terms);
        var shown_terms_skeys = shown_terms_keys.sort(function(a,b) {
            return ((a < b) ? -1 : ((a > b) ? 1 : 0)); });

        change_hash( "" );
        $("#search_menu").html("").append( $.map( shown_terms_skeys, function ( term, i ) {
            if ( shown_terms[term] < 0 ) {
                if ( shown_terms[term] == -1 ) {
                    change_hash( window.location.hash + "(" + term + ")" );
                    return changefilterlink ( term, term, "!" + term, "active" ); }
                else {
                    change_hash( window.location.hash + "(" + "!" + term + ")" );
                    return removefilterlink ( term, "!" + term, "inactive" ); } }
            else if ( shown_terms[term] == 0 ) {
                return [ term, " " ]; }
            else {
                return addfilterlink ( term, term, "possible" ); } } ) );

        $("#packages_table tr:visible:even").removeClass("even");
        $("#packages_table tr:visible:odd").addClass("even"); };

    function object_keys ( o ) {
        var names = [];
        $.each(o, function(key, value) { names.push(key) });
        return names; }

    function open_info ( i ) {
        update_info( i );
        $( "#package_info" ).dialog( "open" ); }

    $.getJSON( "/pkgs-all.json", function( resp ) {
        var names = object_keys(resp);
        var snames = names.sort(function(a,b) {
            return ((a < b) ? -1 : ((a > b) ? 1 : 0)); })

        var now = new Date().getTime() / 1000;

        $.each( snames,
                function (name_i) {
                    var name = snames[name_i];
                    var value = resp[name];

                    $('<tr>',
                      { class: ((now - (60*60*24*2)) < value['last-updated'] ? "recent" : "old") })
                        .data( "obj", value)
                        .append(
                            $('<td>').html("")
                                .append( $('<span>', { class: "curate_link" } ).hide()
                                         // xxx make these links
                                         .append("&blacktriangledown;", "1", "&blacktriangle;", "&nbsp;"),
                                         $('<a>', { text: value['name'],
                                                    href: "javascript:void(0)",
                                                    click: function () {
                                                        open_info ( value ); } } ) ),
                            $('<td>').append( $.map( value['authors'], function ( author, i ) {
                                return addfilterlink ( author, "author:" + author, "possible" ); } ) ),
                            $('<td>').text( value['description'] ),
                            $('<td>').append( $.map( value['tags'], function ( tag, i ) {
                                return addfilterlink ( tag, tag, "possible" ); } ) ))
                        .appendTo('#packages_table'); });

        evaluate_search();

        if ( target_pkg && resp[target_pkg] ) {
            open_info ( resp[target_pkg] ) } });

    $("#login").dialog({
        autoOpen: false,
        minWidth: 600,
        minHeight: 600,
        position: { my: "center", at: "center", of: "#search_menu" },
        modal: true });

    var saved_password = false;
    function login_submit () {
        $( "#login_error" ).html( "" );

        var et = $( "#login_email_text" );
        var pt = $( "#login_passwd_text" );

        var e = et.val();
        var p;
        var c;
        if ( saved_password ) {
            p = saved_password;
            c = pt.val(); }
        else {
            p = pt.val();
            c = ""; }

        $.getJSON( dynamic_url("/jsonp/authenticate"),
                   { email: e, passwd: p, code: c },
                   function( resp ) {
                       if ( resp == "emailed" ) {
                           saved_password = p;
                           $( "#login_passwd_label" ).html( "Code:" );
                           pt.val("");
                           $( "#login_error" ).html( "Check your email for an email code." ); }
                       else if ( resp == "wrong-code" ) {
                           pt.val("");
                           $( "#login_error" ).html( "That is the incorrect code." ); }
                       else if ( resp ) {
                           $( "#login_passwd_label" ).html( "Password:" );

                           et.val("");
                           pt.val("");

                           localStorage['email'] = e;
                           localStorage['passwd'] = p;

                           $( "#login" ).dialog( "close" );

                           initial_login(); }
                       else {
                           pt.val("");
                           $( "#login_error" ).html( "Incorrect password, please retry" ); }; } ); }
    $( "#login_passwd_text" ).keypress( function (e) {
        if (e.which == 13) { login_submit (); } } );
    $( "#login_button" ).click( function (e) { login_submit (); } );

    function menu_logout () {
        $("#logout").html( $('<a>', { text: "login",
                                      href: "javascript:void(0)",
                                      click: function () {
                                          $( "#login" ).dialog( "open" ); } } ) ); }
    function menu_loggedin ( curate_p ) {
        $("#logout").html("")
            .append( localStorage['email'],
                     ( curate_p ? [ " (", $('<a>', { text: "curator",
                                                     href: "javascript:void(0)",
                                                     click: function () {
                                                         $( "span.curate_link" ).show();
                                                         clear_terms();
                                                         search_terms[ "!:conflicts:" ] = true;
                                                         search_terms[ "ring:2" ] = true;
                                                         evaluate_search(); } } ),
                                    ")" ] : ""),
                     " | ",
                     $('<a>', { text: "upload",
                                href: "javascript:void(0)",
                                click: function () {
                                    console.log("XXX upload"); } } ),
                     " | ",
                     $('<a>', { text: "update",
                                href: "javascript:void(0)",
                                click: function () {
                                    console.log("XXX update"); } } ),
                     " | ",
                     $('<a>', { text: "logout",
                                href: "javascript:void(0)",
                                click: function () {
                                    localStorage['email'] = "";
                                    localStorage['passwd'] = "";

                                    menu_logout (); } } ) ); }

    function initial_login () {
        $.getJSON( dynamic_url("/jsonp/authenticate"),
                   { email: localStorage['email'], passwd: localStorage['passwd'], code: "" },
                   function( resp ) {
                       if ( $.isPlainObject(resp) ) {
                           menu_loggedin( resp['curation'] ); }
                       else {
                           menu_logout();
                           console.log( "login failed" ); } } ); }


    if ( localStorage['email'] && localStorage['passwd'] ) {
        initial_login();
    } else {
        menu_logout ();
    } });
