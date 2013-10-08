// xxx display curation if allowed
// xxx logout
// xxx what user am i
// xxx upload
// xxx update

$( document ).ready(function() {
    var search_terms = { };

    function parse_hash ( h ) {
        while ( h != "" ) {
            if ( h.charAt(0) == "(" ) {
                var end = 1;
                while ( h.charAt(end) != ")" ) {
                    end++;
                    if ( ! h.charAt(end) ) { break; } }
                search_terms[ h.substring(1, end) ] = true;
                h = h.substring(end+1); }
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
                $(dom).hide(); } });

        $.each(search_terms,
               function ( term, termv ) {
                   if ( term.charAt(0) == "!" ) {
                       shown_terms[ term.substring(1) ] = -2; }
                   else {
                       shown_terms[ term ] = -1; } });

        var shown_terms_keys = object_keys(shown_terms);
        var shown_terms_skeys = shown_terms_keys.sort(function(a,b) {
            var va = shown_terms[a];
            var vb = shown_terms[b];
            if ( va < 0 && vb < 0 ) { return ((a < b) ? -1 : ((a > b) ? 1 : 0)); }
            else if ( va >= 0 && vb >= 0 ) { return ((va < vb) ? -1 : ((va > vb) ? 1 : 0)); }
            else if ( va < 0 ) { return -1; }
            else if ( vb < 0 ) { return 1; } });

        change_hash( "" );
        $("#search_menu").html("").append( $.map( shown_terms_skeys, function ( term, i ) {
            if ( shown_terms[term] < 0 ) {
                if ( shown_terms[term] == -1 ) {
                    change_hash( window.location.hash + "(" + term + ")" );
                    return changefilterlink ( term, term, "!" + term, "active" ); }
                else {
                    change_hash( window.location.hash + "(" + "!" + term + ")" );
                    return removefilterlink ( term, "!" + term, "inactive" ); } }
            else {
                return addfilterlink ( term, term, "possible" ); } } ) );

        $("#packages_table tr:visible:even").removeClass("even");
        $("#packages_table tr:visible:odd").addClass("even"); };

    function object_keys ( o ) {
        var names = [];
        $.each(o, function(key, value) { names.push(key) });
        return names; }

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
                            $('<td>').html( $('<a>', { text: value['name'],
                                                       href: "javascript:void(0)",
                                                       click: function () {
                                                           // XXX open up a subwindow
                                                           console.log(value); } } ) ),
                            $('<td>').append( $.map( value['authors'], function ( author, i ) {
                                return addfilterlink ( author, "author:" + author, "possible" ); } ) ),
                            $('<td>').text( value['description'] ),
                            $('<td>').append( $.map( value['tags'], function ( tag, i ) {
                                return addfilterlink ( tag, tag, "possible" ); } ) ))
                        .appendTo('#packages_table'); });

        evaluate_search(); }); });
