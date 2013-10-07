// xxx curate
// xxx rss
// xxx re-login
// xxx logout
// xxx what user am i
// xxx manage packages

$( document ).ready(function() {
    var search_terms = [ "!main-tests", "!main-distribution" ];

    function addfilterlink ( text, term ) {
        return [$('<a>', { text: text,
                           href: "javascript:void(0)",
                           click: function () {
                               search_terms.push(term);
                               evaluate_search();
                           } } ),
                " "
               ];
    };
    function removefilterlink ( text, term ) {
        return [$('<a>', { text: text,
                           href: "javascript:void(0)",
                           click: function () {
                               search_terms = $.grep( search_terms, function (v) { return v != term } );
                               evaluate_search();
                           } } ),
                " "
               ];
    };

    $.getJSON( "/pkgs-all.json", function( resp ) {
        var names = [];
        $.each(resp, function(key, value) { names.push(key) });
        var snames = names.sort(function(a,b) {
            return ((a < b) ? -1 : ((a > b) ? 1 : 0));
        })

        var now = new Date().getTime() / 1000;

        $.each( snames,
                function (name_i) {
                    var name = snames[name_i];
                    var value = resp[name];

                    $('<tr>',
                      { class: ((now - (60*60*24*2)) < value['last-updated'] ? "recent" : "old") }).
                        data( "obj", value).append(
                            $('<td>').html( $('<a>', { text: value['name'],
                                                       href: "javascript:void(0)",
                                                       click: function () {
                                                           // XXX open up a subwindow
                                                           console.log(value);
                                                       } } ) ),
                            $('<td>').append( $.map( value['authors'], function ( author, i ) {
                                return addfilterlink ( author, "author:" + author );
                            } ) ),
                            $('<td>').text( value['description'] ),
                            $('<td>').append( $.map( value['tags'], function ( tag, i ) {
                                return addfilterlink ( tag, tag );
                            } ) )).appendTo('#packages_table');
                });

        evaluate_search();
    });

    function evaluate_search_term( value, term ) {
        if ( term == ":error:") {
            return value['checksum-error'];
        } else if ( term == ":no-tag:") {
            return value['tags'].length == 0;
        } else if ( term.substring(0, 5) == "ring:") {
            return value['ring'] == term.substring(5);
        } else if ( term.substring(0, 7) == "author:") {
            return ($.inArray( term.substring(7), value['authors'] ) != -1);
        } else if ( term.charAt(0) == "!" ) {
            return ! evaluate_search_term( value, term.substring(1) );
        } else if ( $.inArray( term, value['tags']) != -1 ) {
            return true;
        } else {
            return false;
        }
    }

    function evaluate_search () {
        $.each( $('#packages_table tr'), function (key, dom) {
            var value = $(dom).data("obj");
            var show = true;

            for (termi in search_terms) {
                var term = search_terms[termi];
                if ( ! evaluate_search_term( value, term ) ) {
                    show = false;
                }
            }

            if ( show ) {
                $(dom).show();
            } else {
                $(dom).hide();
            }

        });
        
        // xxx handle search button

        // xxx update menu available
        $("#search_menu").html("").append( $.map( search_terms, function ( term, i ) {
            return removefilterlink ( term, term );
        } ) );

        $("#packages_table tr:visible:even").removeClass("even");
        $("#packages_table tr:visible:odd").addClass("even");
    };

});
