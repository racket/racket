// xxx curate
// xxx rss
// xxx re-login
// xxx logout
// xxx what user am i
// xxx manage packages

$( document ).ready(function() {
    var search_terms = { "!main-tests": true, "!main-distribution": true };

    function addfilterlink ( text, term ) {
        return [$('<a>', { text: text,
                           href: "javascript:void(0)",
                           click: function () {
                               search_terms[term] = true;
                               evaluate_search();
                           } } ),
                " "
               ];
    };
    function removefilterlink ( text, term ) {
        return [$('<a>', { text: text,
                           href: "javascript:void(0)",
                           click: function () {
                               delete search_terms[term];
                               evaluate_search();
                           } } ),
                " "
               ];
    };

    function evaluate_search () {
        $.each( $('#packages_table tr'), function (key, dom) {
            var value = $(dom).data("obj");
            var show = true;
            var vterms = value['search-terms'];

            $.each(search_terms,
                   function ( term, termv ) {
                       if ( term.charAt(0) == "!" ) {
                           if ( vterms[term.substring(1)] ) {
                               show = false;
                           }
                       } else {
                           if ( ! vterms[term] ) {
                               show = false;
                           }
                       }
                   });

            if ( show ) {
                $(dom).show();
            } else {
                $(dom).hide();
            }

        });

        // xxx handle search button

        // xxx update menu available
        $("#search_menu").html("").append( $.map( search_terms, function ( term, i ) {
            return removefilterlink ( i, i );
        } ) );

        $("#packages_table tr:visible:even").removeClass("even");
        $("#packages_table tr:visible:odd").addClass("even");
    };

    function object_keys ( o ) {
        var names = [];
        $.each(o, function(key, value) { names.push(key) });
        return names;
    }

    $.getJSON( "/pkgs-all.json", function( resp ) {
        var names = object_keys(resp);
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
});
