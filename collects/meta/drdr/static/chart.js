var path = ""
var data = null;
var sub_times = [];
var overall_times = [];
var chart_data = [];
var show_hide = {}
var options = { selection: { mode: "xy" },
                legend: { backgroundOpacity: 0,
                          position: "sw",
                          show: true,
                          noColumns : 1,
                          labelFormatter :
                          function(label, series) {
                              if (show_hide[label] === undefined)
                                  show_hide[label] = true;
                              var css = '';
                              if (!show_hide[label]) {
                                  css = 'style="font-style: italic"';
                              }
                              var v = '<div '+css+' onclick="legend_click(\''+label+'\')">' + label + '</div>';
                              return v;}},
                xaxes: [{min: null, max: null, label: 'push'}],
                yaxes: [{min: null, max: null, label: "time"},
                        {position: "right"}],
                grid: { clickable: true, hoverable : true }
              };

function addCommas(nStr) {
    var rgx = /(\d+)(\d{3})/;
    while (rgx.test(nStr)) {
        nStr = nStr.replace(rgx, '$1' + ',' + '$2');
    }
    return nStr;
}

// Number -> String
function format_ms(ms) {
    return addCommas(String(ms)) + " ms"
}

// Number -> String
function format_time(ms) {
    if (ms >= 300000)
        return Number(ms/60000).toFixed(2) + " m " + "("+ format_ms(ms)+")";
    if (ms >= 10000)
        return Number(ms/1000).toFixed(2) + " s" + "("+ format_ms(ms)+")";
    return format_ms(ms);
}

function legend_click(l) {
    show_hide[l] = !show_hide[l];
    show();
    serialize_opts(options);
}

var placeholder = $("#_chart");
var previousPoint = null;

function showTooltip(x, y, contents) {
    $('<div id="tooltip">' + contents + '</div>').css( {
        position: 'absolute',
        display: 'none',
        top: y + 5,
        left: x + 5,
        border: '1px solid #fdd',
        padding: '2px',
        'background-color': '#fee',
        opacity: 0.80
    }).appendTo("body").fadeIn(200);
}

function makeTooltip(item,path) {
    var x = item.datapoint[0];
    var y = item.datapoint[1].toFixed(2);
    showTooltip(item.pageX, item.pageY,
                item.series.label + ' at <a href="http://drdr.racket-lang.org/'
                + x + path + '">push ' + x + "</a>: "
                + format_time(y));
}
placeholder.bind("plotselected", handle_selection);

// is the tooltip shown b/c of a click?
var tooltip_clicked = false;

function remove_tooltip() {
    tooltip_clicked = false;
    $("#tooltip").remove();
}

function hover(event,pos,item) {
    if (tooltip_clicked) return;
    if (item) {
        // don't re-show the same tool-tip that's already shown
        if (previousPoint != item.dataIndex) {
            previousPoint = item.dataIndex;
            remove_tooltip();
            makeTooltip(item,path);
        }
    }
    else {
        remove_tooltip();
        previousPoint = null;
    }
}

function click(e,pos,item) {
    if (tooltip_clicked) {
        remove_tooltip();
        return;
    }
    if (!item) return;
    tooltip_clicked = true;
    // if we've already got the tooltip, just keep it around
    if (previousPoint != item.dataIndex) {
        $("#tooltip").remove();
        makeTooltip(item,path);
    }
}


// sort chart data based on the order of a[0], b[0]
function sorter(a,b) {
    if (a[0] < b[0]) return -1;
    if (a[0] > b[0]) return 1;
    return 0;
}



function load_data(d) {
    chart_data = [];
    overall_times = [];
    sub_times = [];
    pdata = []
    data = d;
    reset_chart();
    pdata = data && JSON.parse(data);

    var max_overall = 0;
    var max_sub = 0;

    // build the timing data arrays
    for (var i = 0; i < pdata.length; i++) {
        overall_times.push([pdata[i][0], pdata[i][1]]);
        max_overall = Math.max(max_overall, pdata[i][1]);
        if (pdata[i][2].length != 0) {
            for (var j = 0; j < pdata[i][2].length; j++) {
                sub_times[j] = sub_times[j] || [];
                sub_times[j].push([pdata[i][0],pdata[i][2][j][0]]);
                max_sub = Math.max(max_sub, pdata[i][2][j][0]);
            }
        }
    };

    // is there a significant difference between the overall times
    // and the internal timings?

    var ya = 1;
    if ((max_overall > (5 * max_sub)) || ((max_overall * 5) < max_sub))
        ya = 2;

    // put the data into the chart format
    chart_data.push({data: overall_times.sort(sorter), label: "Overall Time", color: "#804040"});
    for(var i = 0; i < sub_times.length; i++) {
        var n = (sub_times[i].length/overall_times.length);
        chart_data.push({data: sub_times[i].sort(sorter), label: "Timer "+ (i+1),
                         lines: { show: (.9<n) },
                         points: { show: !(.9<n) },
                         yaxis: ya});
    }
    cur_options.legend.noColumns = Math.max(1,Math.round(chart_data.length / 10));
}

function get_data(_path) {
    if (_path[0] != '/')
        _path = '/' + _path;
    path = _path;
    $.ajax({url: 'http://drdr.racket-lang.org/json/timing'+path,
            beforeSend: function(xhr) {
                xhr.overrideMimeType( 'text/plain; charset=x-user-defined' );
            },
            success: function(d) { load_data(d); show(); }});
}


function show() {
    for(var i = 0; i < chart_data.length; i++) {
        if (show_hide[chart_data[i].label] === false) {
            if (!chart_data[i].saved)
                chart_data[i].saved = chart_data[i].data
            chart_data[i].data = [];
        }
        else if (chart_data[i].data.length === 0 && chart_data[i].saved !== null) {
            chart_data[i].data = chart_data[i].saved;
            chart_data[i].saved = null;
        }
    }
    $.plot(placeholder, chart_data, cur_options);
}

function serialize_opts(options) {
    var o = {};
    if (options.xaxes[0].min)
        o.xmin = options.xaxes[0].min;
    if (options.xaxes[0].max)
        o.xmax = options.xaxes[0].max;
    if (options.yaxes[0].min)
        o.ymin = options.yaxes[0].min;
    if (options.yaxes[0].max)
        o.ymax = options.yaxes[0].max;
    window.location.hash = "#" + (JSON.stringify([o,show_hide]));
}

function handle_selection(event, ranges) {
    cur_options = $.extend(true, {}, cur_options, {
        yaxes: [ { min: ranges.yaxis.from, max: ranges.yaxis.to },cur_options.yaxes[1]],
        xaxes: [ { min: ranges.xaxis.from, max: ranges.xaxis.to } ]});
    serialize_opts(cur_options);
    show();

}

function set_legend(new_val) {
  cur_options = $.extend(true,{},cur_options, {legend: {show: new_val}});
  show();
  if (new_val)
      $("#setlegend").text("Hide Legend")
  else
      $("#setlegend").text("Show Legend")
}

function reset_chart() {
    cur_options = options; show_hide = {}; show();
}

placeholder.bind("plothover", hover);
placeholder.bind("plotclick", click);

var opts = {xmin : null, ymin: null, xmax: null, ymax : null};

var cur_options = options;

try {
    opts = JSON.parse(window.location.hash.substring(1));
} catch(e) {}

if (opts && opts.length == 2) {
    cur_options.xaxes[0].min = opts[0].xmin;
    cur_options.xaxes[0].max = opts[0].xmax;
    cur_options.yaxes[0].min = opts[0].ymin;
    cur_options.yaxes[0].max = opts[0].ymax;
    for(i in opts[1]) {
        console.log(i,opts[1][i]);
        show_hide[i] = opts[1][i];
    }
}
