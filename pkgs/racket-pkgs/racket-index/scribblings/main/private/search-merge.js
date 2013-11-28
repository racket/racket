
// Merge user and main search arrays
{
  var orig_span_length = plt_span_classes.length;

  plt_span_classes = plt_span_classes.concat(plt_user_span_classes);

  var rev_map = [], user_rev_map = [];
  for (name in plt_manual_ptrs)
    rev_map[plt_manual_ptrs[name]] = name;
  for (name in plt_user_manual_ptrs)
    user_rev_map[plt_manual_ptrs[name]] = name;

  // Convert a span by adjusting the span-index offset

  function convert_span(x) {
    if (typeof x == "string") {
      return x;
    } else if ((x.length == 2) && (typeof(x[0]) == "number")) {
      return [x[0] + orig_span_length, convert_span(x[1])];
    } else {
      var res = [];
      for (var i=0; i<x.length; i++)
        res[i] = convert_span(x[i]);
      return res;
    }
  }

  function convert(line) {
    return [line[0], line[1], convert_span(line[2]), line[3]];
  }

  var i = 0, j = 0;
  var ilen = plt_search_data.length;
  var jlen = plt_user_search_data.length;
  var result = []
  while ((i < ilen) || (j < jlen)) {
    if (j >= jlen) {
      if (rev_map[i]) plt_manual_ptrs[rev_map[i]] = result.length;
      result.push(plt_search_data[i]);
      i++;
    } else if (i >= ilen) {
      if (user_rev_map[j]) plt_manual_ptrs[user_rev_map[j]] = result.length;
      result.push(convert(plt_user_search_data[j]));
      j++;
    } else if (plt_search_data[i][0] < plt_user_search_data[j][0]) {
      if (rev_map[i]) plt_manual_ptrs[rev_map[i]] = result.length;
      result.push(plt_search_data[i]);
      i++;
    } else {
      if (user_rev_map[j]) plt_manual_ptrs[user_rev_map[j]] = result.length;
      result.push(convert(plt_user_search_data[j]));
      j++;
    }
  }
  plt_search_data = result;
}
