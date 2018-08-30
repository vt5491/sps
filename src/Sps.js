"use strict"

var fs = require('fs');

exports.readPuzzleFile = function (fn) {
  var contents = fs.readFileSync( fn, 'utf8');

  return contents;
}
