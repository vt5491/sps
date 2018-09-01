"use strict"

var fs = require('fs');
// var sps = require('../output/Sps');
// var sps = require('./output/Sps');
// following works, as long as '/output/*' is in $SPS/node_modules
// var sps = require('Sps');

exports.readPuzzleFile = function (fn) {
  var contents = fs.readFileSync( fn, 'utf8');

  return contents;
}

exports.cellDefault = function (parms) {
  // Sps.foo("john")("smith");
  // sps.showAbc("a");
  // return 0;
  // console.log("cl: now in cellDefault");
  // process.stdout.write(`cellDefault: parms.val=${parms.val}`);
  // process.stdout.write("cellDefault: parms.val= " + parms.val); // works
  var cell = {};

  // if (parms.val) {
  //   cell.val = parms.val
  // }
  // if (parms.status) {
  //   cell.status = parms.status
  // }
  cell.val = parms.val || 0;
  cell.status = parms.status || "";
  // return { val: 1, status: ""}
  return cell;
}

// sps.showAbc("abcd");
// process.stdout.write(sps.doIt(1));
// sps.showRow([1,2,3]);
// process.stdout.write(" hola from Sps.js\n")
