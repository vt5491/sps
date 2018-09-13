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
  // cell.status = parms.status || "";
  // cell.row = parms.row || -1;
  cell.row = "row" in parms ? parms.row : -1;
  cell.col = "col" in parms ? parms.col : -1;
  // cell.col = parms.col || -1;
  // return { val: 1, status: ""}
  return cell;
}

exports.printGrid = function (g) {
  // process.stdout.write("hello from printGrid\n");
  // process.stdout.write("printGrid: g.length=" + g.length)
  var closedCells = 0;
  var openCells = 0;
  process.stdout.write("\n  012 345 678");
  for (var i = 0; i < g.length; i++) {
    if (i % 3 == 0) {
      process.stdout.write("\n ------------");
    }
    process.stdout.write("\n" + i);
    var row = g[i];
    for (var j=0; j < row.length; j++) {
      if (j % 3 == 0) {
        process.stdout.write("|");
      }

      if (row[j].val === 0) {
        process.stdout.write("-");
        openCells++;
      }
      else{
        process.stdout.write( row[j].val.toString());
        closedCells++;
      }
    }
  }
  // process.stdout.write("\n ------------\n");
  process.stdout.write("\n\n");

  return "closed=" + closedCells + ", open=" + openCells;
}

exports.printMsg = function (msg) {
  process.stdout.write(msg + "\n");

  return ""
}

// sps.showAbc("abcd");
// process.stdout.write(sps.doIt(1));
// sps.showRow([1,2,3]);
// process.stdout.write(" hola from Sps.js\n")
