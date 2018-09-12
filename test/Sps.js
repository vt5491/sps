"use strict"

// var fs = require('fs');

exports.debugIt = function (n) {
  debugger;
}

exports.printObj = function (o) {
  process.stdout.write("hello from printObj");
}

exports.printArray = function (a) {
  process.stdout.write("hello from printArray\n");
  process.stdout.write("a.length=" + a.length + "\n");
  for (var i = 0; i < a.length; i++) {
    process.stdout.write("row=" + a[i].row + "\n")
  }

  return "done"
}
