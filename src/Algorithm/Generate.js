"use strict";

var sudoku = require('sudoku');

exports.generateSudokuImpl = (nothing, just) => function () {
    return sudoku.makepuzzle().map(n => n ? just(n) : nothing);
}