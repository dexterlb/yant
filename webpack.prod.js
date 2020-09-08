var merge  = require('webpack-merge');
var path   = require('path');
var common = require('./webpack.common.js');

module.exports = merge.smart(common, {
  mode: 'production',
  module: {
    rules: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader',
        options: {
          optimize: true
        }
      },
    ],

    noParse: /\.elm$/,
  },
});
