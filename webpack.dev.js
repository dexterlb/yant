var merge  = require('webpack-merge');
var path   = require('path');
var common = require('./webpack.common.js');

module.exports = merge.smart(common, {
  mode: 'development',

  devtool: 'inline-source-map',

  devServer: {
    host: '0.0.0.0',
    port: '8082',
    inline: false,
    hot: false,
    stats: { colors: true },
    proxy: {
      '/ws': {
        target: 'ws://localhost:1880',
        ws: true
      }
    },
    disableHostCheck: true,
  },
});
