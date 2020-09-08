const   fs                            = require('fs');
const   path                          = require('path');
const { CleanWebpackPlugin }          = require('clean-webpack-plugin');
const   HtmlWebpackPlugin             = require('html-webpack-plugin');
const   HtmlWebpackInlineSourcePlugin = require('html-webpack-inline-source-plugin');

function base64_encode(file) {
    var bitmap = fs.readFileSync(file);
    return new Buffer(bitmap).toString('base64');
}

module.exports = {
  entry: {
    app: [
      './src/index.ts'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },

  plugins: [
    new HtmlWebpackPlugin({
      inlineSource: '.js$',
      template: 'src/index.ejs',
      templateParameters: (compilation, assets, assetTags, options) => {
        return {
          compilation,
          webpackConfig: compilation.options,
          htmlWebpackPlugin: {
            tags: assetTags,
            files: assets,
            options
          },
          'favicon': 'data:image/png;base64,' + base64_encode('./src/images/favicon.png'),
        };
      },
    }),
    new HtmlWebpackInlineSourcePlugin(),
    new CleanWebpackPlugin({
      cleanAfterEveryBuildPatterns: ['app.js'],
      verbose: true,
      protectWebpackAssets: false,
    }),
  ],

  module: {
    rules: [
      {
        test: /\.(scss)$/,
        use: [
          'style-loader',
          'css-loader',
          'sass-loader'
        ]
      },
      {
        test: /\.(css)$/,
        use: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'url-loader?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader?verbose=true',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader',
      },
    ],

    noParse: /\.elm$/,
  },
};
