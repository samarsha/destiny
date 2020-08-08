const CopyWebpackPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const path = require("path");
const webpack = require("webpack");

const project = "./src/Client/Client.fsproj";

const style = "./src/Client/Style/style.sass";

const assetsDir = "src/Client/assets";

const htmlWebpackPlugin = new HtmlWebpackPlugin({
  filename: "index.html",
  template: "src/Client/index.html"
});

const babel = {
  presets: [
    [ "@babel/preset-env", { modules: false, useBuiltIns: "usage", corejs: 3 } ]
  ]
};

module.exports = env => ({
  mode: env.production ? "production" : "development",

  entry: env.production
    // Combine code and style because MiniCssExtractPlugin will separate them.
    ? { app: [ project, style ] }
    // Separate code and style for faster HMR.
    : { app: [ project ], style: [ style ] },

  output: {
    path: path.resolve("src/Client/deploy"),
    filename: env.production ? "[name].[contenthash].js" : "[name].js",
    publicPath: "/"
  },

  optimization: {
    splitChunks: { chunks: "all" }
  },

  plugins: env.production 
    ? [ htmlWebpackPlugin,
        new MiniCssExtractPlugin({ filename: "style.[contenthash].css" }),
        new CopyWebpackPlugin({ patterns: [ { from: assetsDir } ] }) ]
    : [ htmlWebpackPlugin,
        new webpack.HotModuleReplacementPlugin() ],
  
  devtool: env.production ? "source-map" : "eval-source-map",

  devServer: {
    host: "0.0.0.0",
    port: 8080,
    publicPath: "/",
    contentBase: assetsDir,
    hot: true,
    inline: true,
    historyApiFallback: { index: "/" },
    proxy: {
      "/socket": {
        target: "http://localhost:8085",
        ws: true
      }
    }
  },

  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: { babel }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: babel
        },
      },
      {
        test: /\.(sass|scss|css)$/,
        use: [
          env.production ? MiniCssExtractPlugin.loader : "style-loader",
          "css-loader",
          "resolve-url-loader",
          {
            loader: "sass-loader",
            options: { implementation: require("sass") }
          }
        ],
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
        use: [ "file-loader" ]
      }
    ]
  }
});
