const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  mode: "production",
  entry: {
    index: "./site/bootstrap.tsx",
  },
  output: {
    path: dist,
    filename: "bundle.js",
  },
  experiments: {
    asyncWebAssembly: true,
  },
  performance: {
    hints: false,
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        include: path.resolve(__dirname, "site"),
        use: [MiniCssExtractPlugin.loader, "css-loader", "postcss-loader"],
      },
    ],
  },
  resolve: {
    extensions: [".tsx", ".ts", ".js"],
  },
  devServer: {
    static: {
      directory: path.resolve(__dirname, "static"),
    },
  },
  plugins: [
    new MiniCssExtractPlugin({ filename: "bundle.css" }),
    new CopyPlugin({ patterns: [path.resolve(__dirname, "site/static")] }),
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "wasm"),
      outDir: path.resolve(__dirname, "pkg"),
    }),
  ],
};
