export default {
  optimize: {
    bundle: true,
    minify: true,
    target: "es2020",
  },
  mount: {
    "site/public": { url: "/", static: true },
    site: { url: "/dist" },
  },
  plugins: [
    "@snowpack/plugin-typescript",
    "@snowpack/plugin-postcss",
    "@snowpack/plugin-babel",
    [
      "@emily-curry/snowpack-plugin-wasm-pack",
      {
        projectPath: "./wasm",
        outDir: "../pkg",
        watch: false,
        logLevel: "error",
      },
    ],
  ],
  devOptions: {
    tailwindConfig: "./tailwind.config.js",
  },
  packageOptions: {
    knownEntrypoints: ["react", "react/jsx-runtime"],
  },
  alias: {
    "wasm-compiler": "./pkg",
  },
};
