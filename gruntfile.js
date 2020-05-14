/// <binding ProjectOpened='development' />
"use strict";

module.exports = function(grunt) {

    grunt.file.defaultEncoding = "utf8";
    grunt.file.preserveBOM = true;

    var path = require("path");
    var nodesass = require("node-sass");

    grunt.initConfig(getGruntConfig());

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-cache-bust");
    grunt.loadNpmTasks("grunt-webpack");
    grunt.loadNpmTasks("grunt-notify");
    grunt.loadNpmTasks("grunt-text-replace");
    grunt.loadNpmTasks("grunt-concurrent");
    grunt.loadNpmTasks("grunt-sass");
    grunt.loadNpmTasks("ditmer-embla");
    
    grunt.registerTask("production-overwrite-assets", "Overwrite webpack assets in CSHTML files.", function () {

        var jsReplacements = [];
        var jsAssets = grunt.config.getRaw('webpackStats').assetsByChunkName;
        for (var jsName in jsAssets) {
            if (jsAssets.hasOwnProperty(jsName)) {
                if (jsName !== "common.js") {
                    jsReplacements.push(
                        {
                            from: "js/" + jsName + ".js",
                            to: "js/" + jsAssets[jsName]
                        });
                }
            }
        }

        grunt.config('replace.jsreferences.replacements', jsReplacements);

        var cssReplacements = [];
        var cssAssets = grunt.file.readJSON("css-assets.json");
        for (var cssName in cssAssets) {
            cssReplacements.push(
                {
                    from: cssName,
                    to: cssAssets[cssName]
                });
        }
        grunt.config('replace.cssreferences.replacements', cssReplacements);


        grunt.task.run(['replace']);
    });

    grunt.registerTask("development", ["clean", "copy", "ditmer_embla", "sass", "concurrent:development"]);
    grunt.registerTask("production",
        [
            "clean", "copy", "ditmer_embla", "sass", "cacheBust:css", "webpack:production",
            "production-overwrite-assets"
        ]);

    grunt.registerTask("d", ["development"]);
    grunt.registerTask("dev", ["development"]);

    function concurrent() {
        return {
            development: ["webpack:development", "watch:styles"],
            options: {
                logConcurrentOutput: true
            }
        };
    }

    function clean() {
        return {
            dist: {
                src: "dist"
            }
        };
    }

    function sass() {

        var path = require("path");

        return {
            options: {
                implementation: nodesass,
                sourceMap: true,
                includePaths: [
                    path.join(process.cwd(), "node_modules")
                ]
            },
            "site": {
                files: [
                    {
                        expand: true,
                        cwd: path.resolve(__dirname + "/www/styles/"),
                        src: [
                            "shared.scss"
                        ],
                        dest: path.join(__dirname, "dist/css"),
                        ext: '.css'
                    }
                ]
            },
            "print-tilbud": {
                files: [
                   {
                        expand: true,
                        cwd: path.resolve(__dirname + "/www/styles/"),
                        src: [
                          "print-tilbud-flow.scss"
                        ],
                        dest: path.join(__dirname, "dist/css"),
                        ext: '.css'
                    }
                ]
            },
            "print-ordre": {
                files: [
                    {
                        expand: true,
                        cwd: path.resolve(__dirname + "/www/styles/"),
                        src: [
                            "print-ordre-flow.scss"
                        ],
                        dest: path.join(__dirname, "dist/css"),
                        ext: '.css'
                    }
                ]
            }
        };
    }


    function copy() {
        return {
            fonts: {
                expand: true,
                flatten: true,
                src: [
                    "node_modules/bootstrap/dist/fonts/*",
                    "node_modules/font-awesome/fonts/*"
                ],
                dest: "dist/fonts"
            },
            "bootstrap-fonts": {
                expand: true,
                flatten: true,
                src: [
                    "node_modules/bootstrap/dist/fonts/*"
                ],
                dest: "dist/fonts/bootstrap"
            },
            "ckeditor": {
                expand: true,
                flatten: false,
                src: [
                    "**"
                ],
                dest: "dist/js/ckeditor",
                cwd: "www/scripts/libs/ckeditor/"
            }
        };
    }

    function replace() { //https://github.com/yoniholmes/grunt-text-replace instead of grunt-replace
        return {
            jsreferences: {
                src: "Views/**/*.cshtml",
                overwrite: true,
                replacements: []
            },
            cssreferences: {
                src: "Views/**/*.cshtml",
                overwrite: true,
                replacements: []
            }
        };
    }

    function watch() {
        return {
            styles: {
                files: ["www/styles/**/*"],
                tasks: [
                    "sass",
                    "notify:watch-styles-success"
                ],
                options: {
                    reload: true
                }
            }
        };
    }

    function notify() {
        return {
            "watch-styles-success": {
                options: {
                    title: "Styles",
                    message: "Build successful"
                }
            }
        };
    }

    function webpack() {
        var webpack = require("webpack");
        var WebpackAssetsPlugin = require("assets-webpack-plugin");
        var WebpackNotifierPlugin = require('webpack-notifier');

        var path = require("path");

        //See https://github.com/webpack/css-loader/issues/145
        require("es6-promise").polyfill();

        return {
            options: {
                //root: path.resolve(__dirname),
                cache: false,


                stats: {
                    colors: false,
                    modules: false,
                    reasons: true,
                    errorDetails: true
                },

                storeStatsTo: "webpackStats",

                progress: false,

                // Enable sourcemaps for debugging webpack's output.
                devtool: "source-map",

                resolve: {
                    alias: {
                        "jquery": path.join(__dirname, "node_modules/jQuery")
                    },
                    modules: [
                        path.join(__dirname, "www/scripts"),
                        path.join(__dirname, "node_modules")
                    ],
                    extensions: [".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".css"]
                },

                module: {
                    rules: [
                        {
                            test: /\.css$/,
                            use: [
                                "style-loader",
                                "css-loader"
                            ]
                        },
                        {
                            test: /\.tsx?$/,
                            use: [
                                "ts-loader"
                            ]
                        },
                        {
                            test: /\.js$/,
                            enforce: "pre",
                            use: [
                                "source-map-loader"
                            ]
                        }
                    ]
                },

                plugins: [
                    new webpack.optimize.CommonsChunkPlugin({
                        name: "common",
                        filename: "common.js",
                        minChunks: 10 // Hvor mange filer den skal være genbrugt i, før den flyttes til common.js
                    }),
                    new webpack.ProvidePlugin({
                        jQuery: "jquery",
                        $: "jquery",
                        Promise: "es6-promise", //es6 promise polyfill for IE
                        fetch: "imports-loader?this=>global!exports-loader?global.fetch!whatwg-fetch" //es6 fetch polyfill for IE
                    }),
                    new WebpackAssetsPlugin({
                        filename: "webpack-assets.json",
                        path: path.resolve(__dirname),
                        prettyPrint: true
                    }),
                    new WebpackNotifierPlugin({ alwaysNotify: true })
                ],

                entry: {
                    svgxuse: "./www/scripts/libs/svgxuse/svgxuse.js", //Fix for IE svg <use> tags
                    shared: "./www/scripts/shared.ts",
                },

                output: {
                    filename: "[name].js",
                    path: path.join(__dirname, "/dist/js")
                }
            },

            build: {
                plugins: [
                    new webpack.DefinePlugin({
                        __DEV__: 'true',
                        __TEST__: 'false',
                        __PRODUCTION__: 'false'
                    })
                ],
                devtool: "source-map"
            },

            development: {
                watch: true, // use webpacks watcher
                // You need to keep the grunt process alive

                keepalive: true, // don"t finish the grunt task
                // Use this in combination with the watch option

                failOnError: false, //don't break the watch task on error

                cache: true,

                plugins: [
                    new webpack.DefinePlugin({
                        __DEV__: 'true',
                        __TEST__: 'false',
                        __PRODUCTION__: 'false'
                    })
                ],
                devtool: "cheap-module-source-map"
            },

            production: {
                plugins: [
                    new webpack.optimize.UglifyJsPlugin({
                        compress: {
                            warnings: false,
                            screw_ie8: true,
                            conditionals: true,
                            unused: true,
                            comparisons: true,
                            sequences: true,
                            dead_code: true,
                            evaluate: true,
                            if_return: true,
                            join_vars: true
                        },
                        output: {
                            comments: false
                        }
                    }),
                    new webpack.HashedModuleIdsPlugin(),
                    new webpack.DefinePlugin({
                        __DEV__: 'false',
                        __TEST__: 'false',
                        __PRODUCTION__: 'true',
                        "process.env.NODE_ENV": JSON.stringify("production")
                    }),
                    new WebpackAssetsPlugin()
                ],
                output: {
                    path: path.join(__dirname, "dist/js/"),
                    filename: "[name].[chunkhash].js"
                },
                devtool: "source-map"
            }
        };
    }

    function cssCacheBust() {

        return {
            options: {
                assets: [
                    "dist/css/*.css"
                ],
                jsonOutput: true,
                jsonOutputFilename: "css-assets.json"
            },
            css: {
                options: {
                    deleteOriginals: true,
                    createCopies: true
                },
                src: []
            }
        };

    }

    function getGruntConfig() {
        return {
            pkg: grunt.file.readJSON("package.json"),
            "clean": clean(),
            "sass": sass(),
            "copy": copy(),
            "watch": watch(),
            "webpack": webpack(),
            "cacheBust": cssCacheBust(),
            "replace": replace(),
            "concurrent": concurrent(),
            "notify": notify(),
            "ditmer_embla": {
                build: {
                    options: {
                        emblaIllustrationsOutputPath: path.resolve(__dirname + "/dist/illustrations"),
                        emblaIconsOutputPath: path.resolve(__dirname + "/dist/icons")
                    }
                }
            }
        };
    }
};

// ReSharper restore FunctionsUsedBeforeDeclared
