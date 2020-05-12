var path = require('path');

module.exports = function(grunt){

    // Configuration
    grunt.initConfig(getGruntConfig());

    // Load plugins
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('ditmer-embla');
    grunt.loadNpmTasks("grunt-ts");

    // Register tasks
    grunt.registerTask('run', ['ditmer_embla', 'ts', 'concat']);

    function getGruntConfig() {
        return {
            ditmer_embla: {
                build: {
                    options: {
                        emblaIconsOutputPath: path.resolve(__dirname + "/dist/icons"),
                        themeScssFilePath: path.resolve(__dirname + "/www/styles/embla-variables-theme.scss"),
                        emblaCssOutputPath: path.resolve(__dirname + "/dist/css"),
                    },
                }
            },
            concat: {
                js: {
                    src: ["dist/js/*.js"],
                    dest: 'dist/js/scripts.js'
                },
                css: {
                    src: ["dist/css/*.css"],
                    dest: 'dist/css/styles.css'
                }
            },
            ts: {
                default : {
                    src: ["**/*.ts", "!node_modules/**"],
                    dest: 'dist/js/'
                }
            },
            rootDir: "www/",
            getGruntConfigCallback: getGruntConfig
        }
    }
};