module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],

    clean: {
      tests: ["tmp"],
      lib: ["js", "externs"]
    },

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],

    psc: {
      tests: {
        options: {
          module: ["Main"],
          main: true
        },
        src: [
          "tests/Tests.purs",
          "<%=libFiles%>"
        ],
        dest: "tmp/tests.js"
      }
    },

    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    },

    docgen: {
      types: {
        src: "src/**/*.purs",
        dest: "TYPES.md"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test", ["clean:tests", "psc:tests", "execute:tests"]);
  grunt.registerTask("doc", ["docgen:types"]);
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("default", ["make"]);
};
