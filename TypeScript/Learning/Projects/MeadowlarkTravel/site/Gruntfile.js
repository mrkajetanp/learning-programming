module.exports = function(grunt) {
  // load plugins
  [
    'grunt-cafe-mocha',
    'grunt-exec',
    'grunt-tslint',
    'grunt-nodemon',
    'grunt-ts',
    'grunt-contrib-jshint',
    'grunt-contrib-watch'
  ].forEach(function(task) {
    grunt.loadNpmTasks(task);
  });

  // configure plugins
  grunt.initConfig({
    cafemocha: {
      all: { src: 'qa/tests-*.js', options: { ui: 'tdd' }, }
    },
    tslint: {
      options: {
        force: false
      },
      files: {
        src: ['meadowlark.ts', 'public/ts/**/*.ts', 'lib/**/*.ts',
        'qa/**/*.ts', 'public/qa/**/*.ts']
      }
    },
    jshint: {
      options: {
        esversion: 6
      },
      all: ['Gruntfile.js', 'qa/tests-crosspage.js', 'public/qa/**/*.js']
    },
    ts: {
      default: {
        src: ["*.ts", "lib/**/*.ts", "qa/**/*.ts"]
      }
    },
    watch: {
      scripts: {
        files: ["*.ts", "lib/**/*.ts", "qa/**/*.ts"],
        tasks: ["ts"],
        options: {
          spawn: false,
        }
      },
    },
    exec: {
      linkchecker: { cmd: 'linkchecker http://localhost:8088 --check-extern' }
    },
    nodemon: {
      dev: {
        script: 'meadowlark.js'
      }
    },
  });

  grunt.registerTask("default", ['cafemocha','tslint','jshint','exec']);
};
