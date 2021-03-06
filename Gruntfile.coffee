module.exports = (grunt) ->
  grunt.initConfig
    coffee:
      compile:
        cwd: "client/coffee"
        src: ["**/*.coffee"]
        dest: "public/js/modules/"
        ext: ".js"
        expand: true
        flatten: false
        bare: false

    jade:
      compile:
        files:
          "public/index.html": "client/jade/index.jade"

    stylus:
      compile:
        files:
          "public/css/styles.css": "client/stylus/*.styl"

        options:
          compress: true

    shell:
      server:
        options:
          async: true
          failOnError: true
          stdout: true
          stderr: true
        command: 'runghc -iserver -package-db=`echo .cabal-sandbox/*.d` Main'


    watch:
      coffee:
        files: ["client/coffee/**/*.coffee"]
        tasks: ["coffee"]

      stylus:
        files: ["client/stylus/*.styl"]
        tasks: ["stylus"]

      jade:
        files: ["client/jade/*.jade"]
        tasks: ["jade"]

  grunt.loadNpmTasks "grunt-contrib-coffee"
  grunt.loadNpmTasks "grunt-contrib-jade"
  grunt.loadNpmTasks "grunt-contrib-stylus"
  grunt.loadNpmTasks "grunt-contrib-watch"
  grunt.loadNpmTasks "grunt-shell-spawn"

  grunt.registerTask "default", ["coffee", "jade", "stylus", "shell:server", "watch"]
  grunt.registerTask "build", ["coffee", "jade", "stylus"]
