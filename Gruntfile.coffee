# standard grunt init
module.exports = (grunt) ->
  grunt.initConfig
    # read our reqs from package.json
    pkg: grunt.file.readJSON "package.json"
    # clean up our directories after generating files
    clean:
      build:
        src: [
          "dist/doc" ]
    # shell tasks to automate some haskell / doc / test
    shell:
      updatenpm:
        command: 'npm install'
        options:
          stdout: true
          failOnError: true
      cabalprepare:
        command: 'cabal install --enable-tests --only-dependencies'
        options:
          stdout: true
          failOnError: true
      cabalbuild:
        command: 'cabal configure --enable-tests && cabal build'
        options:
          stdout: true
          failOnError: true
      cabaltest:
        command: 'cabal configure --enable-tests && cabal test'
        options:
          stdout: true
          failOnError: true
      cabalinstall:
        command: 'cabal build'
        options:
          stdout: true
          failOnError: true
      haddock:
        command: 'cabal haddock --executables'
        options:
          stdout: true
          failOnError: true
      cabalupdate:
        command: 'cabal update'
        options:
          stdout: true
          failOnError: true
    # watch certain file and do the right thing when they change
    watch:
      grunt:
        files: [ "Gruntfile.coffee"
          "package.json" ]
        tasks: "default"
      haskell:
        files: [
          "LICENSE.md"
          "README.md"
          "asteroids-netwire.cabal"
          "**/*.hs" ]
        tasks: [
          "shell:cabalprepare"
          "shell:cabalbuild"
          "shell:cabaltest"
          "clean:build"
          "shell:haddock"
          "shell:cabalinstall" ]
      updatenpm:
        files: "package.json"
        tasks: "shell:updatenpm"
  # load all our dependent grunt tasks
  grunt.loadNpmTasks "grunt-contrib-clean"
  grunt.loadNpmTasks "grunt-contrib-watch"
  grunt.loadNpmTasks "grunt-shell"
  # default task that will run whenever we run grunt with no options 
  grunt.registerTask "default", [
    "shell:cabalupdate"
    "shell:cabalprepare"
    "shell:cabalbuild"
    "shell:cabaltest"
    "clean:build"
    "shell:haddock"
    "shell:cabalinstall"
    "watch" ]
