##Quick start

1. Install Node.js and npm

- NPM dependencies (used for linting JS, LESS files, minifying JS files & running jasmine specs etc.)
  * Install Grunt command-line runner by running (after installing Node.js)
    `> npm install -g grunt-cli`
  * Install karma test runner with karma coverage by running
    `> npm install -g karma karma-coverage`
  * Install karma command line with:
    `> npm install -g karma-cli`
  * You may need to install further karma dependencies for FireFox:
    `> npm install -g  karma-firefox-install`
  * And for jasmine:
    `> npm install -g  karma-jasmine`
  * Grunt tasks available can be found in `modules/openlmis-web/Gruntfile.js`

2. Install SC5 Style Guide Generator
   `> npm install sc5-styleguide`
3. Run grunt task
   `> grunt less`
4. Generate styleguide
   `> styleguide --kss-source "style/*.scss" --style-source "path/to/openlmis-v2/all/css/files" --style-source "path/to/openlmis-v2/bootstrap/all/css/files" --overviewPath "style/overview.md" --output styleguide --watch --server`
