##Quick start

1. Install Node.js
2. Install SC5 Style Guide Generator
   `> npm install -g sc5-styleguide`
3. Run grunt task
   `> grunt less`
4. Create `/styleguide` at `openlmis-requisition`.
5. Copy `/images` and `/fonts` from `modules/openlmis-web/public` into `/styleguide`.
6. Create file `body.css` in `/styleguide` with that content:

  body {
    background-color: #fff;
    font-family: Arial, sans-serif;
  }
  .content h2 {
    color: #17888F;
  }

7. Generate styleguide
   `> styleguide --kss-source "style/*.scss" --style-source "path/to/openlmis-v2/bootstrap.min.css" --style-source "path/to/openlmis-v2/app.css" --style-source "path/to/openlmis-v2/select2.css" --overviewPath "style/overview.md" --extraHead '<link rel="stylesheet" type="text/css" href="/body.css"/>' --title "OpenLMIS Styleguide" --output styleguide --watch --server`

