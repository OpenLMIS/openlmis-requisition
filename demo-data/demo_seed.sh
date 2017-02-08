#!/bin/bash

# This script populates the database with demo data for presentational and testing purposes.
# It searches for json files in given directory and inserts the contained records into the database.
# Note: file name should refer to their target table name.

DIRECTORY=${1}
GENERATOR=${2}
OUTPUT_DIR=${DIRECTORY}/../build/demo-data

# Get list of JSON files in current directory
FILES=`find ${DIRECTORY} -name "*.json"`

# Run database input generation
${GENERATOR} ${FILES}

# Prepend and append conditional return so SQL file only runs once
# Also prepend starter SQL file because Flyway only likes one afterMigrate.sql file
CAT_ARGS=()

# this small function add path to the given file ($1) only if the file exist.
function addCatArg {
  if [ -f $1 ]; then
    CAT_ARGS+=($1)
  fi
}

addCatArg ${DIRECTORY}/demo_sql_header.txt
addCatArg ${DIRECTORY}/../src/main/resources/db/starter/afterMigrate.sql
addCatArg input.sql
addCatArg ${DIRECTORY}/demo_sql_footer.txt

cat ${CAT_ARGS[@]} > result.sql

mkdir -p ${OUTPUT_DIR}
mv result.sql ${OUTPUT_DIR}/afterMigrate.sql
rm input.sql

echo "Generated ${OUTPUT_DIR}/afterMigrate.sql"
echo "To insert the data into database, first run the service, and then from outside of container type:"
echo "docker exec -i openlmisrequisition_db_1 psql -Upostgres open_lmis < ${OUTPUT_DIR}/afterMigrate.sql"
