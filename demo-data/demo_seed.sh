#!/bin/sh

# This script populates the database with demo data for presentational and testing purposes.
# It searches for json files in given directory and inserts the contained records into the database.
# Note: file name should refer to their target table name.

DIRECTORY=${1}
GENERATOR=${2}

# Get list of JSON files in current directory
FILES=`find ${DIRECTORY} -name "*.json"`

# Run database input generation
${GENERATOR} ${FILES}

# TODO: Automatically populate the database with seed file
mv input.sql ${DIRECTORY}

echo "\nGenerated ${DIRECTORY}/input.sql"
echo "To insert the data into database, first run the service, and then from outside of container type:"
echo "docker exec -i openlmisrequisition_db_1 psql -Upostgres open_lmis < demo-data/input.sql"
