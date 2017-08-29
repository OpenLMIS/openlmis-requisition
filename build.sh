#!/bin/sh

# Sync with Transifex
export TX_PUSH=false
/transifex/sync_transifex.sh \
  --resource openlmis-requisition.messages \
  --pattern 'src/main/resources/messages_<lang>.properties' \
  --source-file src/main/resources/messages_en.properties

# Run Gradle build
gradle clean build
