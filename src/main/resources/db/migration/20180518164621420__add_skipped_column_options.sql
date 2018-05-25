INSERT INTO requisition.available_requisition_column_sources (columnid, value)
VALUES ('c6dffdee-3813-40d9-8737-f531d5adf420', 'PREVIOUS_REQUISITION');

INSERT INTO requisition.available_requisition_column_options (id, optionlabel, optionname, columnid)
VALUES
  ('17d6e860-a746-4500-a0fa-afc84d799dca', 'requisitionConstants.disableSkippedLineItems', 'disableSkippedLineItems',
   'c6dffdee-3813-40d9-8737-f531d5adf420'),
  ('488e6882-563d-4b69-b7eb-fd59e7772a41', 'requisitionConstants.hideSkippedLineItems', 'hideSkippedLineItems',
   'c6dffdee-3813-40d9-8737-f531d5adf420');

-- Migrate existing implementations to the default 'skipped' column behavior.
-- previously defined requisition templates will default to "disable skipped line items" option.
UPDATE requisition.columns_maps
SET requisitioncolumnoptionid = '17d6e860-a746-4500-a0fa-afc84d799dca'
WHERE name = 'skipped' and requisitioncolumnoptionid IS NULL;


-- allow the user
UPDATE requisition.available_requisition_column_options
    SET optionLabel = 'requisitionConstants.newPatientCount'
    WHERE optionName = 'newPatientCount';

UPDATE requisition.available_requisition_column_options
    SET optionLabel = 'requisitionConstants.dispensingUnitsForNewPatients'
    WHERE optionName = 'dispensingUnitsForNewPatients';

UPDATE requisition.available_requisition_column_options
    SET optionLabel = 'requisitionConstants.default'
    WHERE optionName = 'default';