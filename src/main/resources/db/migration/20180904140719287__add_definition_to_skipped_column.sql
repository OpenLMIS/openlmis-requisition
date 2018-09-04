UPDATE requisition.available_requisition_columns
SET definition = 'Select the check box below to skip a single product. Remove all data from the row prior to selection.'
WHERE name = 'skipped';