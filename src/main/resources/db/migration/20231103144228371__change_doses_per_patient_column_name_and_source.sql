UPDATE requisition.available_requisition_columns
SET name = 'dosesPerPatient'
WHERE id = '3efdf855-cc03-4ccc-97f2-60108bc4b7a3';

UPDATE requisition.columns_maps
SET name = 'dosesPerPatient', key = 'dosesPerPatient', source = 2
WHERE requisitioncolumnid = '3efdf855-cc03-4ccc-97f2-60108bc4b7a3';