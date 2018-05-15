ALTER TABLE requisition.available_requisition_columns
ADD COLUMN supportsTag BOOLEAN DEFAULT false;

UPDATE requisition.available_requisition_columns
SET supportsTag = true
WHERE name = 'totalConsumedQuantity' OR name = 'totalReceivedQuantity';

ALTER TABLE requisition.columns_maps
ADD COLUMN tag character varying(255);