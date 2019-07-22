-- WHEN COMMITTING OR REVIEWING THIS FILE: Make sure that the timestamp in the file name (that serves as a version) is the latest timestamp, and that no new migration have been added in the meanwhile.
-- Adding migrations out of order may cause this migration to never execute or behave in an unexpected way.
-- Migrations should NOT BE EDITED. Add a new migration to apply changes.

ALTER TABLE requisition.available_products
  RENAME COLUMN orderableVersionId TO orderableVersionNumber;

ALTER TABLE requisition.requisition_line_items
  RENAME COLUMN orderableVersionId TO orderableVersionNumber;

ALTER TABLE requisition.requisition_line_items
  RENAME COLUMN facilityTypeApprovedProductVersionId TO facilityTypeApprovedProductVersionNumber;

ALTER TABLE requisition.available_products
  RENAME COLUMN facilityTypeApprovedProductVersionId TO facilityTypeApprovedProductVersionNumber;