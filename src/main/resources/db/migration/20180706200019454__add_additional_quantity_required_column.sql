INSERT INTO requisition.available_requisition_columns
  (id, name, label, indicator,
   mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition)
VALUES
  ('90a84767-1009-4d8e-be13-f006fce2a002', 'additionalQuantityRequired', 'Additional quantity required', 'Z',
   false, false, true, true, 'NUMERIC', 'Additional quantity required for new patients');

INSERT INTO requisition.available_requisition_column_sources (columnId, value)
VALUES ('90a84767-1009-4d8e-be13-f006fce2a002', 'USER_INPUT');

ALTER TABLE requisition.requisition_line_items
  ADD COLUMN additionalQuantityRequired INTEGER;

