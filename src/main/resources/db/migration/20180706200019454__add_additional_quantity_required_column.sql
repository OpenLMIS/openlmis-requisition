-- add a new column to the system
INSERT INTO requisition.available_requisition_columns
  (id, name, label, indicator,
   mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition)
VALUES
  ('90a84767-1009-4d8e-be13-f006fce2a002', 'additionalQuantityRequired', 'Additional quantity required', 'Z',
   false, false, true, true, 'NUMERIC', 'Additional quantity required for new patients');

INSERT INTO requisition.available_requisition_column_sources (columnId, value)
VALUES ('90a84767-1009-4d8e-be13-f006fce2a002', 'USER_INPUT');

-- update the requisition line item structure
ALTER TABLE requisition.requisition_line_items
  ADD COLUMN additionalQuantityRequired INTEGER;

-- add the new column to all requisition templates
INSERT INTO
	requisition.columns_maps(requisitiontemplateid, requisitioncolumnid, definition, displayorder, indicator, isdisplayed, label, name, requisitioncolumnoptionid, source, key, tag)
SELECT
	t.id, a.id, a.definition, c.count + 1, a.indicator, false, a.label, a.name, NULL, 0, a.name, NULL
FROM
	requisition.requisition_templates AS t
	INNER JOIN (SELECT requisitiontemplateid, count(*) FROM requisition.columns_maps GROUP BY requisitiontemplateid) AS c ON c.requisitiontemplateid = t.id
	INNER JOIN requisition.available_requisition_columns AS a ON a.name = 'additionalQuantityRequired';
