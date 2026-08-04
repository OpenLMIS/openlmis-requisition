-- Register the read-only supplyingFacilityStockOnHand requisition-template column.
--
-- The column is display-only; its value is computed at read time, so no
-- requisition_line_items column is added here (mirrors idealStockAmount registration).
-- It is disabled by default: the column is added to every existing template with isdisplayed=false.
-- Enabling it for a given implementation (e.g. Gambia) is done via template configuration, not here.

-- (a) Register the available column. supportsTag is left to its DEFAULT false. canChangeOrder=true /
--     canBeChangedByUser=false mirror the read-only 'stockOnHand' column.
INSERT INTO requisition.available_requisition_columns
  (id, name, label, indicator,
   mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition)
VALUES
  ('1a079695-9f34-4339-850c-1fc0d9675314', 'supplyingFacilityStockOnHand',
   'Supplying facility stock on hand', 'SF',
   false, false, true, false, 'NUMERIC',
   'Stock on hand at the supplying facility for this product, displayed during approval');

-- (b) Register the (only) allowed source. The value stores the SourceType enum NAME.
INSERT INTO requisition.available_requisition_column_sources (columnId, value)
VALUES ('1a079695-9f34-4339-850c-1fc0d9675314', 'SUPPLYING_FACILITY_STOCK');

-- (c) Add the column to every existing template, DISABLED by default (isdisplayed=false).
--     source = 5 is the ordinal of SUPPLYING_FACILITY_STOCK (columns_maps.source is persisted by
--     ordinal). key = name keeps the (requisitiontemplateid, key) primary key unique.
INSERT INTO requisition.columns_maps
  (requisitiontemplateid, requisitioncolumnid, definition, displayorder, indicator,
   isdisplayed, label, name, requisitioncolumnoptionid, source, key, tag)
SELECT
  t.id, a.id, a.definition, c.count + 1, a.indicator,
  false, a.label, a.name, NULL, 5, a.name, NULL
FROM
  requisition.requisition_templates AS t
  INNER JOIN (SELECT requisitiontemplateid, count(*) FROM requisition.columns_maps
              GROUP BY requisitiontemplateid) AS c ON c.requisitiontemplateid = t.id
  INNER JOIN requisition.available_requisition_columns AS a
             ON a.name = 'supplyingFacilityStockOnHand';
