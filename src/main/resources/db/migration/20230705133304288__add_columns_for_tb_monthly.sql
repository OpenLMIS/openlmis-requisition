INSERT INTO requisition.available_requisition_columns
  (id, name, label, indicator,
   mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition)
VALUES
  ('ef7c65c6-1be3-410b-9a2e-8b9f68221e82', 'numberOfPatientsOnTreatmentNextMonth', 'No of Patients on Treatment next month (C)', 'TC',
   false, false, true, true, 'NUMERIC', 'The number of patients who will be treated in the next month.'),
  ('7563a92f-2c07-4b09-a88e-d0faa0baee93', 'totalRequirement', 'Total Requirement (E) E = C * D', 'TE',
      false, false, true, true, 'NUMERIC', 'Product requirement for all patients.'),
  ('d58e83a3-78ee-4657-b3d9-7b1adc983837', 'totalQuantityNeededByHf', 'Total Quantity Needed by HF (F) F = E * 2', 'TF',
     false, false, true, true, 'NUMERIC', 'Total quantity of product needed by Health Facility.'),
  ('421cd415-f127-4683-a396-a10a7bfdc384', 'quantityToIssue', 'Quantity to issue (G) G = F - A', 'TG',
     false, false, true, true, 'NUMERIC', 'Number of products to be issued.'),
  ('d173872d-f5e7-48a2-b0a7-c7fcae4208d4', 'convertedQuantityToIssue', 'Quantity to issue (converted to unit of measure) G/U', 'TH',
     false, false, true, true, 'NUMERIC', 'Quantity to issue expressed in the units in which it is measured.'),
  ('3efdf855-cc03-4ccc-97f2-60108bc4b7a3', 'programOrderables.dosesperpatient', 'Individual Monthly Requirement (D)', 'TD',
     false, false, true, true, 'NUMERIC', 'The amount of product for single patient.');

INSERT INTO requisition.available_requisition_column_sources (columnId, value) VALUES
('ef7c65c6-1be3-410b-9a2e-8b9f68221e82', 'CALCULATED'),
('ef7c65c6-1be3-410b-9a2e-8b9f68221e82', 'USER_INPUT'),
('7563a92f-2c07-4b09-a88e-d0faa0baee93', 'CALCULATED'),
('d58e83a3-78ee-4657-b3d9-7b1adc983837', 'CALCULATED'),
('421cd415-f127-4683-a396-a10a7bfdc384', 'CALCULATED'),
('d173872d-f5e7-48a2-b0a7-c7fcae4208d4', 'CALCULATED'),
('3efdf855-cc03-4ccc-97f2-60108bc4b7a3', 'CALCULATED');


ALTER TABLE requisition.requisition_line_items
  ADD COLUMN numberOfPatientsOnTreatmentNextMonth INTEGER,
  ADD COLUMN totalRequirement INTEGER,
  ADD COLUMN totalQuantityNeededByHf INTEGER,
  ADD COLUMN quantityToIssue INTEGER,
  ADD COLUMN convertedQuantityToIssue INTEGER;

-- add the new column to all requisition templates
INSERT INTO requisition.columns_maps (requisitiontemplateid, requisitioncolumnid, definition, displayorder, indicator,
isdisplayed, label, name, requisitioncolumnoptionid, source, key, tag)
SELECT t.id, a.id, a.definition,
  CASE a.name
    WHEN 'numberOfPatientsOnTreatmentNextMonth' THEN 7
    WHEN 'programOrderables.dosesperpatient' THEN 8
    WHEN 'totalRequirement' THEN 9
    WHEN 'totalQuantityNeededByHf' THEN 10
    WHEN 'quantityToIssue' THEN 11
    WHEN 'convertedQuantityToIssue' THEN 12
  END AS displayorder, a.indicator, false, a.label, a.name, NULL, 1, a.name, NULL
FROM
  requisition.requisition_templates AS t
  INNER JOIN (
    SELECT requisitiontemplateid, count(*) FROM requisition.columns_maps GROUP BY requisitiontemplateid
  ) AS c ON c.requisitiontemplateid = t.id
  INNER JOIN requisition.available_requisition_columns AS a ON a.name IN (
    'numberOfPatientsOnTreatmentNextMonth',
    'programOrderables.dosesperpatient',
    'totalRequirement',
    'totalQuantityNeededByHf',
    'quantityToIssue',
    'convertedQuantityToIssue'
  );
