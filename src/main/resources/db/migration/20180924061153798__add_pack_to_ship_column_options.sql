INSERT INTO requisition.available_requisition_column_options (id, optionlabel, optionname, columnid)
VALUES
  ('dcf41f06-3000-4af6-acf5-5de4fffc966f', 'requisitionConstants.showPackToShipInAllPages', 'showPackToShipInAllPages',
   'dc9dde56-593d-4929-81be-d1faec7025a8'),
  ('d1ff8f6f-5bbb-4b0e-8dd2-3835bfc03629', 'requisitionConstants.showPackToShipInApprovalPage', 'showPackToShipInApprovalPage',
   'dc9dde56-593d-4929-81be-d1faec7025a8');


UPDATE requisition.columns_maps
SET requisitioncolumnoptionid = 'dcf41f06-3000-4af6-acf5-5de4fffc966f'
WHERE name = 'packsToShip' and requisitioncolumnoptionid IS NULL;

