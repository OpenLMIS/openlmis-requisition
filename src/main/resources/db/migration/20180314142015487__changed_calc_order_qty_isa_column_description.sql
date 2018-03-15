UPDATE requisition.available_requisition_columns
SET definition = 'Calculated Order Quantity ISA is based on an ISA configured by commodity type, and several trade items may fill for one commodity type.'
WHERE name = 'calculatedOrderQuantityIsa';
