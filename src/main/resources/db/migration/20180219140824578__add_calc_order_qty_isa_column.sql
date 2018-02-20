INSERT INTO requisition.available_requisition_columns (id, name, label, indicator, mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition)
VALUES('9ac91518-38c7-494c-95d9-c37ef2ffff81', 'calculatedOrderQuantityIsa', 'Calc Order Qty ISA', 'S', false, false, true, true, 'NUMERIC', 'The calculated order quantity based on Ideal Stock Amount');

INSERT INTO requisition.available_requisition_column_sources (columnId, value)
VALUES ('9ac91518-38c7-494c-95d9-c37ef2ffff81', 'CALCULATED');

ALTER TABLE requisition.requisition_line_items
ADD COLUMN calculatedOrderQuantityIsa INTEGER;

