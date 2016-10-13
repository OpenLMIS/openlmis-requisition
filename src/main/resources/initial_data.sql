INSERT INTO requisition.order_file_templates (id, filePrefix, headerInFile)
VALUES ('457ed5b0-80d7-4cb6-af54-e3f6138c8128', 'O', true);

INSERT INTO requisition.order_file_columns (id, openLmisField, dataFieldLabel, columnLabel, include, "position", format, nested, keyPath, related, relatedKeyPath, orderFileTemplateId)
VALUES ('33b2d2e9-3167-46b0-95d4-1295be9afc21', true, 'fulfillment.header.order.number', 'Order number', true, 1, null, 'order', 'orderCode', null, null, '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('6b8d331b-a0dd-4a1f-aafb-40e6a72ab9f6', true, 'fulfillment.header.facility.code', 'Facility code', true, 2, null, 'order', 'requisition/facilityId', 'Facility', 'code', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('752cda76-0db5-4b6e-bb79-0f531ab78e2e', true, 'fulfillment.header.product.code', 'Product code', true, 3, null, 'lineItem', 'orderableProductId', 'OrderableProduct', 'productCode', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('9e825396-269d-4873-baa4-89054e2722f5', true, 'fulfillment.header.product.name', 'Product name', true, 4, null, 'lineItem', 'orderableProductId', 'OrderableProduct', 'name', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('cd57f329-f549-4717-882e-ecbf98122c39', true, 'fulfillment.header.approved.quantity', 'Approved quantity', true, 5, null, 'lineItem', 'approvedQuantity', null, null, '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('d0e1aec7-1556-4dc1-8e21-d80a2d76b678', true, 'fulfillment.header.period', 'Period', true, 6, 'MM/yy', 'order', 'requisition/processingPeriodId', 'ProcessingPeriod', 'startDate', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('dab6eec0-4cb4-4d4c-94b7-820308da73ff', true, 'fulfillment.header.order.date', 'Order date', true, 7, 'dd/MM/yy', 'order', 'createdDate', null, null, '457ed5b0-80d7-4cb6-af54-e3f6138c8128');

INSERT INTO requisition.order_number_configurations (id, orderNumberPrefix, includeOrderNumberPrefix, includeProgramCode, includeRequisitionTypeSuffix)
VALUES ('70543032-b131-4219-b44d-7781d29db330', 'O', true, true, true);

INSERT INTO requisition.available_requisition_columns (id, name, label, indicator, mandatory) VALUES
('4a2e9fd3-1127-4b68-9912-84a5c00f6999', 'requestedQuantity', 'Requested Quantity', 'J', false),
('5ba8b72d-277a-4da8-b10a-23f0cda23cb4', 'totalReceivedQuantity', 'Total Received Quantity', 'B', false),
('33b2d2e9-3167-46b0-95d4-1295be9afc22', 'beginningBalance', 'Beginning Balance', 'A', false),
('752cda76-0db5-4b6e-bb79-0f531ab78e2c', 'stockOnHand', 'Stock On Hand', 'E', false),
('9e825396-269d-4873-baa4-89054e2722f4', 'totalConsumedQuantity', 'Total Consumed Quantity', 'C', false),
('cd57f329-f549-4717-882e-ecbf98122c38', 'totalLossesAndAdjustments', 'Total Losses And Adjustments', 'D', false),
('6b8d331b-a0dd-4a1f-aafb-40e6a72ab9f5', 'requestedQuantityExplanation', 'Requested Quantity Explanation', 'W', false),
('2ed8c74a-f424-4742-bd14-cfbe67b6e7be', 'remarks', 'Remarks', 'L', false),
('bde01507-3837-47b7-ae08-cec92c0c3cd2', 'productCode', 'Product Code', 'O', false),
('a62a5fed-c0b6-4d49-8a96-c631da0d0113', 'approvedQuantity', 'Approved Quantity', 'K', false);

INSERT INTO requisition.available_requisition_column_sources (columnId, value) VALUES
('4a2e9fd3-1127-4b68-9912-84a5c00f6999', 'USER_INPUT'), -- requestedQuantity
('5ba8b72d-277a-4da8-b10a-23f0cda23cb4', 'USER_INPUT'), -- totalReceivedQuantity
('33b2d2e9-3167-46b0-95d4-1295be9afc22', 'USER_INPUT'), -- beginningBalance
('752cda76-0db5-4b6e-bb79-0f531ab78e2c', 'USER_INPUT'), -- stockOnHand
('9e825396-269d-4873-baa4-89054e2722f4', 'USER_INPUT'), -- totalConsumedQuantity
('9e825396-269d-4873-baa4-89054e2722f4', 'CALCULATED'), -- totalConsumedQuantity
('cd57f329-f549-4717-882e-ecbf98122c38', 'USER_INPUT'), -- totalLossesAndAdjustments
('6b8d331b-a0dd-4a1f-aafb-40e6a72ab9f5', 'USER_INPUT'), -- requestedQuantityExplanation
('2ed8c74a-f424-4742-bd14-cfbe67b6e7be', 'USER_INPUT'), -- remarks
('bde01507-3837-47b7-ae08-cec92c0c3cd2', 'USER_INPUT'), -- productCode
('a62a5fed-c0b6-4d49-8a96-c631da0d0113', 'CALCULATED'); -- approvedQuantity
