
INSERT INTO referencedata.users (id, username, firstName, lastName, email, timezone)
VALUES ('35316636-6264-6331-2d34-3933322d3462', 'admin', 'Admin', 'User', 'example@mail.com', 'UTC');

INSERT INTO requisition.order_file_templates (id, filePrefix, headerInFile)
VALUES ('457ed5b0-80d7-4cb6-af54-e3f6138c8128', 'O', true);

INSERT INTO requisition.order_file_columns (id, openLmisField, dataFieldLabel, columnLabel, include, "position", format, nested, keyPath, orderFileTemplateId)
VALUES ('33b2d2e9-3167-46b0-95d4-1295be9afc21', true, 'fulfillment.header.order.number', 'Order number', true, 1, '', 'order', 'orderCode', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('6b8d331b-a0dd-4a1f-aafb-40e6a72ab9f6', true, 'fulfillment.header.facility.code', 'Facility code', true, 2, '', 'order', 'requisition/facility/code', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('752cda76-0db5-4b6e-bb79-0f531ab78e2e', true, 'fulfillment.header.product.code', 'Product code', true, 3, '', 'lineItem', 'product/code', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('9e825396-269d-4873-baa4-89054e2722f5', true, 'fulfillment.header.product.name', 'Product name', true, 4, '', 'lineItem', 'product/primaryName', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('cd57f329-f549-4717-882e-ecbf98122c39', true, 'fulfillment.header.approved.quantity', 'Approved quantity', true, 5, '', 'lineItem', 'approvedQuantity', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('d0e1aec7-1556-4dc1-8e21-d80a2d76b678', true, 'fulfillment.header.period', 'Period', true, 6, 'MM/yy', 'order', 'requisition/processingPeriod/startDate', '457ed5b0-80d7-4cb6-af54-e3f6138c8128'),
('dab6eec0-4cb4-4d4c-94b7-820308da73ff', true, 'fulfillment.header.order.date', 'Order date', true, 7, 'dd/MM/yy', 'order', 'createdDate', '457ed5b0-80d7-4cb6-af54-e3f6138c8128');

INSERT INTO requisition.order_number_configurations (id, orderNumberPrefix, includeOrderNumberPrefix, includeProgramCode, includeRequisitionTypeSuffix)
VALUES ('70543032-b131-4219-b44d-7781d29db330', 'O', true, true, true);
