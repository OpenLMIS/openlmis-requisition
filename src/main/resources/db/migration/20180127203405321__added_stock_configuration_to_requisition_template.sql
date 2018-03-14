ALTER TABLE requisition_templates
    ADD populateStockOnHandFromStockCards boolean DEFAULT false NOT NULL;

INSERT INTO requisition.available_requisition_column_sources (columnId, value)
    VALUES ('752cda76-0db5-4b6e-bb79-0f531ab78e2c', 'STOCK_CARDS'); -- stockOnHand