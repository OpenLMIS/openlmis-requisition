ALTER TABLE requisition_templates
    ADD populateStockOnHandFromStockCardsEnabled boolean NOT NULL DEFAULT false;

INSERT INTO requisition.available_requisition_column_sources (columnId, value)
    VALUES ('752cda76-0db5-4b6e-bb79-0f531ab78e2c', 'STOCK_CARDS'); -- stockOnHand