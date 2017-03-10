ALTER TABLE requisition.stock_adjustments ALTER COLUMN reasonid TYPE uuid USING CAST(substring(CAST (reasonid AS text) from 3) AS uuid);
