ALTER TABLE requisition.available_non_full_supply_products ALTER COLUMN value TYPE uuid USING CAST(substring(CAST (value AS text) from 3) AS uuid);
