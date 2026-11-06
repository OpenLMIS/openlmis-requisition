-- Human readable requisition code, generated from the requisition.code.pattern property.
-- Requisitions created before this migration have no code.
ALTER TABLE requisitions ADD COLUMN code TEXT;

CREATE UNIQUE INDEX req_code_unique_idx ON requisitions (code);
