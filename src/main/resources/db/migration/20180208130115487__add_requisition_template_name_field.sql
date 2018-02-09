ALTER TABLE ONLY requisition_templates ADD COLUMN name character varying(255) NOT NULL;
-- disable for now
-- CREATE UNIQUE INDEX requisition_template_name_unique_idx ON requisition_templates (name)
