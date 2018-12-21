DROP INDEX requisition_template_name_unique_idx;

-- the unique index should work only on current templates. It should be possible to have several
-- archived templates with the same name because of current template structure (we create a new
-- template if there is at least one requisition connected with the current template)
CREATE UNIQUE INDEX requisition_template_name_unique_idx
ON requisition_templates (lower(name), archived)
WHERE archived IS FALSE;
