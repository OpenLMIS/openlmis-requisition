ALTER TABLE ONLY requisition_templates ADD COLUMN archived boolean NOT NULL DEFAULT FALSE;
ALTER TABLE ONLY requisition_templates ADD COLUMN name character varying(255) NOT NULL;

-- set archived flag for old templates
UPDATE requisition_templates
SET archived = true;

UPDATE requisition_templates
SET archived = false
WHERE id IN (
  SELECT id
  FROM requisition.requisition_templates AS t1
  WHERE t1.createdDate = (
    SELECT max(t2.createdDate)
    FROM requisition.requisition_templates AS t2
    WHERE t1.programId = t2.programId
  )
);

-- the unique index should work only on current templates. It should be possible to have several
-- archived templates with the same name because of current template structure (we create a new
-- template if there is at least one requisition connected with the current template)
CREATE UNIQUE INDEX requisition_template_name_unique_idx
ON requisition_templates (name, archived)
WHERE archived IS FALSE;
