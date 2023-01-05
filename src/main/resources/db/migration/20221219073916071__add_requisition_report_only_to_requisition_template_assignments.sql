ALTER TABLE requisition.requisition_template_assignments ADD COLUMN requisitionReportOnly boolean default false;

DROP INDEX req_tmpl_asgmt_prog_fac_type_unique_idx;

-- the unique index should work only on current templates. It should be possible to have several
-- archived templates with the same name because of current template structure (we create a new
-- template if there is at least one requisition connected with the current template)
-- CREATE UNIQUE INDEX requisition_template_name_unique_idx
--    ON requisition_templates (lower(name), archived, requisitionReportOnly)
--    WHERE archived IS FALSE;

-- given program can have two templates for the given facility type and requisitionReportOnly
CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_unique_idx
    ON requisition_template_assignments (facilitytypeid, programid, requisitionReportOnly)
    WHERE facilitytypeid IS NOT NULL;