ALTER TABLE requisition.requisition_template_assignments DROP COLUMN IF EXISTS requisitionReportOnly;

ALTER TABLE requisition.requisition_template_assignments ADD COLUMN requisitionReportOnly boolean default false;

DROP INDEX IF EXISTS req_tmpl_asgmt_prog_fac_type_unique_idx;
DROP INDEX IF EXISTS req_tmpl_asgmt_prog_fac_type_tmpl_unique_idx;
DROP INDEX if exists req_tmpl_asgmt_prog_tmpl_unique_idx;
-- the unique index should work only on current templates. It should be possible to have several
-- archived templates with the same name because of current template structure (we create a new
-- template if there is at least one requisition connected with the current template)
-- CREATE UNIQUE INDEX requisition_template_name_unique_idx
--    ON requisition_templates (lower(name), archived, requisitionReportOnly)
--    WHERE archived IS FALSE;

-- given program can have two templates for the given facility type and requisitionReportOnly
/*CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_unique_idx
    ON requisition.requisition_template_assignments (facilitytypeid, programid,templateid, requisitionReportOnly)
    WHERE facilitytypeid IS NOT NULL;

CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_tmpl_unique_idx
    ON requisition.requisition_template_assignments (facilitytypeid, programid, templateid, requisitionReportOnly)
    WHERE facilitytypeid IS NOT NULL;*/


-- in the given template there could not be facility type duplication
CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_tmpl_unique_idx
    ON requisition.requisition_template_assignments (facilitytypeid, programid, templateid, requisitionReportOnly)
    WHERE facilitytypeid IS NOT NULL;

CREATE UNIQUE INDEX req_tmpl_asgmt_prog_tmpl_unique_idx
    ON requisition.requisition_template_assignments (programid, templateid, requisitionReportOnly)
    WHERE facilitytypeid IS NULL;

-- given program can have only one template for the given facility type
CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_unique_idx
    ON requisition.requisition_template_assignments (facilitytypeid, programid, requisitionReportOnly)
    WHERE facilitytypeid IS NOT NULL;