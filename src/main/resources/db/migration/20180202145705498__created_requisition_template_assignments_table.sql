CREATE TABLE requisition_template_assignments (
    id UUID PRIMARY KEY NOT NULL,
    programid uuid NOT NULL,
    facilitytypeid uuid,
    templateid uuid NOT NULL
);

CREATE UNIQUE INDEX req_tmpl_asgmt_prog_fac_type_tmpl_unique_idx
ON requisition_template_assignments (facilitytypeid, programid, templateid)
WHERE facilitytypeid IS NOT NULL;

CREATE UNIQUE INDEX req_tmpl_asgmt_prog_tmpl_unique_idx
ON requisition_template_assignments (programid, templateid)
WHERE facilitytypeid IS NULL;

ALTER TABLE ONLY requisition_template_assignments
    ADD CONSTRAINT req_tmpl_asgmt_req_tmpl_fkey FOREIGN KEY (templateid) REFERENCES requisition_templates(id);
