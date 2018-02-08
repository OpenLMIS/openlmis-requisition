CREATE TABLE requisition_template_assignments (
    id UUID PRIMARY KEY NOT NULL,
    programid uuid NOT NULL,
    facilitytypeid uuid,
    templateid uuid NOT NULL
);

CREATE UNIQUE INDEX requisition_template_assignment_unique_program_facility_type_template
ON requisition_template_assignments (facilitytypeid, programid, templateid)
WHERE facilitytypeid IS NOT NULL;

CREATE UNIQUE INDEX requisition_template_assignment_unique_program_template
ON requisition_template_assignments (programid, templateid)
WHERE facilitytypeid IS NULL;

ALTER TABLE ONLY requisition_template_assignments
    ADD CONSTRAINT requisition_template_assignments_requisition_template_fkey FOREIGN KEY (templateid) REFERENCES requisition_templates(id);
