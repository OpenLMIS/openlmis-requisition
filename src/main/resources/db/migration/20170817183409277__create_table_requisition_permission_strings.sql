CREATE TABLE requisition_permission_strings (
    id uuid NOT NULL PRIMARY KEY,
    requisitionid uuid NOT NULL REFERENCES requisitions,
    permissionstring text NOT NULL
);

CREATE INDEX ON requisition_permission_strings (requisitionid);