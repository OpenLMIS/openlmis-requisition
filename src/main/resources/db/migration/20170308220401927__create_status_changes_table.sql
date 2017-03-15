CREATE TABLE status_changes (
    id uuid NOT NULL,
    createddate timestamp with time zone,
    modifieddate timestamp with time zone,
    authorid uuid,
    status character varying(255) NOT NULL,
    requisitionid uuid NOT NULL
);
