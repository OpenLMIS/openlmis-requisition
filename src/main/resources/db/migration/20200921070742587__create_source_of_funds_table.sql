-- WHEN COMMITTING OR REVIEWING THIS FILE: Make sure that the timestamp in the file name (that serves as a version) is the latest timestamp, and that no new migration have been added in the meanwhile.
-- Adding migrations out of order may cause this migration to never execute or behave in an unexpected way.
-- Migrations should NOT BE EDITED. Add a new migration to apply changes.

CREATE TABLE source_of_funds (
	id uuid NOT NULL,
	code character varying(200) NOT NULL UNIQUE,
	name character varying(200) NOT NULL UNIQUE,
	createdDate timestamp with time zone NOT NULL DEFAULT 'NOW()',
	modifiedDate timestamp with time zone NOT NULL DEFAULT 'NOW()',
	displayOrder uuid NOT NULL,
	CONSTRAINT source_of_funds_pk PRIMARY KEY (id)
);