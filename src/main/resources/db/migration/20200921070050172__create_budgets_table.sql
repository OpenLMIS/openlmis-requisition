-- WHEN COMMITTING OR REVIEWING THIS FILE: Make sure that the timestamp in the file name (that serves as a version) is the latest timestamp, and that no new migration have been added in the meanwhile.
-- Adding migrations out of order may cause this migration to never execute or behave in an unexpected way.
-- Migrations should NOT BE EDITED. Add a new migration to apply changes.

CREATE TABLE budgets (
	id uuid NOT NULL,
	facilityid uuid NOT NULL,
	sourceapplication character varying(50) NOT NULL UNIQUE,
	createdby uuid NOT NULL,
	createddate timestamp with time zone NOT NULL DEFAULT 'now()',
	modifiedby uuid NOT NULL,
	modifieddate timestamp with time zone NOT NULL DEFAULT 'NOW()',
	CONSTRAINT budgets_pk PRIMARY KEY (id)
);