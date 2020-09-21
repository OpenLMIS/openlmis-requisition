-- WHEN COMMITTING OR REVIEWING THIS FILE: Make sure that the timestamp in the file name (that serves as a version) is the latest timestamp, and that no new migration have been added in the meanwhile.
-- Adding migrations out of order may cause this migration to never execute or behave in an unexpected way.
-- Migrations should NOT BE EDITED. Add a new migration to apply changes.


CREATE TABLE budget_line_items (
	id uuid NOT NULL,
	budgetid uuid NOT NULL,
	fundsourceid uuid NOT NULL,
	allocatedbudget numeric NOT NULL,
	additive BOOLEAN NOT NULL DEFAULT 'true',
	creditvalue character varying(100) NOT NULL,
	notes character varying(255) NOT NULL,
	createdby uuid NOT NULL,
	createddate timestamp with time zone NOT NULL DEFAULT 'NOW()',
	modifiedby uuid NOT NULL,
	modifieddate timestamp with time zone NOT NULL DEFAULT 'NOW()',
	CONSTRAINT budget_line_items_pk PRIMARY KEY (id)
);

ALTER TABLE budget_line_items ADD CONSTRAINT fk_budget_line_items_budget_id FOREIGN KEY (budgetid) REFERENCES budgets(id);
ALTER TABLE budget_line_items ADD CONSTRAINT fk_budget_line_items_fund_source_id FOREIGN KEY (fundsourceid) REFERENCES source_of_funds(id);
