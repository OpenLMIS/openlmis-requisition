-- WHEN COMMITTING OR REVIEWING THIS FILE: Make sure that the timestamp in the file name (that serves as a version) is the latest timestamp, and that no new migration have been added in the meanwhile.
-- Adding migrations out of order may cause this migration to never execute or behave in an unexpected way.
-- Migrations should NOT BE EDITED. Add a new migration to apply changes.

DROP INDEX req_prod_fac_per;

CREATE UNIQUE INDEX req_prod_fac_per
ON requisitions(programid, facilityid, processingperiodid)
WHERE (emergency = FALSE AND supervisorynodeid IS NULL);

CREATE UNIQUE INDEX req_prod_fac_per_node
ON requisitions(programid, facilityid, processingperiodid, supervisorynodeid)
WHERE (emergency = FALSE AND supervisorynodeid IS NOT NULL);
