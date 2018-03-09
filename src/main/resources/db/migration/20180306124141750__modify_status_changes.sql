ALTER TABLE status_changes DROP COLUMN previousStatusChangeId;
ALTER TABLE status_changes ADD COLUMN supervisorynodeid uuid;

CREATE OR REPLACE FUNCTION unique_status_changes() returns trigger LANGUAGE plpgsql AS $$
DECLARE
supervisoryNode uuid;
previousSupervisoryNode uuid;
requisitionStatus character varying(255);
previousStatus character varying(255);
BEGIN

  SELECT status, supervisorynodeid
  INTO requisitionstatus, supervisoryNode
  FROM requisition.status_changes
  WHERE requisitionid = NEW.requisitionid
  ORDER BY createddate DESC
  LIMIT 1;

  SELECT status, supervisorynodeid
  INTO previousStatus, previousSupervisoryNode
  FROM requisition.status_changes
  WHERE requisitionid = NEW.requisitionid
  ORDER BY createddate DESC
  LIMIT 1 OFFSET 1;

  IF (previousSupervisoryNode IS NULL OR previousSupervisoryNode = supervisoryNode) AND previousStatus = requisitionStatus
  THEN
    RAISE 'Duplicate status change: % at supervisory node: % ', NEW.status, NEW.supervisoryNodeId USING ERRCODE = 'unique_violation';
  ELSE
    RETURN NEW;
  END IF;

END $$;

CREATE CONSTRAINT TRIGGER check_status_changes
    AFTER INSERT ON status_changes
    INITIALLY DEFERRED
    FOR EACH ROW
    EXECUTE PROCEDURE unique_status_changes();