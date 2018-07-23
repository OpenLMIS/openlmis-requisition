CREATE OR REPLACE FUNCTION unique_status_changes() returns trigger LANGUAGE plpgsql AS $$
BEGIN
  DROP TABLE IF EXISTS status_change_data;
  CREATE TEMP TABLE status_change_data AS
  (
      SELECT status, supervisorynodeid
      FROM requisition.status_changes
      WHERE requisitionid = NEW.requisitionid
      ORDER BY createddate DESC
      LIMIT 2
  );

  IF EXISTS (SELECT 1 FROM status_change_data GROUP BY status, supervisoryNodeId HAVING COUNT(*) > 1)
  THEN
    RAISE 'Duplicate status change: % at supervisory node: % ', NEW.status, NEW.supervisoryNodeId USING ERRCODE = 'unique_violation';
  ELSE
    RETURN NEW;
  END IF;

END $$;
