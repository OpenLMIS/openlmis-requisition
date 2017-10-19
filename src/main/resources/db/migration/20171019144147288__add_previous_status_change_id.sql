ALTER TABLE status_changes ADD COLUMN previousStatusChangeId uuid UNIQUE;

UPDATE status_changes
SET previousStatusChangeId = sub.previousId
FROM (
    SELECT DISTINCT ON (change.id)
    change.id AS id,
    previous.id AS previousId
    FROM status_changes change
    JOIN status_changes previous
	 ON previous.requisitionid = change.requisitionid
	 AND previous.createddate < change.createddate
    ORDER BY change.id, previous.createddate DESC NULLS LAST
    ) sub
WHERE sub.id = status_changes.id;
