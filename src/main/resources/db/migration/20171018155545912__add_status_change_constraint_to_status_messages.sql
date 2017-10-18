ALTER TABLE status_messages ADD COLUMN statusChangeId uuid;

ALTER TABLE ONLY status_changes
    ADD CONSTRAINT status_changes_pkey PRIMARY KEY (id);

UPDATE status_messages
SET statusChangeId = status_changes.id
FROM status_changes
WHERE status_changes.requisitionId = status_messages.requisitionId AND status_changes.status = status_messages.status;

ALTER TABLE status_messages ALTER COLUMN statusChangeId SET NOT NULL;

ALTER TABLE ONLY status_messages
    ADD CONSTRAINT fk_status_messages_status_change_id FOREIGN KEY (statusChangeId) REFERENCES status_changes(id);
