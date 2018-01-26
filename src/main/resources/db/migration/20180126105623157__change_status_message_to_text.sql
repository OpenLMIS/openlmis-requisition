ALTER TABLE requisition.requisitions
ALTER COLUMN draftstatusmessage TYPE TEXT;

ALTER TABLE requisition.status_messages
ALTER COLUMN body TYPE TEXT;
