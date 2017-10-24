DELETE FROM status_messages t1
  USING status_messages t2
WHERE t1.ctid < t2.ctid
  AND t1.statusChangeId = t2.statusChangeId;

ALTER TABLE status_messages ADD CONSTRAINT status_change_id_unique UNIQUE (statusChangeId) DEFERRABLE INITIALLY DEFERRED;