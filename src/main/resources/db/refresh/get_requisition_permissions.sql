WITH requisition_rights (name) AS (VALUES 
  ('REQUISITION_VIEW'))
SELECT r.id AS requisitionid
  , rr.name || '|' || r.facilityid || '|' || r.programid AS permissionstring
FROM requisition.requisitions r
  CROSS JOIN requisition_rights rr