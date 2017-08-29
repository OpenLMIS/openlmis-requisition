INSERT INTO requisition.requisition_permission_strings
WITH requisition_rights (name) AS (VALUES ('REQUISITION_VIEW'))
SELECT uuid_generate_v4() AS id
  , r.id AS requisitionid
  , rr.name || '|' || r.facilityid || '|' || r.programid AS permissionstring
FROM requisition.requisitions r
  CROSS JOIN requisition_rights rr
;