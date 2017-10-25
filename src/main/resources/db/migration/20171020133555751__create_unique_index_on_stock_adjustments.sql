UPDATE stock_adjustments t1
SET quantity = t2.quantity
FROM
  (SELECT SUM(quantity) AS quantity,
          requisitionlineitemid,
          reasonid
   FROM stock_adjustments
   GROUP BY requisitionlineitemid,
            reasonid) t2
WHERE t1.reasonid = t2.reasonid
  AND t1.requisitionlineitemid = t2.requisitionlineitemid;

DELETE FROM stock_adjustments t1
  USING stock_adjustments t2
WHERE t1.ctid < t2.ctid
  AND t1.reasonid = t2.reasonid
  AND t1.requisitionlineitemid = t2.requisitionlineitemid;

CREATE UNIQUE INDEX req_line_reason ON stock_adjustments(reasonid, requisitionlineitemid);
