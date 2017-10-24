DELETE FROM stock_adjustments t1
  USING stock_adjustments t2
WHERE t1.ctid < t2.ctid
  AND t1.reasonid = t2.reasonid
  AND t1.requisitionlineitemid = t2.requisitionlineitemid;

CREATE UNIQUE INDEX req_line_reason ON stock_adjustments(reasonid, requisitionlineitemid);