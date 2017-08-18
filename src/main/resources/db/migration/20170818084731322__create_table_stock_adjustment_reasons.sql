CREATE TABLE stock_adjustment_reasons
(
  id UUID PRIMARY KEY NOT NULL,
  reasonid UUID NOT NULL,
  description TEXT,
  isfreetextallowed BOOLEAN NOT NULL,
  name TEXT NOT NULL,
  reasoncategory TEXT NOT NULL,
  reasontype TEXT NOT NULL,
  requisitionid uuid
);

ALTER TABLE ONLY stock_adjustment_reasons
  ADD CONSTRAINT fk_stock_adjustment_reasons_requisitions
  FOREIGN KEY (requisitionid) REFERENCES requisitions(id);

CREATE UNIQUE INDEX uk_stock_adjustment_reasons_name ON stock_adjustment_reasons (name);
