ALTER TABLE requisition.requisition_templates
ADD COLUMN IF NOT EXISTS enableAvgConsumptionForCurrentPeriod BOOLEAN DEFAULT FALSE NOT NULL;