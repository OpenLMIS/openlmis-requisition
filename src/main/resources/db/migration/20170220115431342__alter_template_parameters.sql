ALTER TABLE requisition.template_parameters RENAME COLUMN selectSql to selectExpression;
ALTER TABLE requisition.template_parameters ADD selectProperty text;
