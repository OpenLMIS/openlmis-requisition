-- This script allows to add indexes to core database without possibility to add duplicated one, with two different names but on the same columns.
CREATE FUNCTION addindexesifnotexist(schema_parameter VARCHAR, table_parameter VARCHAR, indexed_parameter VARCHAR)
RETURNS void AS
$$
DECLARE index_exists BOOLEAN;
DECLARE new_index_name VARCHAR;
DECLARE comma_replaced_indexed_parameter VARCHAR;

BEGIN
	SELECT EXISTS (
        SELECT 1
        FROM (
            SELECT
                n.nspname AS schema_name,
                t.relname AS table_name,
                i.relname AS index_name,
                array_to_string(array_agg(a.attname ORDER BY a.attname), ',') AS indexed_columns
            FROM pg_class t
            JOIN pg_index ix ON t.oid = ix.indrelid
            JOIN pg_class i ON i.oid = ix.indexrelid
            JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = ANY(ix.indkey)
            JOIN pg_namespace n ON n.oid = t.relnamespace
            WHERE t.relkind = 'r'  -- regular tables (relations)
            GROUP BY
                n.nspname,
                t.relname,
                i.relname
        ) AS existing_indexes_info
        WHERE schema_name = schema_parameter
	    AND table_name = table_parameter
	    AND indexed_columns = indexed_parameter
    ) INTO index_exists;

   SELECT REPLACE(indexed_parameter, ',', '_') INTO comma_replaced_indexed_parameter;
   SELECT CONCAT(table_parameter, '_', comma_replaced_indexed_parameter, '_idx') INTO new_index_name;

   IF NOT index_exists THEN
    EXECUTE format('CREATE INDEX %I ON %I.%I (%s)', new_index_name, schema_parameter, table_parameter, indexed_parameter);
   END IF;

END; $$ LANGUAGE plpgsql;


-- do not insert whitespaces in parameters, variables in the third argument must be sorted alfabetically
SELECT * FROM addindexesifnotexist('requisition', 'status_messages', 'requisitionid');
SELECT * FROM addindexesifnotexist('requisition', 'requisitions_previous_requisitions', 'previousrequisitionid');
SELECT * FROM addindexesifnotexist('requisition', 'stock_adjustments', 'requisitionlineitemid');
SELECT * FROM addindexesifnotexist('requisition', 'stock_adjustment_reasons', 'requisitionid');


DROP FUNCTION addindexesifnotexist(varchar, varchar, varchar);