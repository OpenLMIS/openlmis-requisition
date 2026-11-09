-- Counters for the {seq} token of code patterns. The key is built by the code generator from
-- its sequence name and the configured sequence scope, for example
-- 'requisition|facility=<uuid>;year=2026;month=10', so changing the scope needs no migration.
CREATE TABLE code_sequences (
    scopekey TEXT NOT NULL,
    lastvalue INTEGER NOT NULL,
    updateddate TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    CONSTRAINT code_sequences_pkey PRIMARY KEY (scopekey)
);
