--
-- Data for Name: available_requisition_columns; Type: TABLE DATA; Schema: requisition; Owner: postgres
--

INSERT INTO requisition.available_requisition_columns (id, name, label, indicator, mandatory, isDisplayRequired, canChangeOrder, canBeChangedByUser, columnType, definition) VALUES('aa0a1a7e-e5cb-4385-b781-943316fa116a', 'idealStockAmount', 'Ideal Stock Amount', 'G', false, false, true, true, 'NUMERIC', 'The Ideal Stock Amount is the target quantity for a specific commodity type, facility, and period.');

--
-- Data for Name: available_requisition_column_sources; Type: TABLE DATA; Schema: requisition; Owner: postgres
--

INSERT INTO requisition.available_requisition_column_sources (columnId, value) VALUES ('aa0a1a7e-e5cb-4385-b781-943316fa116a', 'REFERENCE_DATA');