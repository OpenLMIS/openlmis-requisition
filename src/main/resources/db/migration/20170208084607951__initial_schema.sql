--
-- Name: available_non_full_supply_products; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE available_non_full_supply_products (
    requisitionid uuid NOT NULL,
    value bytea
);


--
-- Name: available_requisition_column_options; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE available_requisition_column_options (
    id uuid NOT NULL,
    optionlabel character varying(255) NOT NULL,
    optionname character varying(255) NOT NULL,
    columnid uuid NOT NULL
);


--
-- Name: available_requisition_column_sources; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE available_requisition_column_sources (
    columnid uuid NOT NULL,
    value character varying(255)
);


--
-- Name: available_requisition_columns; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE available_requisition_columns (
    id uuid NOT NULL,
    canbechangedbyuser boolean,
    canchangeorder boolean,
    columntype character varying(255) NOT NULL,
    definition text,
    indicator character varying(255),
    isdisplayrequired boolean,
    label character varying(255),
    mandatory boolean,
    name character varying(255)
);


--
-- Name: columns_maps; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE columns_maps (
    requisitiontemplateid uuid NOT NULL,
    requisitioncolumnid uuid NOT NULL,
    definition text,
    displayorder integer NOT NULL,
    indicator character varying(255),
    isdisplayed boolean,
    label character varying(255),
    name character varying(255),
    requisitioncolumnoptionid uuid,
    source integer NOT NULL,
    key character varying(255) NOT NULL
);


--
-- Name: configuration_settings; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE configuration_settings (
    key character varying(255) NOT NULL,
    value character varying(255) NOT NULL
);


--
-- Name: jasper_templates; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE jasper_templates (
    id uuid NOT NULL,
    data bytea,
    description text,
    name text NOT NULL,
    type text
);


--
-- Name: previous_adjusted_consumptions; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE previous_adjusted_consumptions (
    requisitionlineitemid uuid NOT NULL,
    previousadjustedconsumption integer
);


--
-- Name: requisition_line_items; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE requisition_line_items (
    id uuid NOT NULL,
    adjustedconsumption integer,
    approvedquantity integer,
    averageconsumption integer,
    beginningbalance integer,
    calculatedorderquantity integer,
    maxperiodsofstock numeric(19,2),
    maximumstockquantity integer,
    nonfullsupply boolean NOT NULL,
    numberofnewpatientsadded integer,
    orderableid uuid,
    packstoship bigint,
    priceperpack numeric(19,2),
    remarks character varying(250),
    requestedquantity integer,
    requestedquantityexplanation character varying(255),
    skipped boolean,
    stockonhand integer,
    total integer,
    totalconsumedquantity integer,
    totalcost numeric(19,2),
    totallossesandadjustments integer,
    totalreceivedquantity integer,
    totalstockoutdays integer,
    requisitionid uuid
);


--
-- Name: requisition_templates; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE requisition_templates (
    id uuid NOT NULL,
    createddate timestamp with time zone,
    modifieddate timestamp with time zone,
    numberofperiodstoaverage integer,
    programid uuid
);


--
-- Name: requisitions; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE requisitions (
    id uuid NOT NULL,
    createddate timestamp with time zone,
    modifieddate timestamp with time zone,
    authorizeddate timestamp with time zone,
    authorizerid uuid,
    creatorid uuid NOT NULL,
    draftstatusmessage character varying(255),
    emergency boolean NOT NULL,
    facilityid uuid NOT NULL,
    numberofmonthsinperiod integer NOT NULL,
    processingperiodid uuid NOT NULL,
    programid uuid NOT NULL,
    status character varying(255) NOT NULL,
    submitteddate timestamp with time zone,
    submitterid uuid,
    supervisorynodeid uuid,
    supplyingfacilityid uuid,
    templateid uuid NOT NULL
);


--
-- Name: requisitions_previous_requisitions; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE requisitions_previous_requisitions (
    requisitionid uuid NOT NULL,
    previousrequisitionid uuid NOT NULL
);


--
-- Name: status_messages; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE status_messages (
    id uuid NOT NULL,
    createddate timestamp with time zone,
    modifieddate timestamp with time zone,
    authorfirstname character varying(255),
    authorid uuid,
    authorlastname character varying(255),
    body character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    requisitionid uuid NOT NULL
);


--
-- Name: stock_adjustments; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE stock_adjustments (
    id uuid NOT NULL,
    quantity integer NOT NULL,
    reasonid bytea NOT NULL,
    requisitionlineitemid uuid
);


--
-- Name: template_parameters; Type: TABLE; Schema: requisition; Owner: postgres; Tablespace: 
--

CREATE TABLE template_parameters (
    id uuid NOT NULL,
    datatype text,
    defaultvalue text,
    description text,
    displayname text,
    name text,
    selectsql text,
    templateid uuid NOT NULL
);


--
-- Name: available_requisition_column_options_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY available_requisition_column_options
    ADD CONSTRAINT available_requisition_column_options_pkey PRIMARY KEY (id);


--
-- Name: available_requisition_columns_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY available_requisition_columns
    ADD CONSTRAINT available_requisition_columns_pkey PRIMARY KEY (id);


--
-- Name: columns_maps_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY columns_maps
    ADD CONSTRAINT columns_maps_pkey PRIMARY KEY (requisitiontemplateid, key);


--
-- Name: configuration_settings_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY configuration_settings
    ADD CONSTRAINT configuration_settings_pkey PRIMARY KEY (key);


--
-- Name: jasper_templates_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY jasper_templates
    ADD CONSTRAINT jasper_templates_pkey PRIMARY KEY (id);


--
-- Name: requisition_line_items_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY requisition_line_items
    ADD CONSTRAINT requisition_line_items_pkey PRIMARY KEY (id);


--
-- Name: requisition_templates_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY requisition_templates
    ADD CONSTRAINT requisition_templates_pkey PRIMARY KEY (id);


--
-- Name: requisitions_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY requisitions
    ADD CONSTRAINT requisitions_pkey PRIMARY KEY (id);


--
-- Name: status_messages_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY status_messages
    ADD CONSTRAINT status_messages_pkey PRIMARY KEY (id);


--
-- Name: stock_adjustments_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY stock_adjustments
    ADD CONSTRAINT stock_adjustments_pkey PRIMARY KEY (id);


--
-- Name: template_parameters_pkey; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY template_parameters
    ADD CONSTRAINT template_parameters_pkey PRIMARY KEY (id);


--
-- Name: uk_5878s5vb2v4y53vun95nrdvgw; Type: CONSTRAINT; Schema: requisition; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY jasper_templates
    ADD CONSTRAINT uk_5878s5vb2v4y53vun95nrdvgw UNIQUE (name);


--
-- Name: fk_10n2ij8p9q2oyfsn3jma3q85n; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY requisitions_previous_requisitions
    ADD CONSTRAINT fk_10n2ij8p9q2oyfsn3jma3q85n FOREIGN KEY (previousrequisitionid) REFERENCES requisitions(id);


--
-- Name: fk_1k2xr4hbgipw4126xw4pgy70e; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY columns_maps
    ADD CONSTRAINT fk_1k2xr4hbgipw4126xw4pgy70e FOREIGN KEY (requisitiontemplateid) REFERENCES requisition_templates(id);


--
-- Name: fk_1ytg3dn9rcjam5mv9h6u1x14e; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY requisitions
    ADD CONSTRAINT fk_1ytg3dn9rcjam5mv9h6u1x14e FOREIGN KEY (templateid) REFERENCES requisition_templates(id);


--
-- Name: fk_4sg1naierwgt9avsjcm76a2yl; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY requisition_line_items
    ADD CONSTRAINT fk_4sg1naierwgt9avsjcm76a2yl FOREIGN KEY (requisitionid) REFERENCES requisitions(id);


--
-- Name: fk_9nqi8imo7ty6jafeijhviynrt; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY stock_adjustments
    ADD CONSTRAINT fk_9nqi8imo7ty6jafeijhviynrt FOREIGN KEY (requisitionlineitemid) REFERENCES requisition_line_items(id);


--
-- Name: fk_a6nm2s34wa449q2i3crk3mlc0; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY columns_maps
    ADD CONSTRAINT fk_a6nm2s34wa449q2i3crk3mlc0 FOREIGN KEY (requisitioncolumnoptionid) REFERENCES available_requisition_column_options(id);


--
-- Name: fk_au3xmstd4bn77xeia175ftmuc; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY available_requisition_column_sources
    ADD CONSTRAINT fk_au3xmstd4bn77xeia175ftmuc FOREIGN KEY (columnid) REFERENCES available_requisition_columns(id);


--
-- Name: fk_b8078votirpsmh2cpuvm0oull; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY available_non_full_supply_products
    ADD CONSTRAINT fk_b8078votirpsmh2cpuvm0oull FOREIGN KEY (requisitionid) REFERENCES requisitions(id);


--
-- Name: fk_gwot77t2t7y1am93r3qvuyy7u; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY available_requisition_column_options
    ADD CONSTRAINT fk_gwot77t2t7y1am93r3qvuyy7u FOREIGN KEY (columnid) REFERENCES available_requisition_columns(id);


--
-- Name: fk_hp6wryw9250cf3jhceddvmn5b; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY status_messages
    ADD CONSTRAINT fk_hp6wryw9250cf3jhceddvmn5b FOREIGN KEY (requisitionid) REFERENCES requisitions(id);


--
-- Name: fk_k7b90206ee1t5nl8iea8q0ij8; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY columns_maps
    ADD CONSTRAINT fk_k7b90206ee1t5nl8iea8q0ij8 FOREIGN KEY (requisitioncolumnid) REFERENCES available_requisition_columns(id);


--
-- Name: fk_ofrpexcgp8i7ppwit5kbs0ryr; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY previous_adjusted_consumptions
    ADD CONSTRAINT fk_ofrpexcgp8i7ppwit5kbs0ryr FOREIGN KEY (requisitionlineitemid) REFERENCES requisition_line_items(id);


--
-- Name: fk_pg6tsrnawyhqelfg6tb6fq4f5; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY requisitions_previous_requisitions
    ADD CONSTRAINT fk_pg6tsrnawyhqelfg6tb6fq4f5 FOREIGN KEY (requisitionid) REFERENCES requisitions(id);


--
-- Name: fk_qww3p7ho2t5jyutkllrh64khr; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY template_parameters
    ADD CONSTRAINT fk_qww3p7ho2t5jyutkllrh64khr FOREIGN KEY (templateid) REFERENCES jasper_templates(id);
