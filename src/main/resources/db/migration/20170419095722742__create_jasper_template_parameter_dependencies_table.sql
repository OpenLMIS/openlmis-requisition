--
-- Name: jasper_template_parameter_dependencies; Type: TABLE; Schema: requisition; Owner: postgres
--

CREATE TABLE jasper_template_parameter_dependencies (
    id uuid NOT NULL,
    parameterid uuid NOT NULL,
    dependency text NOT NULL,
    placeholder text NOT NULL
);

--
-- Name: jasper_template_parameter_dependencies fkpxphnoksec55h63evgb3obfxq; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY jasper_template_parameter_dependencies
    ADD CONSTRAINT fk_parameterid_template_parameters FOREIGN KEY (parameterid) REFERENCES template_parameters(id);
