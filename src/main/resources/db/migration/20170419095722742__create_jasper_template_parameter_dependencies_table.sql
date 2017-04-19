--
-- Name: jaspertemplateparameter_options; Type: TABLE; Schema: reports; Owner: postgres
--

CREATE TABLE jasper_template_parameter_dependencies (
    id uuid NOT NULL,
    parameterid uuid NOT NULL,
    dependency text NOT NULL,
    placeholder text NOT NULL
);

--
-- Name: jaspertemplateparameter_options fkpxphnoksec55h63evgb3obfxq; Type: FK CONSTRAINT; Schema: reports; Owner: postgres
--

ALTER TABLE ONLY jasper_template_parameter_dependencies
    ADD CONSTRAINT fkpxphnoksec55h63evgb3obfxq FOREIGN KEY (parameterid) REFERENCES template_parameters(id);
