--
-- Name: jaspertemplateparameter_options; Type: TABLE; Schema: requisition; Owner: postgres
--

CREATE TABLE jaspertemplateparameter_options (
    jaspertemplateparameterid uuid NOT NULL,
    options character varying(255)
);

--
-- Name: jaspertemplateparameter_options fkpxphnoksec55h63evgb3obfxq; Type: FK CONSTRAINT; Schema: requisition; Owner: postgres
--

ALTER TABLE ONLY jaspertemplateparameter_options
    ADD CONSTRAINT fkpxphnoksec55h63evgb3obfxq FOREIGN KEY (jaspertemplateparameterid) REFERENCES template_parameters(id);
