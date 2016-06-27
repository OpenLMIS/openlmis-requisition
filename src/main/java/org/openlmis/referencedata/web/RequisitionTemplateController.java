package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.RequisitionTemplate;
import org.openlmis.referencedata.repository.RequisitionTemplateRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.ArrayList;
import java.util.UUID;

@RepositoryRestController
public class RequisitionTemplateController {
    private Logger logger = LoggerFactory.getLogger(RequisitionTemplateController.class);

    @Autowired
    private RequisitionTemplateRepository requisitionTemplateRepository;

    /**
     * Allows creating new programs.
     *
     * @param requisitionTemplate A requisition template bound to the request body
     * @return ResponseEntity containing the created program
     */
    @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.POST)
    public ResponseEntity<?> createRequisitionTemplate(@RequestBody RequisitionTemplate requisitionTemplate) {
        if (requisitionTemplate == null) {
            return new ResponseEntity(HttpStatus.BAD_REQUEST);
        } else {
            logger.debug("Creating new requisitionTemplate");
            // Ignore provided id
            requisitionTemplate.setId(null);

            //ArrayList<RequisitionTemplate> templates =
            //        (ArrayList<RequisitionTemplate>)requisitionTemplateRepository.findByRemarks(requisitionTemplate.getRemarks());



            RequisitionTemplate newRequisitionTemplate = requisitionTemplateRepository.save(requisitionTemplate);
            return new ResponseEntity<RequisitionTemplate>(newRequisitionTemplate, HttpStatus.CREATED);
        }
    }
}
