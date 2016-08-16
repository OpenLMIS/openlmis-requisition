package org.openlmis.requisition.web;


import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class RequisitionTemplateController {

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  /**
   * Returns all requisition templates with matched parameters.
   * @param program program of searched requisition templates.
   * @return ResponseEntity with list of all requisition templates matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/requisitionTemplates/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitionTemplates(
      @RequestParam(value = "program", required = false) Program program) {
    List<RequisitionTemplate> result
        = requisitionTemplateService.searchRequisitionTemplates(program);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
