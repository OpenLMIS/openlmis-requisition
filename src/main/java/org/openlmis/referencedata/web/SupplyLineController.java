package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.service.SupplyLineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class SupplyLineController {

  @Autowired
  private SupplyLineService supplyLineService;

  /**
   * Returns all Supply Lines with matched parameters.
   */
  @RequestMapping(value = "/supplyLines/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchSupplyLines(
      @RequestParam(value = "program", required = true) Program program,
      @RequestParam(value = "supervisoryNode", required = true) SupervisoryNode supervisoryNode) {
    List<SupplyLine> result = supplyLineService.searchSupplyLines(program, supervisoryNode);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
