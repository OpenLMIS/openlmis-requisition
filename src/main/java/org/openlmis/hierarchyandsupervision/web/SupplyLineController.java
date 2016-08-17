package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
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
   * @param program program of searched Supply Lines.
   * @param supervisoryNode supervisory node of searched Supply Lines.
   * @return ResponseEntity with list of all Supply Lines matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/supplyLines/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchSupplyLines(
      @RequestParam(value = "program", required = true) Program program,
      @RequestParam(value = "supervisoryNode", required = true) SupervisoryNode supervisoryNode) {
    List<SupplyLine> result = supplyLineService.searchSupplyLines(program, supervisoryNode);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
