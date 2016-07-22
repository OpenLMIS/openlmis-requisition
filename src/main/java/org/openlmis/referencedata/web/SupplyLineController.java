package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@RepositoryRestController
public class SupplyLineController {

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  /**
   * Saving SupplyLine.
   */
  @RequestMapping(value = "/supplyLines", method = RequestMethod.POST)
  public ResponseEntity<?> saveSupplyLine(@RequestBody SupplyLine supplyLine) {
    SupplyLine newSupplyLine = supplyLineRepository.save(supplyLine);
    return new ResponseEntity<>(newSupplyLine, HttpStatus.OK);
  }
}
