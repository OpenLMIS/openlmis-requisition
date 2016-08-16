package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.service.ProgramProductService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class ProgramProductController {

  @Autowired
  ProgramProductService programProductService;

  /**
   * Finds ProgramProducts matching all of provided parameters.
   * @param program program of ProgramProducts we want search.
   * @param fullSupply is the searched ProgramProducts fullSuplly.
   * @return ResponseEntity with list of all ProgramProducts matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/programProducts/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchProgramProducts(
          @RequestParam(value = "program", required = true) Program program,
          @RequestParam(value = "fullSupply", required = false) Boolean fullSupply) {
    List<ProgramProduct> result = programProductService.searchProgramProducts(program, fullSupply);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
