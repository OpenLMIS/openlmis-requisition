package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.service.ProgramProductService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.client.RestClientException;

import java.util.List;
import java.util.UUID;

@Controller
public class ProgramProductController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProgramProductController.class);

  @Autowired
  private ProgramProductService programProductService;

  @Autowired
  private ProgramProductRepository programProductRepository;

  /**
   * Allows creating new programProducts.
   *
   * @param programProduct A programProduct bound to the request body
   * @return ResponseEntity containing the created programProduct
   */
  @RequestMapping(value = "/programProducts", method = RequestMethod.POST)
  public ResponseEntity<?> createProgramProduct(@RequestBody ProgramProduct programProduct) {
    try {
      LOGGER.debug("Creating new programProduct");
      // Ignore provided id
      programProduct.setId(null);
      ProgramProduct newProgramProduct = programProductRepository.save(programProduct);
      return new ResponseEntity<ProgramProduct>(newProgramProduct, HttpStatus.CREATED);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating programProduct", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all programProducts.
   *
   * @return ProgramProduct.
   */
  @RequestMapping(value = "/programProducts", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProgramProducts() {
    Iterable<ProgramProduct> programProducts = programProductRepository.findAll();
    if (programProducts == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(programProducts, HttpStatus.OK);
    }
  }

  /**
   * Allows updating programProducts.
   *
   * @param programProduct A programProduct bound to the request body
   * @param programProductId UUID of programProduct which we want to update
   * @return ResponseEntity containing the updated programProduct
   */
  @RequestMapping(value = "/programProducts/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProgramProduct(@RequestBody ProgramProduct programProduct,
                                                 @PathVariable("id") UUID programProductId) {
    try {
      LOGGER.debug("Updating programProduct");
      ProgramProduct updatedProgramProduct = programProductRepository.save(programProduct);
      return new ResponseEntity<ProgramProduct>(updatedProgramProduct, HttpStatus.OK);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating programProduct", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen programProduct.
   *
   * @param programProductId UUID of programProduct which we want to get
   * @return ProgramProduct.
   */
  @RequestMapping(value = "/programProducts/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProgramProduct(@PathVariable("id") UUID programProductId) {
    ProgramProduct programProduct = programProductRepository.findOne(programProductId);
    if (programProduct == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(programProduct, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting programProduct.
   *
   * @param programProductId UUID of programProduct which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/programProducts/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProgramProduct(@PathVariable("id") UUID programProductId) {
    ProgramProduct programProduct = programProductRepository.findOne(programProductId);
    if (programProduct == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        programProductRepository.delete(programProduct);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("ProgramProduct cannot be deleted because of existing dependencies",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<ProgramProduct>(HttpStatus.NO_CONTENT);
    }
  }

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
