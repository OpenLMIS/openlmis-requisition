package org.openlmis.referencedata.web;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.service.OrderService;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;
import java.util.UUID;

@RepositoryRestController
public class FacilityController {
  Logger logger = LoggerFactory.getLogger(FacilityController.class);

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private OrderService orderService;

  /**
   * Return list of orders, filtered according to params, filled by certain facility.
   *
   * @param homeFacilityId UUID of facility whose list of orders we want
   * @param programId UUID of program we filter by
   * @param requestingFacilityId UUID of requesting facility we filter by
   * @param request HttpServletRequest object
   * @return result Iterable object with filtered orders
   */
  @RequestMapping(value = "/facilities/{id}/orders", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getOrderList(
          @PathVariable("id") UUID homeFacilityId,
          @RequestParam(value = "program", required = false) UUID programId,
          @RequestParam(value = "facility", required = false) UUID requestingFacilityId,
          HttpServletRequest request) {
    Program program = null;
    Facility requestingFacility = null;
    Facility homeFacility = facilityRepository.findOne(homeFacilityId);
    if (homeFacility == null) {
      return new ResponseEntity("F" +
              "acility with provided id does not exist.", HttpStatus.BAD_REQUEST);
    }
    if (requestingFacilityId != null) {
      requestingFacility = facilityRepository.findOne(requestingFacilityId);
    }
    if (programId != null) {
      program = programRepository.findOne(programId);
    }
    Iterable<Order> iterableOrder = orderService.searchOrders(
            homeFacility,requestingFacility,program);
    return new ResponseEntity<Object>(iterableOrder, HttpStatus.OK);
  }
}
