package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.web.BaseController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class OrderNumberConfigurationController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderNumberConfiguration.class);

  @Autowired
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  /**
   * Saves given OrderNumberConfiguration to database.
   * @param orderNumberConfigurationDto object to save.
   * @return Response entity with Http status code.
   */
  @RequestMapping(value = "/orderNumberConfigurations", method = RequestMethod.POST)
  public ResponseEntity<?> saveOrderNumberConfigurations(
      @RequestBody OrderNumberConfiguration orderNumberConfigurationDto) {
    try {
      OrderNumberConfiguration orderNumberConfiguration =
          orderNumberConfigurationRepository.save(orderNumberConfigurationDto);
      return new ResponseEntity<>(orderNumberConfiguration, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
          new ErrorResponse("An error occurred while saving order", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }
}
