package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.fulfillment.repository.OrderFileTemplateRepository;
import org.openlmis.fulfillment.service.OrderFileTemplateService;
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
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class OrderFileTemplateController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private OrderFileTemplateRepository orderFileTemplateRepository;

  @Autowired
  private OrderFileTemplateService orderFileTemplateService;

  /**
   * Allows creating or updating orderFileTemplate.
   *
   * @param orderFileTemplate A orderFileTemplate bound to the request body
   * @return ResponseEntity containing saved orderFileTemplate
   */
  @RequestMapping(value = "/orderFileTemplates", method = RequestMethod.POST)
  public ResponseEntity<?> savedOrderFileTemplate(
      @RequestBody OrderFileTemplate orderFileTemplate) {
    try {
      LOGGER.debug("Saving Order File Template");
      OrderFileTemplate savedTemplate = orderFileTemplateRepository.save(orderFileTemplate);
      LOGGER.debug("Saved Order File Template with id: " + orderFileTemplate.getId());
      return new ResponseEntity<>(savedTemplate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
          new ErrorResponse("An error occurred while saving Order File Template", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get orderFileTemplate.
   *
   * @return OrderFileTemplate.
   */
  @RequestMapping(value = "/orderFileTemplates", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<OrderFileTemplate> getOrderFileTemplate() {
    OrderFileTemplate orderFileTemplate = orderFileTemplateService.getOrderFileTemplate();
    if (orderFileTemplate == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(orderFileTemplate, HttpStatus.OK);
    }
  }
}
