package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.fulfillment.repository.OrderFileTemplateRepository;
import org.openlmis.fulfillment.service.OrderFileTemplateService;
import org.openlmis.fulfillment.validate.OrderFileTemplateValidator;
import org.openlmis.requisition.web.BaseController;
import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.HashMap;
import java.util.Map;

import javax.validation.Valid;

@Controller
public class OrderFileTemplateController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderFileTemplateController.class);

  @Autowired
  private OrderFileTemplateValidator validator;

  @InitBinder
  protected void initBinder(WebDataBinder binder) {
    binder.setValidator(this.validator);
  }

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
      @RequestBody @Valid OrderFileTemplate orderFileTemplate, BindingResult bindingResult) {
    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
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

  private Map<String, String> getErrors(final BindingResult bindingResult) {
    return new HashMap<String, String>() {
      {
        for (FieldError error : bindingResult.getFieldErrors()) {
          put(error.getField(), error.getDefaultMessage());
        }
      }
    };
  }
}
