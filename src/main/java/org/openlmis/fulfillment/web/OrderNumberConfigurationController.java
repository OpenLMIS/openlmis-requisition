package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.requisition.web.BaseController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.Iterator;

@Controller
public class OrderNumberConfigurationController extends BaseController {

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
    OrderNumberConfiguration orderNumberConfiguration =
        orderNumberConfigurationRepository.save(orderNumberConfigurationDto);
    return new ResponseEntity<>(orderNumberConfiguration, HttpStatus.OK);
  }

  /**
   * Get orderNumberConfiguration.
   *
   * @return OrderNumberConfiguration.
   */
  @RequestMapping(value = "/orderNumberConfigurations", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<OrderNumberConfiguration> getOrderFileTemplate() {
    Iterator<OrderNumberConfiguration> it = orderNumberConfigurationRepository.findAll().iterator();

    if (!it.hasNext()) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    return new ResponseEntity<>(it.next(), HttpStatus.OK);
  }
}
