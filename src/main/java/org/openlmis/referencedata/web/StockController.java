package org.openlmis.referencedata.web;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.service.StockService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class StockController {

  @Autowired
  StockService stockService;

  /**
   * Finds Stocks matching all of provided parameters.
   * @param product product of searched Stocks.
   * @return ResponseEntity with list of all Stocks matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/stocks/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchUsers(
          @RequestParam(value = "product", required = false) Product product) {
    List<Stock> result = stockService.searchStocks(product);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
