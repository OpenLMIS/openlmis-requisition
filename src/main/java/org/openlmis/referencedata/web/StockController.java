package org.openlmis.referencedata.web;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
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
   * aaa.
   * @param stockInventory a.
   * @param product a.
   * @return a.
   */
  @RequestMapping(value = "/stocks/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchUsers(
          @RequestParam(value = "stockInventory", required = false) StockInventory stockInventory,
          @RequestParam(value = "product", required = false) Product product) {
    List<Stock> result = stockService.searchStocks(stockInventory, product);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
