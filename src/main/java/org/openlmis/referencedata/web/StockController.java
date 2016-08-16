package org.openlmis.referencedata.web;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.StockRepository;
import org.openlmis.referencedata.service.StockService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.UUID;

@RepositoryRestController
public class StockController {
  private Logger logger = LoggerFactory.getLogger(ScheduleController.class);

  @Autowired
  private StockService stockService;

  @Autowired
  private StockRepository stockRepository;

  /**
   * Allows creating new stocks.
   *
   * @param stock A stock bound to the request body
   * @return ResponseEntity containing the created stock
   */
  @RequestMapping(value = "/stocks", method = RequestMethod.POST)
  public ResponseEntity<?> createStock(@RequestBody Stock stock) {
    if (stock == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Creating new stock");
      // Ignore provided id
      stock.setId(null);
      Stock newStock = stockRepository.save(stock);
      return new ResponseEntity<Stock>(newStock, HttpStatus.CREATED);
    }
  }

  /**
   * Allows deleting stock.
   *
   * @param stockId UUID of stock whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/stocks/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteStock(@PathVariable("id") UUID stockId) {
    Stock stock = stockRepository.findOne(stockId);
    if (stock == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Deleting choosen stock");
      stockRepository.delete(stock);
      return new ResponseEntity<Stock>(HttpStatus.NO_CONTENT);
    }
  }

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
