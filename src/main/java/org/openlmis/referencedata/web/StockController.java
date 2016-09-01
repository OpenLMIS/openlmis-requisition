package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.StockRepository;
import org.openlmis.referencedata.service.StockService;
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

import java.util.List;
import java.util.UUID;

@Controller
public class StockController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(StockController.class);

  @Autowired
  private StockService stockService;

  @Autowired
  private StockRepository stockRepository;

  /**
   * Allows creating new stocks.
   * If the id is specified, it will be ignored.
   *
   * @param stock A stock bound to the request body
   * @return ResponseEntity containing the created stock
   */
  @RequestMapping(value = "/stocks", method = RequestMethod.POST)
  public ResponseEntity<?> createStock(@RequestBody Stock stock) {
    try {
      LOGGER.debug("Creating new stock");
      stock.setId(null);
      Stock newStock = stockRepository.save(stock);
      LOGGER.debug("Created new stock with id: " + stock.getId());
      return new ResponseEntity<Stock>(newStock, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating stock", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all stocks.
   *
   * @return Stocks.
   */
  @RequestMapping(value = "/stocks", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllStocks() {
    Iterable<Stock> stocks = stockRepository.findAll();
    return new ResponseEntity<>(stocks, HttpStatus.OK);
  }

  /**
   * Allows updating stocks.
   *
   * @param stock A stock bound to the request body
   * @param stockId UUID of stock which we want to update
   * @return ResponseEntity containing the updated stock
   */
  @RequestMapping(value = "/stocks/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateStock(@RequestBody Stock stock,
                                       @PathVariable("id") UUID stockId) {
    try {
      LOGGER.debug("Updating stock with id: " + stockId);

      Stock stockToUpdate = stockRepository.findOne(stockId);

      if (stockToUpdate == null) {
        stockToUpdate = new Stock();
      }

      stockToUpdate.updateFrom(stock);
      stockToUpdate = stockRepository.save(stockToUpdate);

      LOGGER.debug("Updated stock with id: " + stockId);
      return new ResponseEntity<Stock>(stockToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating stock", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen stock.
   *
   * @param stockId UUID of stock which we want to get
   * @return Stock.
   */
  @RequestMapping(value = "/stocks/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getStock(@PathVariable("id") UUID stockId) {
    Stock stock = stockRepository.findOne(stockId);
    if (stock == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(stock, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting stock.
   *
   * @param stockId UUID of stock which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/stocks/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteStock(@PathVariable("id") UUID stockId) {
    Stock stock = stockRepository.findOne(stockId);
    if (stock == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        stockRepository.delete(stock);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting stock", ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
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
