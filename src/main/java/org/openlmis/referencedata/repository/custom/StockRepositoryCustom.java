package org.openlmis.referencedata.repository.custom;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;

import java.util.List;

public interface StockRepositoryCustom {

  List<Stock> searchStocks(Product product);
}
