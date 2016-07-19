package org.openlmis.referencedata.repository;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface StockRepository extends PagingAndSortingRepository<Stock, UUID> {
  Stock findByStockInventoryAndProduct(
      @Param("facilityId") StockInventory facility,
      @Param("productId") Product product
  );
}
