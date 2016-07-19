package org.openlmis.referencedata.repository;


import org.openlmis.referencedata.domain.StockInventory;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface StockInventoryRepository extends ReferenceDataRepository<StockInventory, UUID> {

  @Override
  @RestResource
  <S extends StockInventory> S save(S entity);

  @Override
  @RestResource
  <S extends StockInventory> Iterable<S> save(Iterable<S> entities);
}
