package org.openlmis.fulfillment.repository;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.fulfillment.domain.Order;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface OrderRepository extends PagingAndSortingRepository<Order, UUID> {
  @Override
  @RestResource(exported = false)
  void delete(Order entity);

  @Override
  @RestResource(exported = false)
  void delete(UUID id);

  @Override
  @RestResource(exported = false)
  void delete(Iterable<? extends Order> entities);

  @Override
  @RestResource(exported = false)
  void deleteAll();

  String supplyingFacilityString = "supplyingFacility";

  Iterable<Order> findBySupplyingFacility(@Param(supplyingFacilityString) Facility supplyingFacility);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacility(@Param(supplyingFacilityString) Facility supplyingFacility,
                                                    @Param("requestingFacility") Facility requestingFacility);

  Iterable<Order> findBySupplyingFacilityAndProgram(@Param(supplyingFacilityString) Facility supplyingFacility,
                                                    @Param("program") Program program);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacilityAndProgram(
          @Param(supplyingFacilityString) Facility supplyingFacility,
          @Param("requestingFacility") Facility requestingFacility,
          @Param("program") Program program);
}
