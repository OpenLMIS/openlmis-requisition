package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.custom.OrderRepositoryCustom;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface OrderRepository extends PagingAndSortingRepository<Order, UUID>,
        OrderRepositoryCustom {

  String SUPPLYING_FACILITY = "supplyingFacility";
  String REQUESTING_FACILITY = "requestingFacility";
  String PROGRAM = "program";

  Iterable<Order> findBySupplyingFacility(@Param(SUPPLYING_FACILITY) Facility supplyingFacility);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacility(
      @Param(SUPPLYING_FACILITY) Facility supplyingFacility,
      @Param(REQUESTING_FACILITY) Facility requestingFacility);

  Iterable<Order> findBySupplyingFacilityAndProgram(
      @Param(SUPPLYING_FACILITY) Facility supplyingFacility,
      @Param(PROGRAM) Program program);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacilityAndProgram(
          @Param(SUPPLYING_FACILITY) Facility supplyingFacility,
          @Param(REQUESTING_FACILITY) Facility requestingFacility,
          @Param(PROGRAM) Program program);
}
