package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.custom.OrderRepositoryCustom;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface OrderRepository extends PagingAndSortingRepository<Order, UUID>,
        OrderRepositoryCustom {

  String SUPPLYING_FACILITY = "supplyingFacility";
  String REQUESTING_FACILITY = "requestingFacility";
  String PROGRAM = "program";

  Iterable<Order> findBySupplyingFacility(@Param(SUPPLYING_FACILITY) FacilityDto supplyingFacility);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacility(
      @Param(SUPPLYING_FACILITY) FacilityDto supplyingFacility,
      @Param(REQUESTING_FACILITY) FacilityDto requestingFacility);

  Iterable<Order> findBySupplyingFacilityAndProgram(
      @Param(SUPPLYING_FACILITY) FacilityDto supplyingFacility,
      @Param(PROGRAM) ProgramDto program);

  Iterable<Order> findBySupplyingFacilityAndRequestingFacilityAndProgram(
          @Param(SUPPLYING_FACILITY) FacilityDto supplyingFacility,
          @Param(REQUESTING_FACILITY) FacilityDto requestingFacility,
          @Param(PROGRAM) ProgramDto program);
}
