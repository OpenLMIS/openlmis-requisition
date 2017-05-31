/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.repository;

import static org.openlmis.requisition.domain.Requisition.EMERGENCY;
import static org.openlmis.requisition.domain.Requisition.FACILITY_ID;
import static org.openlmis.requisition.domain.Requisition.PROCESSING_PERIOD_ID;
import static org.openlmis.requisition.domain.Requisition.PROGRAM_ID;

import java.util.List;
import java.util.UUID;
import org.javers.spring.annotation.JaversSpringDataAuditable;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

@JaversSpringDataAuditable
public interface RequisitionRepository extends
    PagingAndSortingRepository<Requisition, UUID>,
    RequisitionRepositoryCustom {
  List<Requisition> findByTemplateId(@Param("templateId") UUID templateId);

  @Query(value = "SELECT COUNT(*) FROM requisition.requisitions "
      + "WHERE " + PROCESSING_PERIOD_ID + " = :periodId AND "
      + FACILITY_ID + " = :facilityId AND "
      + PROGRAM_ID + " = :programId AND "
      + EMERGENCY + " = :emergency",
      nativeQuery = true)
  int getRequisitionsCount(@Param("periodId") UUID periodId,
                           @Param("facilityId") UUID facilityId,
                           @Param("programId") UUID programId,
                           @Param("emergency") boolean emergency);

}
