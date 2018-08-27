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

import static com.google.common.collect.Lists.newArrayList;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import javax.persistence.EntityManager;
import org.apache.commons.lang.RandomStringUtils;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonType;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class BaseRequisitionRepositoryIntegrationTest extends
    BaseCrudRepositoryIntegrationTest<Requisition> {

  @Autowired
  protected RequisitionRepository repository;

  @Autowired
  protected RequisitionTemplateRepository templateRepository;

  @Autowired
  protected EntityManager entityManager;

  protected RequisitionTemplate testTemplate;

  protected List<String> userPermissionStrings = new ArrayList<>();

  @Override
  RequisitionRepository getRepository() {
    return this.repository;
  }

  @Override
  Requisition generateInstance() {
    return generateInstance(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID());
  }

  protected Requisition generateInstance(UUID facilityId, UUID programId, UUID processingPeriodId) {
    // Add permission to user for each facility and program
    userPermissionStrings.add("REQUISITION_VIEW|" + facilityId + "|" + programId);

    Requisition requisition = new Requisition(facilityId, programId, processingPeriodId,
        INITIATED, getNextInstanceNumber() % 2 == 0);
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setSupervisoryNodeId(UUID.randomUUID());
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setTemplate(testTemplate);
    requisition.setDraftStatusMessage(RandomStringUtils.randomAlphanumeric(500));
    List<StatusChange> statusChanges = new ArrayList<>();
    statusChanges.add(StatusChange.newStatusChange(requisition, UUID.randomUUID()));
    requisition.setStatusChanges(statusChanges);

    StockAdjustmentReason reason = generateStockAdjustmentReason();
    requisition.setStockAdjustmentReasons(newArrayList(reason));

    return requisition;
  }

  protected StockAdjustmentReason generateStockAdjustmentReason() {
    StockAdjustmentReason reason = new StockAdjustmentReason();
    reason.setReasonId(UUID.randomUUID());
    reason.setReasonCategory(ReasonCategory.ADJUSTMENT);
    reason.setReasonType(ReasonType.CREDIT);
    reason.setDescription("simple description");
    reason.setIsFreeTextAllowed(false);
    reason.setName(RandomStringUtils.random(5));
    return reason;
  }
}
