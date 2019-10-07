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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.persistence.EntityManager;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.testutils.StatusChangeDataBuilder;
import org.openlmis.requisition.testutils.StockAdjustmentReasonDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;

@SuppressWarnings("PMD.TooManyMethods")
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

  protected Set<Pair<UUID, UUID>> programNodePairs = new HashSet<>();

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

    int nextInstanceNumber = getNextInstanceNumber();

    Requisition requisition = new RequisitionDataBuilder()
          .withFacilityId(facilityId)
          .withProgramId(programId)
          .withProcessingPeriodId(processingPeriodId)
          .withStatus(INITIATED)
          .withEmergency(nextInstanceNumber % 2 == 0)
          .withCreatedDate(ZonedDateTime.now())
          .withModifiedDate(ZonedDateTime.now())
          .withSupervisoryNodeId(UUID.randomUUID())
          .withNumberOfMonthsInPeriod(1)
          .withTemplate(testTemplate)
          .withDraftStatusMessage("draft status message " + nextInstanceNumber)
          .buildAsNew();

    StatusChange statusChange = new StatusChangeDataBuilder()
        .withRequisition(requisition)
        .withStatus(INITIATED)
        .buildAsNew();

    requisition.getStatusChanges().add(statusChange);

    StockAdjustmentReason reason = generateStockAdjustmentReason();
    requisition.setStockAdjustmentReasons(newArrayList(reason));

    programNodePairs.add(new ImmutablePair<>(programId, requisition.getSupervisoryNodeId()));

    return requisition;
  }

  private StockAdjustmentReason generateStockAdjustmentReason() {
    return new StockAdjustmentReasonDataBuilder()
        .withIsFreeTextAllowed(false)
        .withName(RandomStringUtils.random(5))
        .build();
  }

}
