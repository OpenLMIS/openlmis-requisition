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
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.IN_APPROVAL;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.REJECTED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Lists;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import javax.persistence.EntityManager;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
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

    addStatusChanges(requisition, 0);

    StockAdjustmentReason reason = generateStockAdjustmentReason();
    requisition.setStockAdjustmentReasons(newArrayList(reason));

    programNodePairs.add(new ImmutablePair<>(programId, requisition.getSupervisoryNodeId()));

    return requisition;
  }

  protected StockAdjustmentReason generateStockAdjustmentReason() {
    StockAdjustmentReason reason = new StockAdjustmentReasonDataBuilder()
        .withIsFreeTextAllowed(false)
        .withName(RandomStringUtils.random(5))
        .build();
    return reason;
  }

  /**
   * Adds status changes for the given requisition.
   *
   * @param requisition the given requisition
   * @param rejectedCount define how many times the given requisition should be rejected. Zero means
   *                      that the given requisition will not have rejected status change.
   */
  void addStatusChanges(Requisition requisition, int rejectedCount) {
    requisition.setStatusChanges(Lists.newArrayList());

    if (requisition.getStatus() == RequisitionStatus.INITIATED) {
      addStatusChangesForInitiatedRequisition(requisition);
    } else if (requisition.getStatus() == RequisitionStatus.SKIPPED) {
      addStatusChangesForSkippedRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.REJECTED) {
      addStatusChangesForRejectedRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.SUBMITTED) {
      addStatusChangesForSubmittedRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      addStatusChangesForAuthorizedRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.IN_APPROVAL) {
      addStatusChangesForInApprovalRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.APPROVED) {
      addStatusChangesForApprovedRequisition(requisition, rejectedCount);
    } else if (requisition.getStatus() == RequisitionStatus.RELEASED
        || requisition.getStatus() == RequisitionStatus.RELEASED_WITHOUT_ORDER) {
      addStatusChangesForReleasedRequisition(requisition, rejectedCount);
    }
  }

  private void addStatusChangesForInitiatedRequisition(Requisition requisition) {
    addStatusChange(requisition, INITIATED);
  }

  private void addStatusChangesForRejectedRequisition(Requisition requisition,
      int rejectedCount) {
    addStatusChangesForInitiatedRequisition(requisition);

    for (int i = 0; i < rejectedCount; ++i) {
      addStatusChange(requisition, SUBMITTED);
      addStatusChange(requisition, AUTHORIZED);
      addStatusChange(requisition, IN_APPROVAL);
      addStatusChange(requisition, REJECTED);
    }
  }

  private void addStatusChangesForSkippedRequisition(Requisition requisition,
      int rejectedCount) {
    addStatusChangesForRejectedRequisition(requisition, rejectedCount);
    addStatusChange(requisition, SKIPPED);
  }

  private void addStatusChangesForSubmittedRequisition(Requisition requisition,
      int rejectedCount) {
    addStatusChangesForRejectedRequisition(requisition, rejectedCount);
    addStatusChange(requisition, SUBMITTED);
  }

  private void addStatusChangesForAuthorizedRequisition(Requisition requisition,
      int rejectedCount) {
    addStatusChangesForSubmittedRequisition(requisition, rejectedCount);
    addStatusChange(requisition, AUTHORIZED);
  }

  private void addStatusChangesForInApprovalRequisition(Requisition requisition,
      int rejectedCount) {
    addStatusChangesForAuthorizedRequisition(requisition, rejectedCount);
    addStatusChange(requisition, IN_APPROVAL);
  }

  private void addStatusChangesForApprovedRequisition(Requisition requisition, int rejectedCount) {
    addStatusChangesForInApprovalRequisition(requisition, rejectedCount);
    addStatusChange(requisition, APPROVED);
  }

  private void addStatusChangesForReleasedRequisition(Requisition requisition, int rejectedCount) {
    addStatusChangesForApprovedRequisition(requisition, rejectedCount);
    addStatusChange(requisition, requisition.getStatus());
  }

  private void addStatusChange(Requisition requisition, RequisitionStatus status) {
    StatusChange statusChange = new StatusChangeDataBuilder()
        .withRequisition(requisition)
        .withStatus(status)
        .buildAsNew();

    requisition.getStatusChanges().add(statusChange);

    try {
      // we use createdDate which is set automatically before saving into the database
      // because of that we wait some time between statuses.
      TimeUnit.MILLISECONDS.sleep(10);
    } catch (InterruptedException exp) {
      Thread.currentThread().interrupt();
      throw new IllegalStateException(exp);
    }
  }
}
