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

package org.openlmis.requisition.web;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;
import static org.openlmis.requisition.i18n.MessageKeys.LINE_ITEM_SUPPLIED_BY_OTHER_PARTNER;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.SupplyPartnerAssociationDto;
import org.openlmis.requisition.dto.SupplyPartnerDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyPartnerReferenceDataService;
import org.openlmis.requisition.utils.Message;

@RequiredArgsConstructor
class RequisitionSplitter {
  private final SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;
  private final SupplyPartnerReferenceDataService supplyPartnerReferenceDataService;
  private final RequisitionRepository requisitionRepository;
  private final MessageService messageService;
  private final boolean active;

  private Requisition requisition;
  private Map<UUID, RequisitionLineItem> requisitionLineItems;
  private Set<UUID> programIds;
  private Set<UUID> facilityIds;
  private Set<UUID> orderableIds;

  @Setter
  private UUID supervisoryNodeId;
  private Set<UUID> partnerNodeIds;

  private List<SupplyPartnerAssociationDto> associations;

  /**
   * Checks if the given requisition is splittable. A requisition is splittable when all of the
   * following rules apply:
   * <ul>
   * <li>The requisition was not split before</li>
   * <li>The requisition is part of another requisition</li>
   * <li>Parent node has a partner node</li>
   * <li>There is a supply partner with an entry for that partner node and requisition's
   * program</li>
   * <li>That entry has a facility that matches the requisition's facility</li>
   * <li>That entry has a product that matches any products in the requisition line items</li>
   * </ul>
   *
   * @return true if requisition is splittable; otherwise false.
   * @throws NullPointerException if the requisition field was not set
   * @throws NullPointerException if a partner node id list was not set.
   */
  boolean isSplittable() {
    if (!active) {
      return false;
    }

    checkNotNull(requisition);

    partnerNodeIds = Sets.newHashSet();
    associations = Lists.newArrayList();

    if (null == supervisoryNodeId) {
      return false;
    }

    if (requisition.hasOriginalRequisitionId()
        || requisitionRepository.existsByOriginalRequisitionId(requisition.getId())) {
      return false;
    }

    partnerNodeIds = supervisoryNodeReferenceDataService
        .findOne(supervisoryNodeId)
        .getPartnerNodeIds();

    associations = supplyPartnerReferenceDataService
        .search(partnerNodeIds)
        .stream()
        .map(this::findAssociation)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());

    return !associations.isEmpty();
  }

  /**
   * Splits the given requisition into parts based on associations from supply partners. After the
   * split there will be:
   * <ul>
   * <li>X requisitions, where X is the number of partners handling the requisition</li>
   * <li>the original requisition but data for orderables that are supplied by partners will be
   * changed to zero and in the remarks column, there will be an explanation text.
   * </li>
   * </ul>
   *
   * @return a list of requisitions.
   * @throws IllegalStateException if a requisition is not splittable. To verify if the requisition
   *                               is splittable please use {@link #isSplittable()}.
   */
  RequisitionSplitResult split() {
    checkState(active, "Split feature is disabled");
    checkState(!isEmpty(associations), "Requisition is not splittable");

    List<Requisition> partnerRequisitions = Lists.newArrayListWithCapacity(associations.size());
    createPartnerRequisitions(partnerRequisitions);

    adjustOriginalRequisition(partnerRequisitions);

    return new RequisitionSplitResult(requisition, partnerRequisitions);
  }

  void setRequisition(Requisition requisition) {
    this.requisition = requisition;
    this.requisitionLineItems = requisition
        .getNonSkippedRequisitionLineItems()
        .stream()
        .collect(Collectors.toMap(RequisitionLineItem::getOrderableId, Function.identity()));
    this.programIds = Sets.newHashSet(requisition.getProgramId());
    this.facilityIds = Sets.newHashSet(requisition.getFacilityId());
    this.orderableIds = requisitionLineItems
        .values()
        .stream()
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet());
  }

  private void createPartnerRequisitions(List<Requisition> requisitions) {
    for (SupplyPartnerAssociationDto association : associations) {
      List<RequisitionLineItem> partnerLineItems = association
          .getOrderableIds()
          .stream()
          .map(requisitionLineItems::get)
          .filter(Objects::nonNull)
          .map(this::createPartnerLineItem)
          .collect(Collectors.toList());

      requisitions.add(createPartnerRequisition(association, partnerLineItems));
    }
  }

  private void adjustOriginalRequisition(List<Requisition> partnerRequisitions) {
    String remarks = messageService
        .localize(new Message(LINE_ITEM_SUPPLIED_BY_OTHER_PARTNER))
        .asMessage();

    partnerRequisitions
        .stream()
        .map(Requisition::getRequisitionLineItems)
        .flatMap(Collection::stream)
        .map(RequisitionLineItem::getOrderableId)
        .map(requisitionLineItems::get)
        .forEach(lineItem -> {
          lineItem.setRequestedQuantity(0);
          lineItem.setRequestedQuantityExplanation("0");
          lineItem.setApprovedQuantity(0);
          lineItem.setRemarks(remarks);
        });
  }

  private Requisition createPartnerRequisition(SupplyPartnerAssociationDto association,
      List<RequisitionLineItem> partnerLineItems) {
    Requisition partnerRequisition = new Requisition(this.requisition);

    partnerRequisition.setId(null);
    partnerRequisition.setRequisitionLineItems(partnerLineItems);
    partnerRequisition.setVersion(1L);
    partnerRequisition.setDraftStatusMessage("");
    partnerRequisition.setStatusChanges(Lists.newArrayList());
    partnerRequisition.setSupervisoryNodeId(association.getSupervisoryNodeId());
    partnerRequisition.setPreviousRequisitions(Lists.newArrayList());
    partnerRequisition.setAvailableProducts(Sets.newHashSet());

    partnerRequisition.setExtraData(this.requisition.getExtraData());
    partnerRequisition.setOriginalRequisitionId(requisition.getId());

    partnerLineItems.forEach(lineItem -> lineItem.setRequisition(partnerRequisition));

    return partnerRequisition;
  }

  private RequisitionLineItem createPartnerLineItem(RequisitionLineItem lineItem) {
    RequisitionLineItem partnerLineItem = new RequisitionLineItem(lineItem);
    partnerLineItem.setId(null);
    partnerLineItem.setRequisition(null);

    return partnerLineItem;
  }

  private SupplyPartnerAssociationDto findAssociation(SupplyPartnerDto partner) {
    return partner
        .getAssociations()
        .stream()
        .filter(association -> filterSet(programIds, association.getProgramId()))
        .filter(association -> filterSet(partnerNodeIds, association.getSupervisoryNodeId()))
        .filter(association -> filterSet(facilityIds, association.getFacilityIds()))
        .filter(association -> filterSet(orderableIds, association.getOrderableIds()))
        .findFirst()
        .orElse(null);
  }

  private boolean filterSet(Set<UUID> set, UUID value) {
    return isEmpty(set) || set.contains(value);
  }

  private boolean filterSet(Set<UUID> set, Set<UUID> values) {
    // disjoint returns true if collections have no elements in common but we want
    // set that have at least one element from the values set
    // that is why we negate the result of the method.
    return isEmpty(set) || !Collections.disjoint(set, values);
  }

}
