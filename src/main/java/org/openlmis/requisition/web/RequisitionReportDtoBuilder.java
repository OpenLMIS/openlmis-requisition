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

import static java.util.Comparator.comparing;
import static java.util.Objects.requireNonNull;
import static org.openlmis.requisition.CurrencyConfig.currencyCode;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.VersionEntityReference;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RequisitionReportDtoBuilder {

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private MessageService messageService;
  
  /**
   * Create a {@link RequisitionReportDto} based on a given {@link Requisition}.
   *
   * @param requisition a single {@link Requisition} to be converted to report dto.
   * @return a single {@link RequisitionReportDto}
   */
  public RequisitionReportDto build(Requisition requisition) {
    Set<VersionEntityReference> orderableIdentities = requisition
        .getRequisitionLineItems()
        .stream()
        .map(RequisitionLineItem::getOrderable)
        .collect(Collectors.toSet());

    Map<VersionIdentityDto, OrderableDto> orderables = orderableReferenceDataService
        .findByIdentities(orderableIdentities)
        .stream()
        .collect(Collectors.toMap(OrderableDto::getIdentity, Function.identity()));

    List<RequisitionLineItem> fullSupply =
        requisition.getNonSkippedFullSupplyRequisitionLineItems(orderables);
    List<RequisitionLineItem> nonFullSupply =
        requisition.getNonSkippedNonFullSupplyRequisitionLineItems(orderables);

    RequisitionReportDto reportDto = new RequisitionReportDto();
    reportDto.setRequisition(requisitionDtoBuilder.build(requisition));
    reportDto.setFullSupply(exportLinesToDtos(fullSupply, requisition.getProgramId()));
    reportDto.setNonFullSupply(exportLinesToDtos(nonFullSupply, requisition.getProgramId()));
    reportDto.setFullSupplyTotalCost(
        calculateTotalCostForLines(requisition, fullSupply, orderables));
    reportDto.setNonFullSupplyTotalCost(
        calculateTotalCostForLines(requisition, nonFullSupply, orderables));
    reportDto.setTotalCost(getTotalCost(requisition, orderables));

    List<StatusChange> statusChanges = requisition.getStatusChanges();
    if (statusChanges != null) {
      Optional<StatusChange> initiatedEntry = statusChanges.stream()
          .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.INITIATED)
          .findFirst();
      if (initiatedEntry.isPresent()) {
        reportDto.setInitiatedBy(getUser(initiatedEntry.get()));
        reportDto.setInitiatedDate(initiatedEntry.get().getCreatedDate());
      }

      Optional<StatusChange> submittedEntry = statusChanges.stream()
          .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.SUBMITTED)
          .findFirst();
      if (submittedEntry.isPresent()) {
        reportDto.setSubmittedBy(getUser(submittedEntry.get()));
        reportDto.setSubmittedDate(submittedEntry.get().getCreatedDate());
      }

      Optional<StatusChange> authorizedEntry = statusChanges.stream()
          .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.AUTHORIZED)
          .findFirst();
      if (authorizedEntry.isPresent()) {
        reportDto.setAuthorizedBy(getUser(authorizedEntry.get()));
        reportDto.setAuthorizedDate(authorizedEntry.get().getCreatedDate());
      }
    }

    return reportDto;
  }

  List<RequisitionLineItemDto> exportLinesToDtos(List<RequisitionLineItem> lineItems,
      UUID programId) {
    List<RequisitionLineItemDto> list = new ArrayList<>();
    for (RequisitionLineItemDto requisitionLineItemDto : requisitionExportHelper
        .exportToDtos(lineItems)) {
      list.add(requisitionLineItemDto);
    }
    list.sort(byDisplayOrder(programId));
    return list;
  }

  private Comparator<RequisitionLineItemDto> byDisplayOrder(UUID programId) {
    return comparing(r -> requireNonNull(r.getOrderable()
            .getProgramOrderable(programId))
            .getOrderableCategoryDisplayOrder());
  }

  private UserDto getUser(StatusChange statusChange) {
    UserDto user;

    if (statusChange.getAuthorId() == null) {
      /*
       * This will happen for Javers entries that were generated by the system
       * in the AuditLogInitializer class at startup. This concerns primarily
       * pre-made requisitions, either from demo-data or inserted to the database
       * through other means. This is a solution to handle those entries and display
       * the user as 'SYSTEM' in the report. If one wishes more detailed information for
       * their data, they will have to consider correctly populating Javers tables in
       * their ETL process.
      */
      Message.LocalizedMessage localizedMessage = messageService.localize(
          new Message(MessageKeys.STATUS_CHANGE_USER_SYSTEM));
      String system = localizedMessage.asMessage();

      user = new UserDto();
      user.setUsername(system);
      user.setFirstName(system);
    } else {
      user = userReferenceDataService.findOne(statusChange.getAuthorId());
    }

    return user;
  }

  private Money getTotalCost(Requisition requisition,
      Map<VersionIdentityDto, OrderableDto> orderables) {
    return calculateTotalCostForLines(requisition, requisition.getRequisitionLineItems(),
        orderables);
  }

  private Money calculateTotalCostForLines(Requisition requisition,
      List<RequisitionLineItem> requisitionLineItems,
      Map<VersionIdentityDto, OrderableDto> orderables) {
    Money defaultValue = Money.of(CurrencyUnit.of(currencyCode), 0);

    if (requisitionLineItems.isEmpty()) {
      return defaultValue;
    }

    Optional<Money> money = requisitionLineItems
        .stream()
        .map(line -> calculateTotalCost(requisition, line,
            orderables.get(new VersionIdentityDto(line.getOrderable()))))
        .reduce(Money::plus);

    return money.orElse(defaultValue);
  }

  private Money calculateTotalCost(Requisition requisition, RequisitionLineItem lineItem,
      OrderableDto orderable) {
    ProgramOrderableDto programOrderable = orderable
        .getProgramOrderable(requisition.getProgramId());

    Money pricePerPack = programOrderable.getPricePerPack();

    if (pricePerPack == null) {
      pricePerPack = Money.of(CurrencyUnit.of(currencyCode), BigDecimal.ZERO);
    }

    long packsToShip = orderable.packsToOrder(lineItem.getOrderQuantity());

    return pricePerPack.multipliedBy(packsToShip);
  }
}
