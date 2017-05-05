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

package org.openlmis.requisition.service;

import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang.text.StrSubstitutor;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.NotifierHelper;
import org.openlmis.utils.RightName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import java.time.ZonedDateTime;
import java.time.chrono.Chronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT;

@Component
public class ApprovedRequisitionNotifier {

  private static final String CONVERT_TO_ORDER_URL = System.getenv("BASE_URL")
      + "/#!/requisitions/convertToOrder";

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private MessageService messageService;

  /**
   * Notifies all the clerks that the requisition has been approved and is ready to be converted to
   * order.
   *
   * @param requisition  the requisition to notify the clerks for
   */
  public void notifyClerks(Requisition requisition) {
    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT);

    Map<String, String> messageParams = new HashedMap();
    messageParams.put("requisitionType", getRequisitionType(requisition));
    messageParams.put("finalApprovalDate", getFinalApprovalDate(requisition));
    messageParams.put("facility", getFacilityName(requisition));
    messageParams.put("url", CONVERT_TO_ORDER_URL);
    messageParams.put("program", getProgram(requisition));
    messageParams.put("period", getPeriod(requisition));

    for (UserDto user : getClerks(requisition)) {
      messageParams.put("username", user.getUsername());
      notificationService.notify(user, subject, new StrSubstitutor(messageParams).replace(content));
    }
  }

  private String getRequisitionType(Requisition requisition) {
    return messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_EMERGENCY : REQUISITION_TYPE_REGULAR)).asMessage();
  }

  private String getFinalApprovalDate(Requisition requisition) {
    Optional<StatusChange> approvedAuditEntry = requisition.getStatusChanges().stream()
        .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.APPROVED)
        .findFirst();
    ZonedDateTime approvedDate = approvedAuditEntry.get().getCreatedDate();

    Locale locale = LocaleContextHolder.getLocale();
    String datePattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(
        FormatStyle.MEDIUM, FormatStyle.MEDIUM, Chronology.ofLocale(locale), locale);
    DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(datePattern);

    return approvedDate.format(dateTimeFormatter);
  }

  private String getFacilityName(Requisition requisition) {
    return facilityReferenceDataService.findOne(requisition.getFacilityId()).getName();
  }

  private String getProgram(Requisition requisition) {
    return programReferenceDataService.findOne(requisition.getProgramId()).getName();
  }

  private String getPeriod(Requisition requisition) {
    return periodReferenceDataService.findOne(requisition.getProcessingPeriodId()).getName();
  }

  private Set<UserDto> getClerks(Requisition requisition) {
    UUID rightId = authenticationHelper.getRight(RightName.ORDERS_EDIT).getId();
    Set<UserDto> users = new HashSet<>();

    requisitionService.getAvailableSupplyingDepots(requisition.getId()).forEach(warehouse ->
        users.addAll(userReferenceDataService.findUsers(
            rightId,
            null,
            null,
            warehouse.getId()
        ))
    );

    return users.stream().filter(NotifierHelper::canBeNotified).collect(Collectors.toSet());
  }
}
