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

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_STATUS_UPDATE_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT;

import org.apache.commons.lang.text.StrSubstitutor;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.utils.Message;
import org.openlmis.utils.NotifierHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;
import java.text.MessageFormat;
import java.time.chrono.Chronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

@Component
public class RequisitionStatusNotifier {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionStatusNotifier.class);

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private MessageService messageService;

  @Value("${requisitionUri}")
  private String requisitionUri;

  /**
   * Notify user(s) that the requisition's status has changed.
   *
   * @param requisition a requisition that has just changed its status
   */
  public void notifyStatusChanged(Requisition requisition) {

    List<StatusChange> statusChanges = requisition.getStatusChanges();
    if (statusChanges == null) {
      LOGGER.error("Could not find status changes for requisition " + requisition.getId() + "to "
          + "notify for requisition status change.");
      return;
    }

    Optional<StatusChange> initiateAuditEntry = statusChanges.stream()
        .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.INITIATED)
        .findFirst();
    if (!initiateAuditEntry.isPresent() || initiateAuditEntry.get().getAuthorId() == null) {
      LOGGER.warn("Could not find initiator for requisition " + requisition.getId() + " to notify "
          + "for requisition status change.");
      return;
    }
    UserDto initiator = userReferenceDataService.findOne(initiateAuditEntry.get().getAuthorId());

    if (!NotifierHelper.canBeNotified(initiator)) {
      return;
    }

    Optional<StatusChange> submitAuditEntry = statusChanges.stream()
        .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.SUBMITTED)
        .findFirst();
    if (!submitAuditEntry.isPresent()) {
      LOGGER.warn("Could not find submitter for requisition " + requisition.getId() + " to notify "
          + "for requisition status change.");
      return;
    }

    Optional<StatusChange> currentAuditEntry = statusChanges.stream()
        .filter(statusChange -> statusChange.getStatus() == requisition.getStatus())
        .findFirst();
    if (!currentAuditEntry.isPresent() || currentAuditEntry.get().getAuthorId() == null) {
      LOGGER.warn("Could not find author of current status change for requisition "
          + requisition.getId() + " to notify for requisition status change.");
      return;
    }

    String requisitionUrl = System.getenv("BASE_URL") + MessageFormat.format(
        requisitionUri, requisition.getId());
    String requisitionType = messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_EMERGENCY : REQUISITION_TYPE_REGULAR)).asMessage();

    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());
    UserDto author = userReferenceDataService.findOne(currentAuditEntry.get().getAuthorId());

    Locale locale = LocaleContextHolder.getLocale();
    String datePattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(
        FormatStyle.MEDIUM, FormatStyle.MEDIUM, Chronology.ofLocale(locale), locale);

    Map<String, String> valuesMap = new HashMap<>();
    valuesMap.put("initiator", initiator.getUsername());
    valuesMap.put("requisitionType", requisitionType);
    valuesMap.put("submittedDate", submitAuditEntry.get().getCreatedDate().format(
        DateTimeFormatter.ofPattern(datePattern)));
    valuesMap.put("periodName", period.getName());
    valuesMap.put("programName", program.getName());
    valuesMap.put("facilityName", facility.getName());
    valuesMap.put("requisitionStatus", requisition.getStatus().toString());
    valuesMap.put("author", author.getUsername());
    valuesMap.put("changeDate", currentAuditEntry.get().getCreatedDate().format(
        DateTimeFormatter.ofPattern(datePattern)));
    valuesMap.put("requisitionUrl", requisitionUrl);

    String subject =
        messageService.localize(new Message(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT)).asMessage();
    String content =
        messageService.localize(new Message(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT)).asMessage();

    StrSubstitutor sub = new StrSubstitutor(valuesMap);
    content = sub.replace(content);

    notificationService.notify(initiator, subject, content);
  }
}
