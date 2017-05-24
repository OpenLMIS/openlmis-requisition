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
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;

import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.time.chrono.Chronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.lang.text.StrSubstitutor;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisingUsersReferenceDataService;
import org.openlmis.utils.Message;
import org.openlmis.utils.NotifierHelper;
import org.openlmis.utils.RightName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

@Component
public class ApprovalNotifier {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApprovalNotifier.class);

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private SupervisingUsersReferenceDataService supervisingUsersReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private MessageService messageService;

  @Value("${requisitionUri}")
  private String requisitionUri;

  /**
   * Notify requisition's creator that it was converted to order.
   *
   * @param requisition requisition that was converted
   */
  public void notifyApprovers(Requisition requisition) {
    Collection<UserDto> approvers = getApprovers(requisition);
    String reqType = messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_EMERGENCY : REQUISITION_TYPE_REGULAR)).asMessage();
    ProcessingPeriodDto period =
        periodReferenceDataService.findOne(requisition.getProcessingPeriodId());
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());

    List<StatusChange> statusChanges = requisition.getStatusChanges();
    if (statusChanges == null) {
      LOGGER.warn("Could not find requisition audit data to notify for convert to order.");
      return;
    }

    Optional<StatusChange> submitAuditEntry = statusChanges.stream()
        .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.SUBMITTED)
        .findFirst();
    if (!submitAuditEntry.isPresent()) {
      LOGGER.warn("Could not find requisition submitter to notify for requisition status change.");
      return;
    }
    ZonedDateTime submittedDate = submitAuditEntry.get().getCreatedDate();

    String subject =
        messageService.localize(new Message(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT)).asMessage();
    String content =
        messageService.localize(new Message(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT)).asMessage();

    DateTimeFormatter dateTimeFormatter = getDateTimeFormatter();

    String url = System.getenv("BASE_URL")
        + MessageFormat.format(requisitionUri, requisition.getId());

    Map<String, String> valuesMap =
        getValuesMap(reqType, period, program, facility, submittedDate, dateTimeFormatter, url);

    StrSubstitutor sub = new StrSubstitutor(valuesMap);

    for (UserDto approver : approvers) {
      if (NotifierHelper.canBeNotified(approver)) {
        valuesMap.put("approver", approver.getUsername());
        notificationService.notify(approver, subject, sub.replace(content));
      }
    }
  }

  private DateTimeFormatter getDateTimeFormatter() {
    Locale locale = LocaleContextHolder.getLocale();

    String datePattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(
        FormatStyle.MEDIUM, FormatStyle.MEDIUM, Chronology.ofLocale(locale), locale);
    return DateTimeFormatter.ofPattern(datePattern);
  }

  private Map<String, String> getValuesMap(String reqType, ProcessingPeriodDto period,
                                           ProgramDto program, FacilityDto facility,
                                           ZonedDateTime submittedDate,
                                           DateTimeFormatter dateTimeFormatter, String url) {
    Map<String, String> valuesMap = new HashMap<>();
    valuesMap.put("requisitionType", reqType);
    valuesMap.put("submittedDate", submittedDate.format(dateTimeFormatter));
    valuesMap.put("periodName", period.getName());
    valuesMap.put("programName", program.getName());
    valuesMap.put("facilityName", facility.getName());
    valuesMap.put("requisitionUrl", url);
    return valuesMap;
  }

  private Collection<UserDto> getApprovers(Requisition requisition) {
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    return supervisingUsersReferenceDataService
        .findAll(requisition.getSupervisoryNodeId(), right.getId(), requisition.getProgramId());
  }
}
