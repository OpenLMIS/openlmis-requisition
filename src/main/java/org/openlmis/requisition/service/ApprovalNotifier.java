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

import static org.apache.commons.beanutils.PropertyUtils.getPropertyDescriptors;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_HEADER;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_SMS;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_SUBJECT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_TITLE;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_USER;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_SMS_ACTION_REQUIRED_CONTENT;

import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang.text.StrSubstitutor;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.RequisitionUnSkippedDetails;
import org.openlmis.requisition.domain.requisition.RequisitionUnSkippedLineItem;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.notification.NotificationService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisingUsersReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ApprovalNotifier extends BaseNotifier {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApprovalNotifier.class);
  static final String NOTIFICATION_TAG = "requisition-actionRequired";

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
  private UserReferenceDataService userReferenceDataService;

  @Value("${requisitionUri}")
  private String requisitionUri;
  
  @Value("${publicUrl}")
  private String publicUrl;

  /**
   * Notify requisition's creator that it was converted to order.
   *
   * @param requisition requisition that was converted
   */
  public void notifyApprovers(Requisition requisition, Locale locale) {
    Collection<UserDto> approvers = getApprovers(requisition);
    String reqType = getMessage(getEmergencyKey(requisition), locale);
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

    String subject = getMessage(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT, locale);
    String emailContent = getMessage(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT, locale);
    String smsContent = getMessage(REQUISITION_SMS_ACTION_REQUIRED_CONTENT, locale);

    DateTimeFormatter dateTimeFormatter = getDateTimeFormatter();

    String url = publicUrl + MessageFormat.format(requisitionUri, requisition.getId());

    Map<String, String> valuesMap =
        getValuesMap(reqType, period, program, facility, submittedDate, dateTimeFormatter, url);

    StrSubstitutor sub = new StrSubstitutor(valuesMap);

    for (UserDto approver : approvers) {
      valuesMap.put("approver", approver.getUsername());
      notificationService.notify(approver, subject,
          sub.replace(emailContent), sub.replace(smsContent), NOTIFICATION_TAG);
    }
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
    RightDto right = rightReferenceDataService.findRight(PermissionService.REQUISITION_APPROVE);
    return supervisingUsersReferenceDataService
        .findAll(requisition.getSupervisoryNodeId(), right.getId(), requisition.getProgramId());
  }

  /**
   * add approver details and sent email to facility in charge.
   * @param requisition to propagate approvers
   * @param locale system locale
   */
  public void notifyApproversUnskippedRequisitionLineItems(Requisition requisition,
                                                           UserDto approver, Locale locale,
                                                           UserDto initiator) {
    if (requisition.getExtraData().containsKey("unSkippedRequisitionLineItems")) {
      StringBuilder emailContent = new StringBuilder();

      String url = publicUrl + MessageFormat.format(requisitionUri, requisition.getId());
      ProcessingPeriodDto period =
              periodReferenceDataService.findOne(requisition.getProcessingPeriodId());
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
      FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());
      Map<String,String> valuesMap = getUnskippedRequisitionValuesMap(period,program,facility,url);
      StrSubstitutor sub = new StrSubstitutor(valuesMap);
      String title = getMessage(REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_TITLE, locale);
      emailContent.append(sub.replace(title));

      RequisitionUnSkippedDetails requisitionDetails =
              (RequisitionUnSkippedDetails) requisition.getExtraData()
                      .get("unSkippedRequisitionLineItems");
      String header = getMessage(REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_HEADER,locale);
      emailContent.append(header);
      addUnskippedRequisitionLineItems(emailContent,
              requisitionDetails.getUnSkippedLineItemList(),locale);
      addApproverDetails(requisitionDetails,approver);
      requisition.getExtraData().put("unSkippedRequisitionLineItems", requisitionDetails);
      Map<String,String> userMessageParams = buildMessageParamsForUser(approver);
      String userContent = getContent(approver,REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_USER,
              userMessageParams, locale);
      emailContent.append(userContent).append(System.lineSeparator());

      String subject = getMessage(REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_SUBJECT, locale);
      String emailBody = emailContent.toString();

      if (initiator == null) {
        LOGGER.warn("Initiator is null for requisition %s",requisition.getId());
        return;
      }
      notificationService.notify(initiator, subject,
              emailBody, REQUISITION_EMAIL_UNSKIPPED_LINE_ITEMS_SMS, "");
    }
  }

  private void addUnskippedRequisitionLineItems(StringBuilder emailContent,
                                                List<RequisitionUnSkippedLineItem> lineItems,
                                                Locale locale) {
    int count = 1;
    for (RequisitionUnSkippedLineItem lineItem : lineItems) {
      Map<String,String> lineMessageParams = buildMessageParamsForRequisitionLine(lineItem);
      String lineContent = getContent(lineItem,
              REQUISITION_EMAIL_UNSKIPPED_LINE, lineMessageParams, locale);
      lineContent = lineContent.replace("{0}",String.valueOf(count));
      emailContent.append(lineContent).append(System.lineSeparator());
      count++;
    }
  }

  /**
   * returns requisition initiator.
   * @param statusChanges of the requisition
   * @param requisitionId of the requisition
   * @return userdto
   */
  public UserDto getInitiator(List<StatusChange> statusChanges, UUID requisitionId) {
    UUID initiatorId = getInitiatorId(statusChanges);
    if (initiatorId == null) {
      LOGGER.warn("Could not find initiator for requisition %s to notify "
              + "for requisition status change.", requisitionId);
      return null;
    }
    return userReferenceDataService.findOne(initiatorId);
  }

  /**
   * returns the uuid of the initiator.
   * @param statusChanges of the requisition
   * @return uuid
   */
  public UUID getInitiatorId(List<StatusChange> statusChanges) {
    for (StatusChange statusChange : statusChanges) {
      if (statusChange.getStatus() == RequisitionStatus.INITIATED) {
        return statusChange.getAuthorId();
      }
    }
    return null;
  }

  private void addApproverDetails(RequisitionUnSkippedDetails
                                                 requisitionDetails, UserDto user) {
    requisitionDetails.setFirstname(user.getFirstName());
    requisitionDetails.setLastname(user.getLastName());
    requisitionDetails.setUsername(user.getUsername());
  }

  private Map<String,String> buildMessageParamsForUser(UserDto user) {
    Map<String, String> userMap = new HashMap<>();
    userMap.put("firstName",user.getFirstName());
    userMap.put("lastName",user.getLastName());
    userMap.put("userName",user.getUsername());
    return userMap;
  }

  private Map<String,String> buildMessageParamsForRequisitionLine(
          RequisitionUnSkippedLineItem lineItem) {
    Map<String,String> lineMessageParams = new HashMap<>();
    lineMessageParams.put("productCode",lineItem.getProductCode());
    lineMessageParams.put("productName",lineItem.getProductName());
    lineMessageParams.put("approvedQuantity",
            String.valueOf(lineItem.getApprovedQuantity()));
    lineMessageParams.put("remarks",lineItem.getRemarks());
    return lineMessageParams;

  }

  private Map<String, String> getUnskippedRequisitionValuesMap(ProcessingPeriodDto period,
                                           ProgramDto program, FacilityDto facility,String url) {
    Map<String, String> valuesMap = new HashMap<>();
    valuesMap.put("periodName", period.getName());
    valuesMap.put("programName", program.getName());
    valuesMap.put("facilityName", facility.getName());
    valuesMap.put("requisitionUrl", url);
    return valuesMap;
  }

  private String getContent(Object object, String messageKey,
                            Map<String, String> messageParams, Locale locale) {
    String content = getMessage(messageKey,locale);

    try {
      List<PropertyDescriptor> descriptors = Arrays
              .stream(getPropertyDescriptors(object.getClass()))
              .filter(d -> null != d.getReadMethod())
              .collect(Collectors.toList());

      for (PropertyDescriptor descriptor : descriptors) {
        String target = "{" + descriptor.getName() + "}";
        String replacement = String.valueOf(descriptor.getReadMethod().invoke(object));

        content = content.replace(target, replacement);
      }

      for (Map.Entry<String, String> entry : messageParams.entrySet()) {
        String target = "{" + entry.getKey() + "}";
        String replacement = entry.getValue();

        content = content.replace(target, replacement);
      }

    } catch (IllegalAccessException | InvocationTargetException exp) {
      throw new IllegalStateException("Can't get access to getter method", exp);
    }
    return content;
  }
}
