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

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;

import org.apache.commons.lang.text.StrSubstitutor;
import org.openlmis.requisition.domain.BaseTimestampedEntity;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.RightName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class ApprovedRequisitionNotifier extends BaseNotifier {

  private static final String CONVERT_TO_ORDER_URL = System.getenv("BASE_URL")
      + "/#!/requisitions/convertToOrder";

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private RequisitionForConvertBuilder requisitionForConvertBuilder;

  /**
   * Notifies all the clerks that the requisition has been approved and is ready to be converted to
   * order.
   *
   * @param requisition  the requisition to notify the clerks for
   */
  public void notifyClerks(Requisition requisition) {
    String subject = getMessage(REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT);
    String content = getMessage(REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT);

    Map<String, String> messageParams = new HashMap<>();
    messageParams.put("requisitionType", getRequisitionType(requisition));
    messageParams.put("finalApprovalDate", getFinalApprovalDate(requisition));
    messageParams.put("facility", getFacilityName(requisition));
    messageParams.put("url", CONVERT_TO_ORDER_URL);
    messageParams.put("program", getProgram(requisition));
    messageParams.put("period", getPeriod(requisition));

    for (UserDto user : getClerks(requisition)) {
      messageParams.put("user", user.getUsername());
      notificationService.notify(user, subject, new StrSubstitutor(messageParams).replace(content));
    }
  }

  private String getRequisitionType(Requisition requisition) {
    return messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_EMERGENCY : REQUISITION_TYPE_REGULAR)).asMessage();
  }

  private String getFinalApprovalDate(Requisition requisition) {
    ZonedDateTime approvedDate = requisition.getStatusChanges().stream()
        .filter(statusChange -> statusChange.getStatus() == RequisitionStatus.APPROVED)
        .map(BaseTimestampedEntity::getCreatedDate)
        .findFirst()
        .orElse(ZonedDateTime.now());

    return approvedDate.format(getDateTimeFormatter());
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

    requisitionForConvertBuilder.getAvailableSupplyingDepots(requisition.getId())
        .forEach(warehouse -> users.addAll(userReferenceDataService.findUsers(
            rightId,
            null,
            null,
            warehouse.getId())));

    return users.stream().filter(BaseNotifier::canBeNotified).collect(Collectors.toSet());
  }
}
