package org.openlmis.requisition.service;

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_URI;

import org.openlmis.requisition.domain.AuditLogEntry;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
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
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.openlmis.utils.NotifierHelper;
import org.openlmis.utils.RightName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Map;

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
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private SupervisingUsersReferenceDataService supervisingUsersReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private MessageService messageService;

  /**
   * Notify requisition's creator that it was converted to order.
   *
   * @param requisition requisition that was converted
   */
  public void notifyApprovers(Requisition requisition) {
    Collection<UserDto> approvers = getApprovers(requisition);
    String reqType = messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_REGULAR : REQUISITION_TYPE_EMERGENCY)).asMessage();
    ProcessingPeriodDto period =
        periodReferenceDataService.findOne(requisition.getProcessingPeriodId());
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());

    Map<String, AuditLogEntry> statusChanges = requisition.getStatusChanges();
    if (statusChanges == null) {
      LOGGER.warn("Could not find requisition audit data to notify for convert to order.");
      return;
    }

    AuditLogEntry submitAuditEntry = statusChanges.get(RequisitionStatus.SUBMITTED.toString());
    if (submitAuditEntry == null) {
      LOGGER.warn("Could not find requisition submitter to notify for requisition status change.");
      return;
    }
    ZonedDateTime submittedDate = submitAuditEntry.getChangeDate();

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT);

    for (UserDto approver : approvers) {
      if (NotifierHelper.canBeNotified(approver)) {
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd H:mm");
        String url = System.getenv("BASE_URL") + MessageFormat.format(
            configurationSettingService.getStringValue(REQUISITION_URI), requisition.getId());
        Object[] msgArgs = {approver.getUsername(), reqType,
            submittedDate.format(dateTimeFormatter), period.getName(), program.getName(),
            facility.getName(), url};
        content = MessageFormat.format(content, msgArgs);

        notificationService.notify(approver, subject, content);
      }
    }
  }

  private Collection<UserDto> getApprovers(Requisition requisition) {
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    return supervisingUsersReferenceDataService
        .findAll(requisition.getSupervisoryNodeId(), right.getId(), requisition.getProgramId());
  }
}
