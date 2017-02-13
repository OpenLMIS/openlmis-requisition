package org.openlmis.requisition.service;

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;

import org.openlmis.requisition.domain.Requisition;
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
import org.openlmis.requisition.service.referencedata.SupervisedUsersReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.openlmis.utils.RightName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;
import java.util.Collection;

@Component
public class ApprovalNotifier {

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private SupervisedUsersReferenceDataService supervisedUsersReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private MessageService messageService;

  private static final String REQUISITION_URL =
      System.getenv("BASE_URL") + "/#!/requisition/";

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

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT);

    for (UserDto approver : approvers) {
      Object[] msgArgs = {approver.getUsername(), reqType,
          requisition.getSubmittedDate().toString(), period.getName(), program.getName(),
          facility.getName(), REQUISITION_URL + requisition.getId()};
      content = MessageFormat.format(content, msgArgs);

      notificationService.notify(approver, subject, content);
    }
  }

  private Collection<UserDto> getApprovers(Requisition requisition) {
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    return supervisedUsersReferenceDataService
        .findAll(requisition.getSupervisoryNodeId(), right.getId(), requisition.getProgramId());
  }
}
