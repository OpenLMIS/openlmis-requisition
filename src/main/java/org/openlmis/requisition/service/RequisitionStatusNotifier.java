package org.openlmis.requisition.service;

import org.javers.core.commit.CommitMetadata;
import org.javers.core.diff.Change;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT;

@Component
public class RequisitionStatusNotifier {

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private MessageService messageService;

  /**
   * Notify user(s) that the requisition's status has changed.
   *
   * @param requisition a requisition that has just changed its status
   * @param change Javers change containing requisition's status, the author, the time, etc.
   */
  public void notifyStatusChanged(Requisition requisition, Change change) {
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());
    UserDto creator = userReferenceDataService.findOne(requisition.getCreatorId());

    CommitMetadata commitMetadata = change.getCommitMetadata().get();

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT);

    String requisitionUrl = System.getenv("BASE_URL") + "/#!/requisition/" + requisition.getId();
    String requisitionType = messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_REGULAR : REQUISITION_TYPE_EMERGENCY)).toString();

    Object[] msgArgs = {creator.getUsername(), requisitionType, requisition.getSubmittedDate(),
        period.getName(), program.getName(), facility.getName(), requisition.getStatus().toString(),
        commitMetadata.getAuthor(), commitMetadata.getCommitDate(), requisitionUrl};

    content = MessageFormat.format(content, msgArgs);

    notificationService.notify(creator, subject, content);
  }
}
