package org.openlmis.requisition.service;

import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT;

import java.text.MessageFormat;
import java.util.UUID;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RequisitionStatusNotifier {

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Notify requisition's creator that it was converted to order.
   *
   * @param requisition requisition that was converted
   * @return true if success, false if failed.
   */
  public Boolean notifyConvertToOrder(Requisition requisition) {
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());
    // TODO: OLMIS-1182 get creator ID from Javers
    UUID creatorId = UUID.fromString("86495466-6966-4bf8-ae90-1fd1d0c6ce22");
    UserDto creator = userReferenceDataService.findOne(creatorId);

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT);

    Object[] msgArgs = {creator.getFirstName(), creator.getLastName(),
        program.getName(), period.getName()};
    content = MessageFormat.format(content, msgArgs);

    return notificationService.notify(creator, subject, content);
  }
}
