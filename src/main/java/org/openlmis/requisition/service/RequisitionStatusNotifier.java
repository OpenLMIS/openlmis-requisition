package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;

import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT;

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

  /**
   * Notify user that the requisition was converted to order.
   *
   * @param user receiver of the notification
   * @param requisition requisition that was converted
   * @return true if success, false if failed.
   */
  public Boolean notifyConvertToOrder(UserDto user, Requisition requisition)
      throws ConfigurationSettingException {
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT);

    Object[] msgArgs = {user.getFirstName(), user.getLastName(),
        program.getName(), period.getName()};
    content = MessageFormat.format(content, msgArgs);

    return notificationService.notify(user, subject, content);
  }
}
