package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

@Component
public class RequisitionStatusNotifier {

  @Autowired
  private MessageSource messageSource;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private NotificationService notificationService;

  /**
   * Notify user that the requisition was converted to order.
   *
   * @param user receiver of the notification
   * @param requisition requisition that was converted
   */
  public void notifyConvertToOrder(UserDto user, Requisition requisition) {
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());

    String[] msgArgs = {user.getFirstName(), user.getLastName(),
        program.getName(), period.getName()};
    String mailBody = messageSource.getMessage("requisition.mail.convert-to-order.content",
        msgArgs, LocaleContextHolder.getLocale());
    String mailSubject = messageSource.getMessage("requisition.mail.convert-to-order.subject",
        new String[]{}, LocaleContextHolder.getLocale());

    notificationService.notify(user, mailSubject, mailBody);
  }
}
