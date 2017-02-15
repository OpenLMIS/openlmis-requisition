package org.openlmis.requisition.service;

import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_URI;

import org.apache.commons.lang.text.StrSubstitutor;
import org.javers.core.commit.CommitMetadata;
import org.javers.core.diff.Change;
import org.openlmis.requisition.domain.AuditLogEntry;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
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
import org.openlmis.utils.NotifierHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;
import java.time.chrono.Chronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

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
    Map<String, AuditLogEntry> statusChanges = requisition.getStatusChanges();
    if (statusChanges == null) {
      LOGGER.error("Could not find status changes for requisition " + requisition.getId() + "to " 
          + "notify for requisition status change.");
      return;
    }

    AuditLogEntry initiateAuditEntry = statusChanges.get(RequisitionStatus.INITIATED.toString());
    if (initiateAuditEntry == null || initiateAuditEntry.getAuthorId() == null) {
      LOGGER.warn("Could not find initiator for requisition " + requisition.getId() + " to notify " 
          + "for requisition status change.");
      return;
    }
    UserDto initiator = userReferenceDataService.findOne(initiateAuditEntry.getAuthorId());

    if (!NotifierHelper.canBeNotified(initiator)) {
      return;
    }

    AuditLogEntry submitAuditEntry = statusChanges.get(RequisitionStatus.SUBMITTED.toString());
    if (submitAuditEntry == null) {
      LOGGER.warn("Could not find submitter for requisition " + requisition.getId() + " to notify "
          + "for requisition status change.");
      return;
    }

    CommitMetadata commitMetadata = change.getCommitMetadata().get();
    UUID commitAuthorUuid;
    try {
      commitAuthorUuid = UUID.fromString(commitMetadata.getAuthor());
    } catch (Exception ex) {
      LOGGER.warn("Could not find valid commit author UUID.");
      return;
    }

    String requisitionUrl = System.getenv("BASE_URL") + MessageFormat.format(
        configurationSettingService.getStringValue(REQUISITION_URI), requisition.getId());
    String requisitionType = messageService.localize(new Message(requisition.getEmergency()
        ? REQUISITION_TYPE_EMERGENCY : REQUISITION_TYPE_REGULAR)).asMessage();

    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriodId());
    FacilityDto facility = facilityReferenceDataService.findOne(requisition.getFacilityId());
    UserDto author = userReferenceDataService.findOne(commitAuthorUuid);

    Locale locale = LocaleContextHolder.getLocale();
    String datePattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(
        FormatStyle.MEDIUM, FormatStyle.MEDIUM, Chronology.ofLocale(locale), locale);

    Map<String, String> valuesMap = new HashMap<>();
    valuesMap.put("initiator", initiator.getUsername());
    valuesMap.put("requisitionType", requisitionType);
    valuesMap.put("submittedDate", submitAuditEntry.getChangeDate().format(
        DateTimeFormatter.ofPattern(datePattern)));
    valuesMap.put("periodName", period.getName());
    valuesMap.put("programName", program.getName());
    valuesMap.put("facilityName", facility.getName());
    valuesMap.put("requisitionStatus", requisition.getStatus().toString());
    valuesMap.put("author", author.getUsername());
    valuesMap.put("changeDate", commitMetadata.getCommitDate().toString(datePattern));
    valuesMap.put("requisitionUrl", requisitionUrl);

    String subject = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT);
    String content = configurationSettingService
        .getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT);

    StrSubstitutor sub = new StrSubstitutor(valuesMap);
    content = sub.replace(content);

    notificationService.notify(initiator, subject, content);
  }
}
