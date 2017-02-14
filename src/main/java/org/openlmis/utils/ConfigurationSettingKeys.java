package org.openlmis.utils;

public abstract class ConfigurationSettingKeys {

  public static final String REQUISITION_EMAIL_NOREPLY
      = "requisition.email.noreply";
  public static final String REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT
      = "requisition.email.convertToOrder.subject";
  public static final String REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT
      = "requisition.email.convertToOrder.content";
  public static final String REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT
      = "requisition.email.statusUpdate.subject";
  public static final String REQUISITION_EMAIL_STATUS_UPDATE_CONTENT
      = "requisition.email.statusUpdate.content";
  public static final String REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT
      = "requisition.email.actionRequired.subject";
  public static final String REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT
      = "requisition.email.actionRequired.content";
  public static final String REQUISITION_URI
      = "requisition.requisitionUri";

  private ConfigurationSettingKeys() {
    throw new UnsupportedOperationException();
  }

}
