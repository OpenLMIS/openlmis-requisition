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

package org.openlmis.utils;

public abstract class ConfigurationSettingKeys {

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
  public static final String REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT
      = "requisition.email.requisitionApproved.subject";
  public static final String REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT
      = "requisition.email.requisitionApproved.content";

  private ConfigurationSettingKeys() {
    throw new UnsupportedOperationException();
  }

}
