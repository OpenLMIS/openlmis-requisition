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

package org.openlmis.requisition.i18n;

public abstract class MessageKeys {
  private static final String SERVICE_PREFIX = "requisition";
  private static final String ERROR_PREFIX = SERVICE_PREFIX + ".error";

  public static final String ERROR_CLASS_NOT_FOUND = ERROR_PREFIX + ".class-not-found";
  public static final String ERROR_IO = ERROR_PREFIX + ".io";

  public static final String ERROR_JASPER_FILE_FORMAT = ERROR_PREFIX + ".jasper.file.format";
  public static final String ERROR_JASPER_FILE_CREATION = ERROR_PREFIX + ".jasper.file-creation";
  public static final String ERROR_JASPER_TEMPLATE_NOT_FOUND = ERROR_PREFIX
      + ".jasper.templateNotFound";
  public static final String ERROR_REPORTING_CREATION = ERROR_PREFIX + ".reporting.creation";
  public static final String ERROR_REPORTING_FILE_EMPTY = ERROR_PREFIX + ".reporting.file.empty";
  public static final String ERROR_REPORTING_TEMPLATE_PARAMETER_INVALID = ERROR_PREFIX
      + ".reporting.template.parameter.invalid";
  public static final String ERROR_REPORTING_FILE_INCORRECT_TYPE = ERROR_PREFIX
      + ".reporting.file.incorrectType";
  public static final String ERROR_REPORTING_FILE_INVALID = ERROR_PREFIX
      + ".reporting.file.invalid";
  public static final String ERROR_REPORTING_FILE_MISSING = ERROR_PREFIX
      + ".reporting.file.missing";
  public static final String ERROR_REPORTING_PARAMETER_INCORRECT_TYPE = ERROR_PREFIX
      + ".reporting.parameter.incorrectType";
  public static final String ERROR_REPORTING_PARAMETER_MISSING = ERROR_PREFIX
      + ".reporting.parameter.missing";
  public static final String ERROR_REPORTING_TEMPLATE_EXIST = ERROR_PREFIX
      + ".reporting.template.exist";
  public static final String ERROR_REPORTING_TEMPLATE_NOT_FOUND = ERROR_PREFIX
      + ".reporting.template.notFound";
  public static final String ERROR_MUST_BE_SUBMITTED_TO_BE_AUTHORIZED = ERROR_PREFIX
      + ".authorize.mustBeSubmittedToBeAuthorize";
  public static final String ERROR_FINISH_PROVIOUS_REQUISITION = ERROR_PREFIX
      + ".initiate.finishPreviousRequisition";
  public static final String ERROR_MUST_BE_INITIATED_TO_BE_SUBMMITED = ERROR_PREFIX
      + ".submit.mustBeInitiatedToBeSubmitted";
  public static final String ERROR_FIELD_MUST_HAVE_VALUES = ERROR_PREFIX
      + ".submit.fieldsMustHaveValues";
  public static final String ERROR_CANNOT_UPDATE_WITH_STATUS = ERROR_PREFIX
      + ".update.canNotUpdateWithStatus";
  public static final String ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL = ERROR_PREFIX
      + ".reject.requisitionMustBeWaitingForApproval";
  public static final String ERROR_REQUISITION_MUST_BE_APPROVED = ERROR_PREFIX
      + ".release.requisitionMustBeApproved";
  public static final String ERROR_SKIP_FAILED_EMERGENCY = ERROR_PREFIX
      + ".canNotSkipPeriod.emergency";
  public static final String ERROR_PROGRAM_DOES_NOT_ALLOW_SKIP = ERROR_PREFIX
      + ".canNotSkipPeriod.program";
  public static final String ERROR_SKIP_FAILED_WRONG_STATUS = ERROR_PREFIX
      + ".canNotSkipPeriod.status";
  public static final String ERROR_DELETE_FAILED_WRONG_STATUS = ERROR_PREFIX
      + ".badStatus.delete";
  public static final String ERROR_REQUISITION_NOT_FOUND = ERROR_PREFIX
      + ".requisitionNotFound";
  public static final String ERROR_ID_MISMATCH = ERROR_PREFIX
      + ".idMismatch";
  public static final String ERROR_NO_SUBMITTED_REQUISITIONS = ERROR_PREFIX
      + ".submittedRequisitionsNotFound";
  public static final String ERROR_REQUISITION_MUST_BE_AUTHORIZED_OR_SUBMITTED = ERROR_PREFIX
      + ".approve.requisitionMustBeAuthorizedOrSubmittedIfSkipped";
  public static final String ERROR_REQUISITION_PERIODS_FOR_INITIATE_MISSING_PARAMETERS =
      ERROR_PREFIX + ".periodsForInitiate.missingParameters";
  public static final String ERROR_INITIALIZE_MISSING_PARAMETERS = ERROR_PREFIX
      + "initiate.missingParameters";
  public static final String ERROR_INCORRECT_SUGGESTED_PERIOD = ERROR_PREFIX
      + ".initiate.incorrectSuggestedPeriod";
  public static final String ERROR_NULL_ID = ERROR_PREFIX
      + ".initiate.nullId";
  public static final String ERROR_PERIOD_SHOULD_BE_OLDEST_AND_NOT_ASSOCIATED = ERROR_PREFIX
      + ".initiate.periodShouldBeOldestAndNotAssociated";
  public static final String ERROR_PERIOD_MUST_BELONG_TO_THE_SAME_SCHEDULE = ERROR_PREFIX
      + ".initiate.periodMustBelongToTheSameSchedule";
  public static final String ERROR_AUTHORIZATION_TO_BE_SKIPPED = ERROR_PREFIX
      + ".authorizationToBeSkipped";
  public static final String ERROR_CONVERTING_REQUISITION_TO_ORDER = ERROR_PREFIX
      + ".convertingRequisitionToOrder";
  public static final String ERROR_MUST_HAVE_SUPPLYING_FACILITY = ERROR_PREFIX
      + ".release.mustHaveSupplyingFacility";
  public static final String ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM = ERROR_PREFIX
      + ".facilityDoesNotSupportProgram";
  public static final String ERROR_PROGRAM_ID_CANNOT_BE_NULL = ERROR_PREFIX
      + ".programIdCannotBeNull";
  public static final String ERROR_REQUISITION_TEMPLATE_NOT_FOUND = ERROR_PREFIX
      + ".requisitionTemplateNotFound";
  public static final String ERROR_REQUISITION_TEMPLATE_NOT_FOUND_FOR_ID = ERROR_PREFIX
      + ".requisitionTemplateNotFoundForId";
  public static final String ERROR_REQUISITION_TEMPLATE_NOT_DEFINED = ERROR_PREFIX
      + ".requisitionTemplateNotDefined";
  public static final String ERROR_SOURCE_NOT_AVAILABLE_FOR_THIS_COLUMN = ERROR_PREFIX
      + ".sourceNotAvailableForThisColumn";
  public static final String ERROR_OPTION_NOT_AVAILABLE_FOR_THIS_COLUMN = ERROR_PREFIX
      + ".optionNotAvailableForThisColumn";
  public static final String ERROR_COLUMN_NOT_IN_TEMPLATE = ERROR_PREFIX
      + ".columnNotInTemplate";
  public static final String ERROR_COLUMNS_MAP_IS_NULL = ERROR_PREFIX
      + ".columnsMapIsNull";
  public static final String ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING = ERROR_PREFIX
      + ".columnIsNotValidForSorting";
  public static final String ERROR_CONFIGURATION_SETTING_NOT_FOUND = ERROR_PREFIX
      + ".configurationSettingNotFound";
  public static final String
      ERROR_REQUISITION_GROUP_PROGRAM_SCHEDULE_WITH_PROGRAM_AND_FACILITY_NOT_FOUND =
      ERROR_PREFIX + ".initiate.requisitionGroupProgramScheduleWithProgramAndFacilityNotFound";
  public static final String ERROR_FIELD_IS_CALCULATED = ERROR_PREFIX
      + ".validation.fieldIsCalculated";
  public static final String ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD =
      ERROR_PREFIX + ".validation.stockoutDaysCantBeGreaterThanLengthOfPeriod";
  public static final String ERROR_ONLY_AVAILABLE_FOR_APPROVAL = ERROR_PREFIX
      + ".validation.onlyAvailableForApproval";
  public static final String ERROR_IS_INVARIANT = ERROR_PREFIX
      + ".validation.isInvariant";
  public static final String ERROR_DATE_MODIFIED_MISMATCH =
      "requisition.error.validation.dateModifiedMismatch";
  public static final String ERROR_IS_HIDDEN = ERROR_PREFIX
      + ".validation.isHidden";
  public static final String ERROR_MUST_BE_NON_NEGATIVE = ERROR_PREFIX
      + ".validation.mustBeNonNegative";
  public static final String ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE = ERROR_PREFIX
      + ".validation.stockAdjustmentNonNegative";
  public static final String ERROR_STOCK_ADJUSTMENT_NOT_FOUND = ERROR_PREFIX
      + ".validation.stockAdjustmentNotFound";
  public static final String ERROR_VALUE_MUST_BE_ENTERED = ERROR_PREFIX
      + ".validation.valueMustBeEntered";
  public static final String ERROR_INCORRECT_VALUE = ERROR_PREFIX
      + ".validation.incorrectValue";
  public static final String ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED = ERROR_PREFIX
      + ".validation.displayedWhenRequestedQuantityDisplayed";
  public static final String ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED =
      ERROR_PREFIX + ".validation.displayedWhenRequestedQuantityExplanationDisplayed";
  public static final String ERROR_DISPLAYED_WHEN_CALC_ORDER_QUANTITY_EXPLANATION_NOT_DISPLAYED =
      ERROR_PREFIX + ".validation.displayedWhenCalcOrderQuantityExplanationIsNotDisplayed";
  public static final String ERROR_MUST_BE_DISPLAYED =
      ERROR_PREFIX + ".validation.mustBeDisplayed";
  public static final String ERROR_VALIDATION_REQUESTED_QUANTITY_EXPLANATION_REQUIRED =
      ERROR_PREFIX + ".validation.requestedQuantityExplanationRequired";
  public static final String ERROR_VALIDATION_FIELD_IS_TOO_LONG =
      ERROR_PREFIX + ".validation.fieldIsTooLong";
  public static final String ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND =
      ERROR_PREFIX + ".validation.columnDefinitionNotFound";
  public static final String ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED =
      ERROR_PREFIX + ".validation.columnDefinitionModified";
  public static final String ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME = ERROR_PREFIX
      + ".validation.cannotCalculateAtTheSameTime";
  public static final String ERROR_SOURCE_NOT_AVAILABLE = ERROR_PREFIX
      + ".validation.sourceIsNotAvailable";
  public static final String ERROR_OPTION_NOT_AVAILABLE = ERROR_PREFIX
      + ".validation.optionIsNotAvailable";
  public static final String ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED =
      ERROR_PREFIX + ".validation.mustBeDisplayedWhenOnHandCalculated";
  public static final String ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED =
      ERROR_PREFIX + ".validation.mustBeDisplayedWhenConsumedQuantityIsCalculated";
  public static final String ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED = ERROR_PREFIX
      + ".validation.mustBeDisplayedWhenConsumptionIsCalculated";
  public static final String ERROR_MUST_BE_DISPLAYED_WHEN_AVERAGE_CONSUMPTION_IS_CALCULATED =
      ERROR_PREFIX + ".validation.mustBeDisplayedWhenAverageConsumptionIsCalculated";
  public static final String ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE = ERROR_PREFIX
      + ".validation.valueDoesNotMatchCalculatedValue";
  public static final String ERROR_CANNOT_UPDATE_REQUISITION = ERROR_PREFIX
      + ".validation.cannotUpdateRequisition";
  public static final String ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE = ERROR_PREFIX
      + ".validation.fieldMustBeInTemplate";
  public static final String ERROR_VALIDATION_FIELD_CANNOT_BE_NULL = ERROR_PREFIX
      + ".validation.fieldCanNotBeNull";
  public static final String ERROR_VALIDATION_FIELD_MUST_BE_GREATER_OR_EQUAL = ERROR_PREFIX
      + ".validation.fieldMustBeGreaterOrEqual";
  public static final String ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST = ERROR_PREFIX
      + ".validation.referencedObjectDoesNotExist";
  public static final String ERROR_USER_NOT_FOUND = ERROR_PREFIX
      + ".authentication.userCanNotBeFound";
  public static final String ERROR_RIGHT_NOT_FOUND = ERROR_PREFIX
      + ".authentication.rightCanNotBeFound";
  public static final String ERROR_NO_FOLLOWING_PERMISSION = ERROR_PREFIX
      + ".authorization.noFollowingPermission";
  public static final String ERROR_REQUISITION_TEMPLATE_IN_USE = ERROR_PREFIX
      + ".requisitionTemplateInUse";
  public static final String ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL =
      ERROR_PREFIX + ".validation.sourceOfRequisitionTemplateColumnCannotBeNull";
  public static final String ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED =
      ERROR_PREFIX + ".onlyAlphanumericLabelIsAccepted";
  public static final String REQUISITION_TYPE_REGULAR = "requisition.type.regular";
  public static final String REQUISITION_TYPE_EMERGENCY = "requisition.type.emergency";

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

  public static final String STATUS_CHANGE_USER_SYSTEM =
      SERVICE_PREFIX + ".statusChange.user.system";

  private MessageKeys() {
    throw new UnsupportedOperationException();
  }
}
