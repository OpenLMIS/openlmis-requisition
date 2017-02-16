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

package org.openlmis.errorhandling;

import org.openlmis.requisition.exception.AuthenticationMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.exception.VersionMismatchException;
import org.openlmis.requisition.service.DataRetrievalException;
import org.openlmis.requisition.web.PermissionMessageException;
import org.openlmis.util.ErrorResponse;
import org.openlmis.utils.Message;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Global error handling for all controllers in the service.
 * Contains common error handling mappings.
 */
@ControllerAdvice
public class GlobalErrorHandling extends AbstractErrorHandling {
  
  @ExceptionHandler(DataRetrievalException.class)
  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ResponseBody
  public ErrorResponse handleRefDataException(DataRetrievalException ex) {
    return logErrorAndRespond("Error fetching from reference data", ex);
  }

  /**
   * Handles Message exceptions and returns status 400 Bad Request.
   *
   * @param ex the ValidationMessageException to handle
   * @return the error response for the user
   */
  @ExceptionHandler(ValidationMessageException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ResponseBody
  public Message.LocalizedMessage handleMessageException(ValidationMessageException ex) {
    return getLocalizedMessage(ex);
  }

  @ExceptionHandler(AuthenticationMessageException.class)
  @ResponseStatus(HttpStatus.UNAUTHORIZED)
  @ResponseBody
  public Message.LocalizedMessage handleAuthenticationException(AuthenticationMessageException ex) {
    return getLocalizedMessage(ex);
  }

  @ExceptionHandler(PermissionMessageException.class)
  @ResponseStatus(HttpStatus.FORBIDDEN)
  @ResponseBody
  public Message.LocalizedMessage handlePermissionException(PermissionMessageException ex) {
    return getLocalizedMessage(ex);
  }

  @ExceptionHandler(VersionMismatchException.class)
  @ResponseStatus(HttpStatus.CONFLICT)
  @ResponseBody
  public Message.LocalizedMessage handlePermissionException(VersionMismatchException ex) {
    return getLocalizedMessage(ex);
  }
}
