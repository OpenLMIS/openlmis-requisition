package org.openlmis.requisition.web;


public class MissingPermissionException extends AuthorizationException {

  public MissingPermissionException(String permissionName) {
    super("You do not have the following permission to perform this action: " + permissionName);
  }

}
