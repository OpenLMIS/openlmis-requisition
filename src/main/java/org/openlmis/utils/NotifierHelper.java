package org.openlmis.utils;

import org.openlmis.requisition.dto.UserDto;

public class NotifierHelper {

  private NotifierHelper() {
    throw new UnsupportedOperationException();
  }

  /**
   * Check if user want notifications: (enabled, verified, allowNotify all true).
   */
  public static boolean checkNotify(UserDto user) {
    return user.getAllowNotify() != null && user.getAllowNotify()
        && user.isActive() && user.isVerified();
  }

}
