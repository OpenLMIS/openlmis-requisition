package org.openlmis.utils;

import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.AuthenticationException;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class AuthenticationHelper {
  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Method returns current user based on Spring context
   * and fetches his data from referencedata service.
   * @return UserDto entity of current user.
   */
  public UserDto getCurrentUser() {
    String username =
        (String) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    UserDto user = userReferenceDataService.findUser(username);

    if (user == null) {
      throw new AuthenticationException("User with name \"" + username + "\" not found.");
    }

    return user;
  }
}
