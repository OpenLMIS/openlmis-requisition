package org.openlmis.security;

import org.apache.commons.lang3.StringUtils;
import org.javers.spring.auditable.AuthorProvider;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.utils.AuthenticationHelper;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * This class is used by JaVers to retrieve the name of the user currently logged in.
 * JaVers then associates audited changes being made with this particular user.
 */
public class UserNameProvider implements AuthorProvider {

  @Autowired
  AuthenticationHelper authenticationHelper;

  @Override
  public String provide() {
    try {
      UserDto currentUser = authenticationHelper.getCurrentUser();
      if (currentUser != null && StringUtils.isNotBlank(currentUser.getUsername())) {
        return currentUser.getUsername();
      } else {
        return "unauthenticated user";
      }
    } catch (Exception ex) {
      return "unknown user";
    }
  }

}
