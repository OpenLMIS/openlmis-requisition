package org.openlmis.utils;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_RIGHT_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_USER_NOT_FOUND;

import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.AuthenticationMessageException;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class AuthenticationHelper {

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  /**
   * Method returns current user based on Spring context
   * and fetches his data from reference-data service.
   *
   * @return UserDto entity of current user.
   * @throws AuthenticationMessageException if user cannot be found.
   */
  public UserDto getCurrentUser() {
    String username =
        (String) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    UserDto user = userReferenceDataService.findUser(username);

    if (user == null) {
      throw new AuthenticationMessageException(new Message(ERROR_USER_NOT_FOUND, username));
    }

    return user;
  }

  /**
   * Method returns a correct right and fetches his data from reference-data service.
   *
   * @param name right name
   * @return RightDto entity of right.
   * @throws AuthenticationMessageException if right cannot be found.
   */
  public RightDto getRight(String name) {
    RightDto right = rightReferenceDataService.findRight(name);

    if (null == right) {
      throw new AuthenticationMessageException(new Message(ERROR_RIGHT_NOT_FOUND, name));
    }

    return right;
  }
}
