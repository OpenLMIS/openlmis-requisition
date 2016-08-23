package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.service.UserService;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.Facility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.security.oauth2.provider.authentication.OAuth2AuthenticationDetails;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.RestClientException;

import java.util.List;

@RepositoryRestController
public class UserController {

  private static final Logger LOGGER = LoggerFactory.getLogger(UserController.class);

  @Autowired
  private UserService userService;

  /**
   * Returns all users with matched parameters
   * @param username username of user we want to search.
   * @param firstName firstName of user we want to search.
   * @param lastName lastName of user we want to search.
   * @param homeFacility homeFacility of user we want to search.
   * @param active is the user account active.
   * @param verified is the user account verified.
   * @return ResponseEntity with list of all Users matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/users/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchUsers(
          @RequestParam(value = "username", required = false) String username,
          @RequestParam(value = "firstName", required = false) String firstName,
          @RequestParam(value = "lastName", required = false) String lastName,
          @RequestParam(value = "homeFacility", required = false) Facility homeFacility,
          @RequestParam(value = "active", required = false) Boolean active,
          @RequestParam(value = "verified", required = false) Boolean verified) {
    List<User> result = userService.searchUsers(username, firstName,
            lastName, homeFacility, active, verified);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Custom endpoint for creating and updating users.
   */
  @RequestMapping(value = "/users", method = RequestMethod.POST)
  public ResponseEntity<?> save(@RequestBody User user, OAuth2Authentication auth) {
    OAuth2AuthenticationDetails details = (OAuth2AuthenticationDetails) auth.getDetails();
    String token = details.getTokenValue();

    try {
      userService.save(user, token);

      return new ResponseEntity<>(user, HttpStatus.OK);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
          new ErrorResponse("An error occurred while saving user", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }
  }
}