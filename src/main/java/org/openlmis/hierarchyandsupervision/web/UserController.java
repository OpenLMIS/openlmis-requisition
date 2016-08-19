package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.UserService;
import org.openlmis.referencedata.domain.Facility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@RepositoryRestController
public class UserController {

  private static final Logger LOGGER = LoggerFactory.getLogger(UserController.class);

  @Autowired
  private UserService userService;

  @Autowired
  private UserRepository userRepository;

  /**
   * Allows creating new users.
   *
   * @param user A user bound to the request body
   * @return ResponseEntity containing the created user
   */
  @RequestMapping(value = "/users", method = RequestMethod.POST)
  public ResponseEntity<?> createUser(@RequestBody User user) {
    if (user == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new user");
      // Ignore provided id
      user.setId(null);
      User newUser = userRepository.save(user);
      return new ResponseEntity<User>(newUser, HttpStatus.CREATED);
    }
  }

  /**
   * Get all users.
   *
   * @return Users.
   */
  @RequestMapping(value = "/users", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllUsers() {
    Iterable<User> users = userRepository.findAll();
    if (users == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(users, HttpStatus.OK);
    }
  }

  /**
   * Get choosen user.
   *
   * @param userId UUID of user whose we want to get
   * @return User.
   */
  @RequestMapping(value = "/users/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getUser(@PathVariable("id") UUID userId) {
    User user = userRepository.findOne(userId);
    if (user == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(user, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting user.
   *
   * @param userId UUID of user whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/users/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteUser(@PathVariable("id") UUID userId) {
    User user = userRepository.findOne(userId);
    if (user == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        userRepository.delete(user);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("User cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<User>(HttpStatus.NO_CONTENT);
    }
  }

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
}