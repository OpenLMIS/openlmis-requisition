package org.openlmis.requisition.web;

import javax.validation.Valid;
import org.openlmis.hierarchyandsupervision.exception.ExternalApiException;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.referencedata.web.BaseController;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@Controller
public class RefencedataController extends BaseController {

  @Autowired
  private ReferenceDataService referenceDataService;

  /**
   * Get all users.
   *
   * @return Users.
   */
  @RequestMapping(value = "/users", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllUsers() {
    UserDto[] users = referenceDataService.findAllUsers();
    return new ResponseEntity<>(users, HttpStatus.OK);
  }

  /**
   * Get specific user.
   *
   * @return User.
   */
  @RequestMapping(value = "/user/{id}", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getUser(@PathVariable("id") UUID userId) {
    UserDto user = referenceDataService.findOneUser(userId);
    return new ResponseEntity<>(user, HttpStatus.OK);
  }


  /**
   * Custom endpoint for creating and updating users.
   */
  @RequestMapping(value = "/users", method = RequestMethod.POST)
  public ResponseEntity<?> save(@RequestBody @Valid UserDto userDto) {

    try {
      referenceDataService.saveUser(userDto);
      return new ResponseEntity<>(userDto, HttpStatus.OK);
    } catch (ExternalApiException ex) {
      ErrorResponse errorResponse =
          new ErrorResponse("An error occurred while saving user", ex.getMessage());
      return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
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
    UserDto user = referenceDataService.findOneUser(userId);
    if (user == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        referenceDataService.deleteUser(userId);
      } catch (DataIntegrityViolationException ex) {
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<UserDto>(HttpStatus.NO_CONTENT);
    }
  }

}
