package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.service.UserService;
import org.openlmis.referencedata.domain.Facility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RepositoryRestController
public class UserController {

  @Autowired
  UserService userService;

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