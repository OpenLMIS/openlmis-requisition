package org.openlmis.requisition.web;

import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.referencedata.web.BaseController;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class RefencedataController extends BaseController{

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
}
