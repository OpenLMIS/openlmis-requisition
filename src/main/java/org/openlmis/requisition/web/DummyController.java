package org.openlmis.requisition.web;

import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.ReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;

/**
 * DummyController just for showcase.
 */
@Controller
public class DummyController extends BaseController {

  @Autowired
  ReferenceDataService referenceDataService;

  @RequestMapping(value = "/users", method = RequestMethod.GET)
  public ResponseEntity<?> getAllUsers() {
    List<UserDto> users = referenceDataService.findAllUsers();
    return new ResponseEntity<>(users, HttpStatus.OK);
  }
}