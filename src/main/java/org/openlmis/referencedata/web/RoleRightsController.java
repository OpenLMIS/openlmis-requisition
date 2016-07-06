package org.openlmis.referencedata.web;

import static org.springframework.web.bind.annotation.RequestMethod.POST;

import org.openlmis.referencedata.domain.Right;
import org.openlmis.referencedata.domain.Role;
import org.openlmis.referencedata.repository.RightRepository;
import org.openlmis.referencedata.repository.RoleRightsRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

@RepositoryRestController
public class RoleRightsController {
  private Logger logger = LoggerFactory.getLogger(RoleRightsController.class);

  @Autowired
  private RoleRightsRepository roleRightsRepository;
  @Autowired
  private RightRepository rightRepository;

  /**
   * Allows creating new Roles with associated Rights.
   * @param role a role bound to the request body
   * @return ResponseEntity containing the created role
   */

  @RequestMapping(value = "/roles", method = POST)
  public ResponseEntity<?> createRole(
          @RequestBody Role role) {
    if (role == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Creating new role");
      role.setId(null);

      Role it = roleRightsRepository.findByName(role.getName());
      if (it != null) {
        roleRightsRepository.delete(it);
      }

      Role newRole = roleRightsRepository.save(role);
      return new ResponseEntity<Role>(
              newRole, HttpStatus.CREATED);
    }
  }

  /**
   * Allows creating new Rights.
   * @param right a right bound to the request body
   * @return ResponseEntity containing the created right
   */

  @RequestMapping(value = "/rights", method = POST)
  public ResponseEntity<?> createRight(
          @RequestBody Right right) {
    if (right == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Creating new right");
      right.setId(null);

      Right it = rightRepository.findByName(right.getName());
      if (it != null) {
        rightRepository.delete(it);
      }

      Right newRight = rightRepository.save(right);
      return new ResponseEntity<Right>(
              newRight, HttpStatus.CREATED);
    }
  }
}
