package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
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
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@RepositoryRestController
public class RoleController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RoleController.class);

  @Autowired
  private RoleRepository roleRepository;

  /**
   * Allows creating new roles.
   *
   * @param role A role bound to the request body
   * @return ResponseEntity containing the created role
   */
  @RequestMapping(value = "/roles", method = RequestMethod.POST)
  public ResponseEntity<?> createRole(@RequestBody Role role) {
    if (role == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new role");
      // Ignore provided id
      role.setId(null);
      Role newRole = roleRepository.save(role);
      return new ResponseEntity<Role>(newRole, HttpStatus.CREATED);
    }
  }

  /**
   * Get all roles.
   *
   * @return Roles.
   */
  @RequestMapping(value = "/roles", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRoles() {
    Iterable<Role> roles = roleRepository.findAll();
    if (roles == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(roles, HttpStatus.OK);
    }
  }

  /**
   * Get choosen role.
   *
   * @param roleId UUID of role whose we want to get
   * @return Role.
   */
  @RequestMapping(value = "/roles/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRole(@PathVariable("id") UUID roleId) {
    Role role = roleRepository.findOne(roleId);
    if (role == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(role, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting role.
   *
   * @param roleId UUID of role whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/roles/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRole(@PathVariable("id") UUID roleId) {
    Role role = roleRepository.findOne(roleId);
    if (role == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        roleRepository.delete(role);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("Role cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Role>(HttpStatus.NO_CONTENT);
    }
  }
}
