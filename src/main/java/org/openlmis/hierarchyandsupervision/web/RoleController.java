package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.web.BaseController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class RoleController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RoleController.class);

  @Autowired
  private RoleRepository roleRepository;

  /**
   * Allows creating new roles.
   * If the id is specified, it will be ignored.
   *
   * @param role A role bound to the request body
   * @return ResponseEntity containing the created role
   */
  @RequestMapping(value = "/roles", method = RequestMethod.POST)
  public ResponseEntity<?> createRole(@RequestBody Role role) {
    try {
      LOGGER.debug("Creating new role");
      role.setId(null);
      Role newRole = roleRepository.save(role);
      LOGGER.debug("Created new role with id: " + role.getId());
      return new ResponseEntity<Role>(newRole, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating role", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
    return new ResponseEntity<>(roles, HttpStatus.OK);
  }

  /**
   * Allows updating roles.
   *
   * @param role A role bound to the request body
   * @param roleId UUID of role which we want to update
   * @return ResponseEntity containing the updated role
   */
  @RequestMapping(value = "/roles/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRole(@RequestBody Role role,
                                            @PathVariable("id") UUID roleId) {

    Role roleToUpdate = roleRepository.findOne(roleId);
    try {
      if (roleToUpdate == null) {
        roleToUpdate = new Role();
        LOGGER.info("Creating new role");
      } else {
        LOGGER.debug("Updating role with id: " + roleId);
      }

      roleToUpdate.updateFrom(role);
      roleToUpdate = roleRepository.save(roleToUpdate);

      LOGGER.debug("Saved role with id: " + roleToUpdate.getId());
      return new ResponseEntity<Role>(roleToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while saving role with id: "
                  + roleToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen role.
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
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting role with id: "
                    + roleId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Role>(HttpStatus.NO_CONTENT);
    }
  }
}
