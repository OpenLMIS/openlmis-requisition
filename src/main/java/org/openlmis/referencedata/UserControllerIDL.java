package org.openlmis.referencedata;

import org.openlmis.referencedata.domain.User;
import org.openlmis.referencedata.repository.UserRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;


/**
 * A version of UserController which has endpoints that are congruous with those in our IDL (RAML) spec.
 */
@RestController
public class UserControllerIDL
{
  Logger logger = LoggerFactory.getLogger(UserControllerIDL.class);

  @Autowired
  private UserRepository userRepository;

  @RequestMapping(value = "/api/v2/users", method = RequestMethod.GET)
  public ResponseEntity<?> getUsers()
  {
    Iterable<User> users = userRepository.findAll();
    if(users == null || !users.iterator().hasNext())
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    else
      return new ResponseEntity(users, HttpStatus.OK);
  }

}
