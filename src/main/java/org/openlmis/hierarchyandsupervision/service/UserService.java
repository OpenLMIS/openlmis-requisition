package org.openlmis.hierarchyandsupervision.service;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class UserService {

  @Autowired
  UserRepository userRepository;

  /**
   * Method returns all users with matched parameters.
   * @param username username of user.
   * @param firstName firstName of user.
   * @param lastName lastName of user.
   * @param homeFacility homeFacility of user.
   * @param active is the account activated.
   * @param verified is the account verified.
   * @return List of users
   */
  public List<User> searchUsers(
          String username, String firstName, String lastName,
          Facility homeFacility, Boolean active, Boolean verified ) {
    return userRepository.searchUsers(
            username, firstName,
            lastName, homeFacility,
            active, verified);
  }
}
