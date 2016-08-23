package org.openlmis.hierarchyandsupervision.service;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.utils.AuthUserRequest;
import org.openlmis.referencedata.domain.Facility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.util.List;

@Service
public class UserService {

  @Autowired
  private UserRepository userRepository;

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
          Facility homeFacility, Boolean active, Boolean verified) {
    return userRepository.searchUsers(
            username, firstName,
            lastName, homeFacility,
            active, verified);
  }

  /**
   * Creating or updating users.
   */
  @Transactional
  public void save(User user, String token) {
    User savedUser = userRepository.save(user);
    saveAuthUser(savedUser, token);
  }

  private void saveAuthUser(User user, String token) {
    AuthUserRequest userRequest = new AuthUserRequest();
    userRequest.setUsername(user.getUsername());
    userRequest.setEmail(user.getEmail());
    userRequest.setReferenceDataUserId(user.getId());

    String url = "http://auth:8080/api/users?access_token=" + token;
    RestTemplate restTemplate = new RestTemplate();

    restTemplate.postForObject(url, userRequest, Object.class);
  }
}
