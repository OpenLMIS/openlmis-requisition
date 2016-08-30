package org.openlmis.hierarchyandsupervision.service;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.exception.ExternalApiException;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.utils.AuthUserRequest;
import org.openlmis.hierarchyandsupervision.utils.NotificationRequest;
import org.openlmis.hierarchyandsupervision.utils.PasswordChangeRequest;
import org.openlmis.hierarchyandsupervision.utils.PasswordResetRequest;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;

@Service
public class UserService {

  private static final String BASE_URL = System.getenv("BASE_URL");

  //TODO: This address needs to be changed when reset password page will be done
  private static final String RESET_PASSWORD_PATH = BASE_URL + "/reset-password.html";

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private ExposedMessageSource messageSource;

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
    boolean isNewUser = false;

    if (user.getId() == null) {
      isNewUser = true;
    }

    User savedUser = userRepository.save(user);
    saveAuthUser(savedUser, token);

    if (isNewUser) {
      sendResetPasswordEmail(savedUser, token);
    }
  }

  /**
   * Resets a user's password.
   */
  public void passwordReset(PasswordResetRequest passwordResetRequest, String token) {
    try {
      String url = "http://auth:8080/api/users/passwordReset?access_token=" + token;
      RestTemplate restTemplate = new RestTemplate();

      restTemplate.postForObject(url, passwordResetRequest, String.class);

      verifyUser(passwordResetRequest.getUsername());
    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not reset auth user password", ex);
    }
  }

  /**
   * Changes user's password if valid reset token is provided.
   */
  public void changePassword(PasswordChangeRequest passwordChangeRequest, String token) {
    try {
      String url = "http://auth:8080/api/users/changePassword?access_token=" + token;

      RestTemplate restTemplate = new RestTemplate();
      restTemplate.postForObject(url, passwordChangeRequest, String.class);

      verifyUser(passwordChangeRequest.getUsername());
    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not reset auth user password", ex);
    }
  }

  private void verifyUser(String username) {
    User user = userRepository.findOneByUsername(username);
    user.setVerified(true);
    userRepository.save(user);
  }

  private void saveAuthUser(User user, String token) {
    try {
      AuthUserRequest userRequest = new AuthUserRequest();
      userRequest.setUsername(user.getUsername());
      userRequest.setEmail(user.getEmail());
      userRequest.setReferenceDataUserId(user.getId());

      String url = "http://auth:8080/api/users?access_token=" + token;
      RestTemplate restTemplate = new RestTemplate();

      restTemplate.postForObject(url, userRequest, Object.class);
    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not save auth user", ex);
    }
  }

  private void sendResetPasswordEmail(User user, String authToken) {
    UUID token = createPasswordResetToken(user.getId(), authToken);

    String[] msgArgs = {user.getFirstName(), user.getLastName(),
        user.getUsername(), RESET_PASSWORD_PATH + "/username/" + user.getUsername()
        + "/token/" + token};
    String mailBody = messageSource.getMessage("password.reset.email.body",
        msgArgs, LocaleContextHolder.getLocale());
    String mailSubject = messageSource.getMessage("account.created.email.subject",
        new String[]{}, LocaleContextHolder.getLocale());

    sendMail("notification", user.getEmail(), mailSubject, mailBody, authToken);
  }

  private UUID createPasswordResetToken(UUID userId, String token) {
    try {
      String url = "http://auth:8080/api/users/passwordResetToken?userId=" + userId
          + "&access_token=" + token;
      RestTemplate restTemplate = new RestTemplate();

      return restTemplate.postForObject(url, null, UUID.class);
    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not create reset password token", ex);
    }
  }

  private void sendMail(String from, String to, String subject, String content, String token) {
    try {
      NotificationRequest request = new NotificationRequest(from, to, subject, content, null);

      String url = "http://notification:8080/notification?access_token=" + token;
      RestTemplate restTemplate = new RestTemplate();

      restTemplate.postForObject(url, request, Object.class);
    } catch (RestClientException ex) {
      throw new ExternalApiException("Could not send reset password email", ex);
    }
  }
}
