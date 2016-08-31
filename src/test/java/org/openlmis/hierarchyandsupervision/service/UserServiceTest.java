package org.openlmis.hierarchyandsupervision.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.whenNew;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.utils.AuthUserRequest;
import org.openlmis.hierarchyandsupervision.utils.NotificationRequest;
import org.openlmis.hierarchyandsupervision.utils.PasswordChangeRequest;
import org.openlmis.hierarchyandsupervision.utils.PasswordResetRequest;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(PowerMockRunner.class)
@PowerMockRunnerDelegate(BlockJUnit4ClassRunner.class)
@PrepareForTest({UserService.class})
public class UserServiceTest {

  private static final String AUTH_TOKEN = "authToken";

  @Mock
  private UserRepository userRepository;

  @Mock
  private ExposedMessageSource messageSource;

  @InjectMocks
  private UserService userService;

  private User user;

  @Before
  public void setUp() {
    user = generateUser();
  }

  @Test
  public void shouldFindUsersIfMatchedRequiredFields() {
    when(userRepository
            .searchUsers(
                    user.getUsername(),
                    user.getFirstName(),
                    user.getLastName(),
                    user.getHomeFacility(),
                    user.getActive(),
                    user.getVerified()))
            .thenReturn(Arrays.asList(user));

    List<User> receivedUsers = userService.searchUsers(user.getUsername(), user.getFirstName(),
        user.getLastName(), user.getHomeFacility(), user.getActive(), user.getVerified());

    assertEquals(1, receivedUsers.size());
    assertEquals(user, receivedUsers.get(0));
  }

  @Test
  public void shouldSaveRequisitionAndAuthUsers() throws Exception {
    when(userRepository.save(user)).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    userService.save(user, AUTH_TOKEN);

    verify(userRepository).save(user);

    ArgumentCaptor<AuthUserRequest> authUserCaptor = ArgumentCaptor.forClass(AuthUserRequest.class);
    verify(restTemplate).postForObject(contains(AUTH_TOKEN), authUserCaptor.capture(), any());

    assertEquals(1, authUserCaptor.getAllValues().size());
    AuthUserRequest authUser = authUserCaptor.getValue();

    assertEquals(user.getUsername(), authUser.getUsername());
    assertEquals(user.getId(), authUser.getReferenceDataUserId());
    assertEquals(user.getEmail(), authUser.getEmail());
    assertTrue(authUser.getEnabled());
    assertEquals("USER", authUser.getRole());
  }

  @Test
  public void shouldSendResetPasswordEmailWhenNewUserIsCreated() throws Exception {
    user.setId(null);
    UUID resetPasswordTokenId = UUID.randomUUID();
    String mailSubject = "subject";
    String mailBody = "body";

    when(userRepository.save(user)).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    when(restTemplate.postForObject(contains("passwordResetToken?userId=" + user.getId()),
        any(), eq(UUID.class))).thenReturn(resetPasswordTokenId);

    when(messageSource.getMessage(contains(mailSubject), any(Object[].class),
        any(Locale.class))).thenReturn(mailSubject);

    when(messageSource.getMessage(contains(mailBody), any(Object[].class),
        any(Locale.class))).thenReturn(mailBody);

    userService.save(user, AUTH_TOKEN);

    verify(userRepository).save(user);

    verify(restTemplate).postForObject(anyString(), isA(AuthUserRequest.class), eq(Object.class));

    NotificationRequest request = new NotificationRequest("notification", user.getEmail(),
        mailSubject, mailBody, null);

    verify(restTemplate).postForObject(contains("notification?access_token=" + AUTH_TOKEN),
        refEq(request), eq(Object.class));
  }

  @Test
  public void shouldNotSendResetPasswordEmailWhenUserIsUpdated() throws Exception {
    when(userRepository.save(user)).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    userService.save(user, AUTH_TOKEN);

    verify(userRepository).save(user);

    verify(restTemplate).postForObject(contains(AUTH_TOKEN),
        isA(AuthUserRequest.class), eq(Object.class));

    verify(restTemplate, never()).postForObject(contains("passwordResetToken"),
        any(), eq(UUID.class));

    verify(restTemplate, never()).postForObject(contains("notification"),
        any(), eq(Object.class));
  }

  @Test
  public void shouldResetPasswordAndVerifyUser() throws Exception {
    PasswordResetRequest passwordResetRequest = new PasswordResetRequest("username", "newPassword");

    when(userRepository.findOneByUsername(passwordResetRequest.getUsername())).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    assertFalse(user.getVerified());

    userService.passwordReset(passwordResetRequest, AUTH_TOKEN);

    verify(userRepository).save(user);

    assertTrue(user.getVerified());

    verify(restTemplate).postForObject(contains("passwordReset?access_token=" + AUTH_TOKEN),
        refEq(passwordResetRequest), eq(String.class));
  }

  @Test
  public void shouldChangePasswordAndVerifyUser() throws Exception {
    PasswordChangeRequest passwordResetRequest = new PasswordChangeRequest(UUID.randomUUID(),
        "username", "newPassword");

    when(userRepository.findOneByUsername(passwordResetRequest.getUsername())).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    assertFalse(user.getVerified());

    userService.changePassword(passwordResetRequest, AUTH_TOKEN);

    verify(userRepository).save(user);

    assertTrue(user.getVerified());

    verify(restTemplate).postForObject(contains("changePassword?access_token=" + AUTH_TOKEN),
        refEq(passwordResetRequest), eq(String.class));
  }

  private User generateUser() {
    User user = new User();
    user.setId(UUID.randomUUID());
    user.setFirstName("Ala");
    user.setLastName("ma");
    user.setUsername("kota");
    user.setEmail("test@mail.com");
    user.setTimezone("UTC");
    user.setHomeFacility(mock(Facility.class));
    user.setVerified(false);
    user.setActive(true);
    return user;
  }
}
