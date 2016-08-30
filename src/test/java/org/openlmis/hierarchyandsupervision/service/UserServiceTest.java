package org.openlmis.hierarchyandsupervision.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.whenNew;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.utils.AuthUserRequest;
import org.openlmis.referencedata.domain.Facility;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.List;

@RunWith(PowerMockRunner.class)
@PowerMockRunnerDelegate(BlockJUnit4ClassRunner.class)
@PrepareForTest({UserService.class})
public class UserServiceTest {

  @Mock
  private UserRepository userRepository;

  @InjectMocks
  private UserService userService;

  @Test
  public void shouldFindUsersIfMatchedRequiredFields() {
    User user = mock(User.class);
    Facility facility = mock(Facility.class);
    final String firstName = "Ala";
    final String lastName = "ma";
    final String userName = "kota";
    final Boolean verified = true;
    final Boolean active = true;

    when(userRepository
            .searchUsers(userName, firstName, lastName, facility, active, verified))
            .thenReturn(Arrays.asList(user));

    List<User> receivedUsers = userService.searchUsers(
        userName, firstName, lastName, facility, active, verified);

    assertEquals(1, receivedUsers.size());
    assertEquals(user, receivedUsers.get(0));
  }

  @Test
  public void shouldSaveRequisitionAndAuthUsers() throws Exception {
    User user = mock(User.class);
    final String token = "authToken";

    when(userRepository.save(user)).thenReturn(user);

    RestTemplate restTemplate = mock(RestTemplate.class);
    whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);

    userService.save(user, token);

    verify(userRepository).save(user);

    ArgumentCaptor<AuthUserRequest> authUserCaptor = ArgumentCaptor.forClass(AuthUserRequest.class);
    verify(restTemplate).postForObject(contains(token), authUserCaptor.capture(), any());

    assertEquals(1, authUserCaptor.getAllValues().size());
    AuthUserRequest authUser = authUserCaptor.getValue();

    assertEquals(user.getUsername(), authUser.getUsername());
    assertEquals(user.getId(), authUser.getReferenceDataUserId());
    assertEquals(user.getEmail(), authUser.getEmail());
    assertTrue(authUser.getEnabled());
    assertEquals("USER", authUser.getRole());
  }
}
