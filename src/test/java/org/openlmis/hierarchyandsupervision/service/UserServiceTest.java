package org.openlmis.hierarchyandsupervision.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
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
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

@RunWith(PowerMockRunner.class)
@PowerMockRunnerDelegate(BlockJUnit4ClassRunner.class)
@PrepareForTest({UserService.class})
public class UserServiceTest {

  @Mock
  private UserRepository userRepository;

  @InjectMocks
  private UserService userService;

  private int currentInstanceNumber;
  private List<User> users;

  @Before
  public void setUp() {
    users = new ArrayList<>();
    currentInstanceNumber = 0;
    userService = new UserService();
    generateInstances();
    initMocks(this);
  }

  @Test
  public void shouldFindUsersIfMatchedRequiredFields() {
    when(userRepository
            .searchUsers(
                    users.get(0).getUsername(),
                    users.get(0).getFirstName(),
                    users.get(0).getLastName(),
                    users.get(0).getHomeFacility(),
                    users.get(0).getActive(),
                    users.get(0).getActive()))
            .thenReturn(Arrays.asList(users.get(0)));

    List<User> receivedUsers = userService.searchUsers(
            users.get(0).getUsername(),
            users.get(0).getFirstName(),
            users.get(0).getLastName(),
            users.get(0).getHomeFacility(),
            users.get(0).getActive(),
            users.get(0).getVerified());

    assertEquals(1,receivedUsers.size());
    assertEquals(
            receivedUsers.get(0).getUsername(),
            users.get(0).getUsername());
    assertEquals(
            receivedUsers.get(0).getFirstName(),
            users.get(0).getFirstName());
    assertEquals(
            receivedUsers.get(0).getLastName(),
            users.get(0).getLastName());
    assertEquals(
            receivedUsers.get(0).getHomeFacility().getId(),
            users.get(0).getHomeFacility().getId());
    assertEquals(
            receivedUsers.get(0).getHomeFacility().getActive(),
            users.get(0).getActive());
    assertEquals(
            receivedUsers.get(0).getVerified(),
            users.get(0).getVerified());
  }

  @Test
  public void shouldSaveRequisitionAndAuthUsers() throws Exception {
    User user = users.get(0);
    String token = "authToken";

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

  private void generateInstances() {
    for (int instancesCount = 0; instancesCount < 5; instancesCount++) {
      users.add(generateUser());
    }
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = generateInstanceNumber();
    user.setId(UUID.randomUUID());
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setEmail(instanceNumber + "@mail.com");
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    return user;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + generateInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + generateInstanceNumber());
    geographicLevel.setLevelNumber(1);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + generateInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + generateInstanceNumber());
    return facilityType;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
