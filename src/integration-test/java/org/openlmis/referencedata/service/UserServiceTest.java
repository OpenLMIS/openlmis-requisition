package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.UserService;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class UserServiceTest {

  @Autowired
  private UserService userService;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  private List<User> users;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    users = new ArrayList<>();
    users.add(generateUser());
  }

  @After
  public void cleanup() {
    Iterable<User> users = userService.searchUsers("kota1", null, null, null, null, null);
    if (users != null && users.iterator().hasNext()) {
      userRepository.delete(users);
    }
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
  }

  @Test
  public void testSearchUsers() {
    List<User> receivedUsers = userService.searchUsers(
            users.get(0).getUsername(),
            users.get(0).getFirstName(),
            users.get(0).getLastName(),
            users.get(0).getHomeFacility(),
            users.get(0).getActive(),
            users.get(0).getVerified());
    Assert.assertEquals(1,receivedUsers.size());
    for ( User user : receivedUsers ) {
      Assert.assertEquals(
              user.getUsername(),
              users.get(0).getUsername());
      Assert.assertEquals(
              user.getFirstName(),
              users.get(0).getFirstName());
      Assert.assertEquals(
              user.getLastName(),
              users.get(0).getLastName());
      Assert.assertEquals(
              user.getHomeFacility().getId(),
              users.get(0).getHomeFacility().getId());
      Assert.assertEquals(
              user.getHomeFacility().getActive(),
              users.get(0).getActive());
      Assert.assertEquals(
              user.getVerified(),
              users.get(0).getVerified());
    }
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = generateInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setPassword("iDobrze" + instanceNumber);
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    userRepository.save(user);
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
    facility.setStockInventory(null);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + generateInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + generateInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + generateInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
