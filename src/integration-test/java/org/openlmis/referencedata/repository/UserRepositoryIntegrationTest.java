package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class UserRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<User> {

  @Autowired
  private UserRepository repository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private RoleRepository roleRepository;

  private List<User> users;
  private List<Role> roles;

  UserRepository getRepository() {
    return this.repository;
  }

  User generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    User user = new User();
    user.setUsername("user" + instanceNumber);
    user.setPassword("test" + instanceNumber);
    user.setFirstName("Test");
    user.setLastName("User");
    user.setHomeFacility(generateFacility());
    user.setActive(true);
    user.setVerified(true);
    return user;
  }

  @Before
  public void setUp() {
    users = new ArrayList<>();
    roles = new ArrayList<>();
    for (int usersCount = 0; usersCount < 5; usersCount++) {
      users.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testMultipleRoleAssignment() {
    User user = this.generateInstance();
    Role role = new Role();
    role.setName("Test1");
    user = repository.save(user);
    List<Role> roles = new ArrayList<>();
    roles.add(role);
    roleRepository.save(role);
    Role role2 = new Role();
    role2.setName("Test2");
    roles.add(role2);
    roleRepository.save(role2);
    Assert.assertNotEquals(roles, user.getRoles());
    user.setRoles(roles);
    user = repository.save(user);
    Assert.assertEquals(roles, user.getRoles());
  }

  @Test
  public void testMultipleUsersRoleAssignment() {
    for (int rolesCount = 0; rolesCount < 5; rolesCount++) {
      Role role = new Role();
      role.setName("Test" + rolesCount);
      roleRepository.save(role);
      roles.add(role);
    }
    User user1 = this.generateInstance();
    user1.setRoles(roles);
    user1.setFirstName("name1");
    user1 = repository.save(user1);
    User user2 = this.generateInstance();
    Assert.assertNotEquals(user1.getRoles(), user2.getRoles());
    List<Role> user1Roles = new ArrayList<>();
    user1Roles.addAll(user1.getRoles());
    user2.setRoles(user1Roles);
    user2.setFirstName("name2");
    user2 = repository.save(user2);
    for (int rolesCount = 0; rolesCount < user1.getRoles().size(); rolesCount++) {
      Assert.assertEquals(
              user1.getRoles().get(rolesCount).getId(),
              user2.getRoles().get(rolesCount).getId());
    }
  }

  @Test
  public void testSearchUsers() {
    List<User> receivedUsers = repository.searchUsers(
            users.get(0).getUsername(),
            users.get(0).getFirstName(),
            users.get(0).getLastName(),
            users.get(0).getHomeFacility(),
            users.get(0).getActive(),
            users.get(0).getVerified());

    Assert.assertEquals(1, receivedUsers.size());
    for (User user : receivedUsers) {
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

  private Facility generateFacility() {
    Integer instanceNumber = this.getNextInstanceNumber();
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
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }
}
