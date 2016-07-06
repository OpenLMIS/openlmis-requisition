package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Right;
import org.openlmis.referencedata.domain.Role;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class RoleRightsControllerIntegrationTest {

  private static final String RESOURCE_URL_ROLES = "http://localhost:8080/api/roles";
  private static final String RESOURCE_URL_RIGHTS = "http://localhost:8080/api/rights";

  private String roleRightsController = "RoleRightsControllerIntegrationTest";

  private Right right;
  private Role role;

  private Right savedRight;

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    right = new Right();
    right.setName(roleRightsController);
    right.setRightType(roleRightsController);
    role = new Role();
    role.setName(roleRightsController);
  }

  @Test
  public void testCreateRight() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(right);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Right> result = restTemplate.postForEntity(
            RESOURCE_URL_RIGHTS, entity, Right.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    savedRight = result.getBody();

    Assert.assertNotNull(savedRight.getId());
  }

  @Test
  public void testCreateRole() throws JsonProcessingException {
    ArrayList<Right> rights = new ArrayList<>();
    rights.add(savedRight);
    role.setRights(rights);
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(role);
    HttpEntity<String> entity = new HttpEntity<>(json, headers);

    ResponseEntity<Role> result = restTemplate.postForEntity(
            RESOURCE_URL_ROLES, entity, Role.class);
    Assert.assertEquals(HttpStatus.CREATED, result.getStatusCode());

    Role savedRole = result.getBody();

    Assert.assertNotNull(savedRole.getId());
  }


}
