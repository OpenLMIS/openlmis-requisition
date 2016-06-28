package org.openlmis.referencedata.resteasy;

import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.jaxrs.CheckingWebTarget;
import guru.nidi.ramltester.junit.RamlMatchers;
import org.jboss.resteasy.client.jaxrs.ResteasyClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClientBuilder;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.User;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import javax.ws.rs.core.Response;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

/*
    This class contains User related integrations tests implemented via RestEasy.
 */
@Ignore
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class UserIntegrationTest {

  private static final String BASE_URL = "http://192.168.99.100:8080";

  private static final RamlDefinition ramlDefinition = RamlLoaders.fromClasspath()
          .load("requisition-service.yaml")
          .assumingBaseUri(BASE_URL);


  private Response response;
  private ResteasyClient client = new ResteasyClientBuilder().build();
  private CheckingWebTarget webTarget;


  @Before
  public void setUp()
  {
    webTarget = ramlDefinition.createWebTarget(client.target(BASE_URL));
  }

  @Test
  public void testCreate()
  {
    response = webTarget.path("/api/v2/users").request().get();
    assertEquals("Expected a 404 response code", response.getStatus(), HttpStatus.NOT_FOUND.value() );

    AssertRamlMatches();
  }

  private void AssertRamlMatches()
  {
    assertThat("HTTP request/response should match RAML definition." , webTarget.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private static User getUser()
  {
    User user = new User();
    user.setFirstName("firstName");
    user.setLastName("lastName");
    user.setUsername("userName");
    user.setPassword("password");
    user.setActive(true);
    user.setVerified(true);
    return user;
  }
}
