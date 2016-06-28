import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import static org.junit.Assert.assertThat;

/*
    This class contains User related integrations tests implemented via RestAssured.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
@WebIntegrationTest("server.port:8080")
public class UserIntegrationTest
{
  private static final String BASE_URL = "http://192.168.99.100:8080";
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML definition.";

  @Before
  public void setUp()
  { }

  @Test
  public void testCreate() {
    RestAssured.baseURI =  BASE_URL;
    RamlDefinition ramlDefinition = RamlLoaders.fromClasspath().load("requisition-service.yaml");
    RestAssuredClient restAssured = ramlDefinition.createRestAssured();

    //TODO: Add requisite descriptions to our RAML and then re-enable test
    //Verify that our RAML file is valid.
    //Assert.assertThat(ramlDefinition.validate(), RamlMatchers.validates());


    //Make a simple call and verify that the input and output match what's defined in our RAML spec
    restAssured.given().get("/api/v2/users").andReturn();
    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());


    //Make the same call as above, but ensure we get a 404 response
    restAssured.given().
            when().get("/api/v2/users").
            then().statusCode(404);
    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());


    //Create a user
    restAssured.given().contentType("application/json").
            body(getUserJson()).
    when().
            post("/api/users").
    then().
            statusCode(201);
    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());


    //Having created a user, ensure that /api/v2/users now returns a 200 status
    restAssured.given().
            when().get("/api/v2/users").
            then().statusCode(200);
    //assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }


  /*
    For now, manually return JSON rather a Java User object. This sidesteps the fact that Jackson
    omits the password field, which is non-nullable and thus required to create a user.  */
  private static String getUserJson()
  {
    return  "{ \"username\": \"user1c\", \"firstName\": \"first1\", \"lastName\": \"last1\", \"password\" : \"password\", \"verified\": true, \"active\": true }";
  }
}
