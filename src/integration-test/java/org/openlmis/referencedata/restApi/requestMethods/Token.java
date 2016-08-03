package org.openlmis.referencedata.restApi.requestMethods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;

import java.io.IOException;

import static io.restassured.RestAssured.given;

/**
 * Created by user on 8/2/16.
 */
public class Token {
    public Token(){};
    RequestSpecBuilder builder = new RequestSpecBuilder();
    ObjectMapper mapper = new ObjectMapper();

    public String returnCreatedToken(String serverURL) throws IOException {
        String APIUrl = serverURL + "8081/oauth/token?grant_type=password&username=admin&password=password";
        builder.setContentType("application/json");
        RequestSpecification requestSpec = builder.build();
        Response response = given().authentication().preemptive().basic("trusted-client", "secret").spec(requestSpec).when().post(APIUrl);
        String responseSting = response.getBody().asString();
        JsonNode obj = mapper.readTree(responseSting);
        return obj.get("access_token").textValue();
    }
}
