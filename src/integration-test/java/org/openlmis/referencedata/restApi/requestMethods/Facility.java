package org.openlmis.referencedata.restApi.requestMethods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.JSONPObject;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.apache.commons.lang3.RandomStringUtils;

import java.io.IOException;

import static io.restassured.RestAssured.given;

/**
 * Created by user on 8/2/16.
 */
public class Facility {

    RequestSpecBuilder builder = new RequestSpecBuilder();
    ObjectMapper mapper = new ObjectMapper();

    public JsonNode createFacility(String serverURL, Integer portNumber ,String token, String facilityTypesHref, String geographicZonesHref) throws IOException {
        String APIUrl = serverURL + portNumber + "/api/facilities" + token;
        String APIBody = "{\"code\":\"" + RandomStringUtils.randomAlphabetic(5) + "\"," +
                "\"geographicZone\":\"" + geographicZonesHref + "\"," +
                "\"type\":\"" + facilityTypesHref + "\"," +
                "\"active\":" + true + "," +
                "\"enabled\": " + false + "}";
        builder.setContentType("application/json");
        builder.setBody(APIBody);
        RequestSpecification requestSpec = builder.build();
        Response response = given().spec(requestSpec).post(APIUrl);
        String responseSting = response.asString();
        return mapper.readTree(responseSting);
    }
}
