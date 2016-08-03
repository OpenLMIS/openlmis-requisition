package org.openlmis.referencedata.restApi.requestMethods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.apache.commons.lang3.RandomStringUtils;

import java.io.IOException;
import java.util.Random;

import static io.restassured.RestAssured.given;

/**
 * Created by user on 8/2/16.
 */
public class Schedule {
    RequestSpecBuilder builder = new RequestSpecBuilder();
    ObjectMapper mapper = new ObjectMapper();

    public JsonNode createSchedule(String serverURL, Integer portNumber, String token) throws IOException {
        String APIUrl = serverURL + portNumber + "/api/schedules" + token;
        String APIBody = "{\"code\":\"" + RandomStringUtils.randomAlphabetic(5) + "\"," +
                "\"name\":\"" + RandomStringUtils.randomAlphabetic(5) + "\"}";
        builder.setContentType("application/json");
        builder.setBody(APIBody);
        RequestSpecification requestSpec = builder.build();
        Response response = given().spec(requestSpec).post(APIUrl);
        String responseSting = response.asString();
        return mapper.readTree(responseSting);


    }
}
