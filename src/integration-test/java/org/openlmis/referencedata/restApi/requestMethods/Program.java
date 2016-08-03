package org.openlmis.referencedata.restApi.requestMethods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.apache.commons.lang3.RandomStringUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static io.restassured.RestAssured.given;

/**
 * Created by user on 8/2/16.
 */
public class Program {

    RequestSpecBuilder builder = new RequestSpecBuilder();
    ObjectMapper mapper = new ObjectMapper();


    public JsonNode createPrograms(String serverURL, Integer portNumber, String token) throws IOException {
        String APIUrl = serverURL + portNumber +"/api/programs" + token;
        String APIBody = "{\"code\":\"" + RandomStringUtils.randomAlphabetic(5) + "\"}";
        builder.setContentType("application/json");
        builder.setBody(APIBody);
        RequestSpecification requestSpec = builder.build();
        Response response = given().spec(requestSpec).post(APIUrl);
        String responseSting = response.asString();
        return mapper.readTree(responseSting);
    }
    /**
     JsonNode obj = mapper.readTree(responseSting);
     JsonNode links = obj.get("_links");
     JsonNode programJson = links.get("program");
     Program program = new Program();*/

    public JsonNode createProgramsUsingAllVariables(String serverURL, Integer portNumber, String token) throws IOException {
        String APIUrl = serverURL + portNumber + "/api/programs" + token;
        String programCode = RandomStringUtils.randomAlphabetic(5);
        String programName = RandomStringUtils.randomAlphabetic(5);
        String programDescription = RandomStringUtils.randomAlphabetic(15);
        String programActive = "false";
        String programPeriodsSkippable = "false";
        String programShowNonFullSupplyTab = "false";
        String APIBody = "{\"code\":\"" + programCode + "\"," +
                "\"name\":\"" + programName + "\"," +
                "\"description\":\"" + programDescription + "\"," +
                "\"active\":\"" + programActive + "\"," +
                "\"periodsSkippable\":\"" + programPeriodsSkippable + "\"," +
                "\"showNonFullSupplyTab\":\"" + programShowNonFullSupplyTab + "\"}";
        builder.setContentType("application/json");
        builder.setBody(APIBody);
        RequestSpecification requestSpec = builder.build();
        Response response = given().spec(requestSpec).post(APIUrl);
        String responseSting = response.asString();
        return mapper.readTree(responseSting);
    }

    public JsonNode editProgramCodeAndProgramName(String serverURL, Integer portNumber, String token, JsonNode program) throws IOException {
        List params = new ArrayList();
        String APIUrl = serverURL + portNumber + "/api/programs" + token;
        String programCode = RandomStringUtils.randomAlphabetic(5);
        String programName = RandomStringUtils.randomAlphabetic(5);
        JsonNode links = program.get("_links");
        JsonNode programJson = links.get("program");
        String id = programJson.get("href").asText();
        System.out.println("ID BEFORE: " + id);
        id = id.substring((serverURL + portNumber + "/api/programs").length());
        System.out.println("ID AFTER: " + id);
        String APIBody = "{\"id\":\"" + id + "\"," +
                "\"code\":\"" + programCode + "\"," +
                "\"name\":\"" + programName + "\"," +
                "\"periodsSkippable\":" + false + "}";
        builder.setBody(APIBody);
        RequestSpecification requestSpec = builder.build();
        Response response = given().spec(requestSpec).post(APIUrl);
        String responseSting = response.asString();
        return mapper.readTree(responseSting);
    }
}
