package org.openlmis.referencedata.restApi.tests;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.restApi.requestMethods.Facility;
import org.openlmis.referencedata.restApi.requestMethods.FacilityType;
import org.openlmis.referencedata.restApi.requestMethods.GeographicLevel;
import org.openlmis.referencedata.restApi.requestMethods.GeographicZone;
import org.openlmis.referencedata.restApi.requestMethods.Program;
import org.openlmis.referencedata.restApi.requestMethods.Schedule;
import org.openlmis.referencedata.restApi.requestMethods.Token;

import java.io.IOException;

/**
 * Created by user on 8/2/16.
 */
public class CreateRequisitionRestTest {

    public static final String SERVER_URL = "http://localhost:";
    public static final Integer SERVER_PORT = 8080;
    public String tokenValue = "?access_token=";
    Token token = new Token();
    Program program = new Program();
    GeographicLevel geographicLevel = new GeographicLevel();
    GeographicZone geographicZone = new GeographicZone();
    FacilityType facilityType = new FacilityType();
    Facility facility = new Facility();
    Schedule schedule = new Schedule();

    @Before
    public void createToken() throws IOException {
        tokenValue += token.returnCreatedToken(SERVER_URL);
    }

    @Test //OLMIS-760
    public void createRequisiton() throws IOException {
        JsonNode programJson = program.createPrograms(SERVER_URL, SERVER_PORT, tokenValue);
        JsonNode geographicLevelJson = geographicLevel.createGeographicLevel(SERVER_URL, SERVER_PORT,  tokenValue);
        String geographicLevelHref = geographicLevelJson.get("_links").get("geographicLevel").get("href").asText();
        JsonNode geographicZoneJson = geographicZone.createGeographicZones(SERVER_URL, SERVER_PORT,  tokenValue, geographicLevelHref);
        String geographicZoneHref = geographicZoneJson.get("_links").get("geographicZone").get("href").asText();
        JsonNode facilityTypeJson = facilityType.createFacilityType(SERVER_URL, SERVER_PORT,  tokenValue);
        String facilityTypesHref = facilityTypeJson.get("_links").get("facilityType").get("href").asText();
        JsonNode facilityJson = facility.createFacility(SERVER_URL, SERVER_PORT, tokenValue, facilityTypesHref, geographicZoneHref);
        JsonNode scheduleJson = schedule.createSchedule(SERVER_URL, SERVER_PORT, tokenValue);
    }
}
