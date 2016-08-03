package org.openlmis.referencedata.restApi.tests;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.restApi.requestMethods.Program;
import org.openlmis.referencedata.restApi.requestMethods.Token;

import java.io.IOException;

/**
 * Created by user on 8/2/16.
 */
public class EditProgramCodeAndNameRestTest {
    public static final String SERVER_URL = "http://localhost:";
    public static final Integer SERVER_PORT = 8080;
    public String tokenValue = "?access_token=";
    Token token = new Token();
    Program program = new Program();

    @Before
    public void createToken() throws IOException {
        tokenValue += token.returnCreatedToken(SERVER_URL);
        System.out.println("Token = " + token.returnCreatedToken(SERVER_URL));
    }

    @Test //OLMIS-230
    public void createRequisiton() throws IOException {
        JsonNode program1 = program.createProgramsUsingAllVariables(SERVER_URL, SERVER_PORT, tokenValue);
        JsonNode program2 = program.editProgramCodeAndProgramName(SERVER_URL, SERVER_PORT, tokenValue, program1);
    }
}
