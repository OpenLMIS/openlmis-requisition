package org.openlmis.requisition.controller;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.Application;
import org.openlmis.requisition.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.http.MediaType;

import org.springframework.restdocs.RestDocumentation;
import org.springframework.restdocs.mockmvc.RestDocumentationResultHandler;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import org.openlmis.requisition.domain.*;
import org.openlmis.restdocs.*;

import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.document;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;
import static org.springframework.restdocs.operation.preprocess.Preprocessors.*;
import static org.springframework.restdocs.payload.PayloadDocumentation.fieldWithPath;
import static org.springframework.restdocs.payload.PayloadDocumentation.requestFields;
import static org.springframework.restdocs.payload.PayloadDocumentation.responseFields;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = Application.class)
@WebAppConfiguration
public class FacilityRepositoryTest
{
    @Rule
    public final RestDocumentation restDocumentation = DocumentationUtility.getRestDocumentation();

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ObjectMapper objectMapper;

    private MockMvc mockMvc;

    private RestDocumentationResultHandler document;

    @Before
    public void setUp() throws IOException
    {
        this.document = document("{method-name}", preprocessRequest(prettyPrint()), preprocessResponse(prettyPrint()));
        this.mockMvc = MockMvcBuilders.webAppContextSetup(this.context)
                .apply(documentationConfiguration(this.restDocumentation))
                .alwaysDo(this.document)
                .build();

        /* For each domain object which is exposed via Spring Data REST and which therefore needs little or no HTTP-based testing,
          create and call a describeMyClassProperties() method. It’s job is to write a file called MyClassProperties.adoc to disk.
          This file contains a description of the class, and is intended to be referenced by the main index.adoc file. */
        describeUserProperties();
    }

    private static void describeUserProperties() throws IOException
    {
        Map<String, List<String>> userProperties = DocumentationUtility.getClassProperties(User.class);
        userProperties.get("verified").add("Whether the user has validated their account by clicking on a link sent to their email address");
        userProperties.get("active").add("A flag which allows system administrators to deactivate user without actually having to delete them from the system");

        DocumentationUtility.writeClassProperties("User", userProperties);
    }


    /*
        Non-trivial endpoints (presumably added explicitly through a Spring MVC Controller rather than automatically generated via
        Spring Data REST) may be tested as well as documented as shown below.

        Although the example primarily tests for the existence of a 200 (OK) status, there’s no reason it couldn't perform more
        interesting checks.

        Note that the responseFields method allows us to define the response expected from the server. It’s used to construct the
        "response-fields.adoc" file snippet used within our documentation. Additionally, it serves as a test: if a mismatch is found
        between the entries specified in responseFields and those returned by the server, this specific test will fail.
     */
    @Test
    public void getUserAlternative() throws Exception
    {
        User sampleUser = saveSampleUser("firstName", "lastName");

        this.document.snippets(
                responseFields(
                        fieldWithPath("firstName").description("The person's first name"),
                        fieldWithPath("lastName").description("The person's last name"),
                        fieldWithPath("username").description("The person's user name"),
                        fieldWithPath("verified").description("Whether the person has finalized the creation of their account by following a link sent to their email address"),
                        fieldWithPath("active").description("Whether the person may log in. This flag is useful because it allows system administrators to suspend access without having to delete a user."),
                        fieldWithPath("_links").description("Some description goes here")
                )
        );

        this.mockMvc.perform(
                get("/api/users/" + sampleUser.getId()).accept(MediaType.APPLICATION_JSON)
        ).andExpect(status().isOk());
    }

    private User saveSampleUser(String firstName, String lastName)
    {
        User user = new User();
        user.setFirstName(firstName);
        user.setLastName(lastName);
        user.setUsername(firstName + "_" + lastName);
        user.setPassword(user.getUsername());
        return userRepository.save(user);
    }


}
