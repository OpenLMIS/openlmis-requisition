package org.openlmis.requisition.service;

import static org.hamcrest.Matchers.contains;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.common.collect.ImmutableMap;

import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public abstract class BaseCommunicationServiceTest {
  private static final String TOKEN = UUID.randomUUID().toString();
  private static final String AUTHORIZATION_URL = "http://localhost/auth/oauth/token";

  protected static final String ACCESS_TOKEN = "access_token=" + TOKEN;

  private static final URI AUTHORIZATION_URI =
      URI.create(AUTHORIZATION_URL + "?grant_type=client_credentials");

  @Mock
  protected RestTemplate restTemplate;

  @Captor
  protected ArgumentCaptor<URI> uriCaptor;

  @Captor
  private ArgumentCaptor<HttpEntity<String>> entityStringCaptor;

  @Before
  public void setUp() throws Exception {
    mockAuth();
  }

  @After
  public void tearDown() throws Exception {
    checkAuth();
  }

  protected abstract BaseCommunicationService getService();

  protected BaseCommunicationService prepareService() {
    BaseCommunicationService service = getService();
    service.setRestTemplate(restTemplate);

    ReflectionTestUtils.setField(service, "clientId", "trusted-client");
    ReflectionTestUtils.setField(service, "clientSecret", "secret");
    ReflectionTestUtils.setField(service, "authorizationUrl", AUTHORIZATION_URL);

    return service;
  }

  private void mockAuth() {
    ResponseEntity<Object> response = mock(ResponseEntity.class);
    Map<String, String> body = ImmutableMap.of("access_token", TOKEN);

    when(restTemplate.exchange(
        eq(AUTHORIZATION_URI), eq(HttpMethod.POST), any(HttpEntity.class), eq(Object.class)
    )).thenReturn(response);

    when(response.getBody()).thenReturn(body);
  }

  private void checkAuth() {
    verify(restTemplate, atLeastOnce()).exchange(
        eq(AUTHORIZATION_URI), eq(HttpMethod.POST), entityStringCaptor.capture(), eq(Object.class)
    );

    List<HttpEntity<String>> entities = entityStringCaptor.getAllValues();
    for (HttpEntity entity : entities) {
      assertThat(
          entity.getHeaders().get("Authorization"),
          contains("Basic dHJ1c3RlZC1jbGllbnQ6c2VjcmV0")
      );
    }

  }

}
