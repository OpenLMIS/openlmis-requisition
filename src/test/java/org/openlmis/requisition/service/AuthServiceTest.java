package org.openlmis.requisition.service;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.common.collect.ImmutableMap;

import org.junit.Before;
import org.junit.Test;
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
import java.util.Map;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class AuthServiceTest {
  private static final String TOKEN = UUID.randomUUID().toString();
  private static final String AUTHORIZATION_URL = "http://localhost/auth/oauth/token";
  private static final URI AUTHORIZATION_URI = URI.create(
      AUTHORIZATION_URL + "?grant_type=client_credentials"
  );

  @Mock
  private RestTemplate restTemplate;

  @Captor
  private ArgumentCaptor<HttpEntity<String>> entityStringCaptor;

  private AuthService authService;

  @Before
  public void setUp() throws Exception {
    authService = new AuthService();
    authService.setRestTemplate(restTemplate);

    ReflectionTestUtils.setField(authService, "clientId", "trusted-client");
    ReflectionTestUtils.setField(authService, "clientSecret", "secret");
    ReflectionTestUtils.setField(authService, "authorizationUrl", AUTHORIZATION_URL);
  }

  @Test
  public void shouldObtainAccessToken() throws Exception {
    ResponseEntity<Object> response = mock(ResponseEntity.class);
    Map<String, String> body = ImmutableMap.of("access_token", TOKEN);

    when(restTemplate.exchange(
        eq(AUTHORIZATION_URI), eq(HttpMethod.POST), any(HttpEntity.class), eq(Object.class)
    )).thenReturn(response);

    when(response.getBody()).thenReturn(body);

    String token = authService.obtainAccessToken();
    assertThat(token, is(equalTo(TOKEN)));

    verify(restTemplate).exchange(
        eq(AUTHORIZATION_URI), eq(HttpMethod.POST), entityStringCaptor.capture(), eq(Object.class)
    );

    HttpEntity<String> entity = entityStringCaptor.getValue();
    assertThat(
        entity.getHeaders().get("Authorization"),
        contains("Basic dHJ1c3RlZC1jbGllbnQ6c2VjcmV0")
    );
  }
}
