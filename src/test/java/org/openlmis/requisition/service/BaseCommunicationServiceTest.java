package org.openlmis.requisition.service;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public abstract class BaseCommunicationServiceTest<T> {
  private static final String TOKEN = UUID.randomUUID().toString();
  protected static final String ACCESS_TOKEN = "access_token=" + TOKEN;

  @Mock
  protected RestTemplate restTemplate;

  @Mock
  private AuthService authService;

  @Captor
  protected ArgumentCaptor<URI> uriCaptor;

  @Before
  public void setUp() throws Exception {
    mockAuth();
  }

  @After
  public void tearDown() throws Exception {
    checkAuth();
  }

  @Test
  public void shouldFindById() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    UUID id = UUID.randomUUID();
    T instance = generateInstance();
    ResponseEntity<T> response = mock(ResponseEntity.class);

    // when
    when(response.getBody()).thenReturn(instance);
    when(restTemplate.getForEntity(
        any(URI.class), eq(service.getResultClass())
    )).thenReturn(response);

    T found = service.findOne(id);

    // then
    verify(restTemplate).getForEntity(
        uriCaptor.capture(), eq(service.getResultClass())
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + id + "?" + ACCESS_TOKEN;

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, is(instance));
  }

  @Test
  public void shouldReturnNullIfEntityCannotBeFoundById() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.getForEntity(
        any(URI.class), eq(service.getResultClass())
    )).thenThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND));

    T found = service.findOne(id);

    // then
    verify(restTemplate).getForEntity(
        uriCaptor.capture(), eq(service.getResultClass())
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + id + "?" + ACCESS_TOKEN;

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, is(nullValue()));
  }

  @Test(expected = DataRetrievalException.class)
  public void shouldThrowExceptionIfThereIsOtherProblemWithFindingById() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.getForEntity(
        any(URI.class), eq(service.getResultClass())
    )).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

    service.findOne(id);
  }

  protected abstract T generateInstance();

  protected abstract BaseCommunicationService<T> getService();

  protected BaseCommunicationService prepareService() {
    BaseCommunicationService service = getService();
    service.setRestTemplate(restTemplate);
    service.setAuthService(authService);

    return service;
  }

  private void mockAuth() {
    when(authService.obtainAccessToken()).thenReturn(TOKEN);
  }

  private void checkAuth() {
    verify(authService, atLeastOnce()).obtainAccessToken();
  }

}
