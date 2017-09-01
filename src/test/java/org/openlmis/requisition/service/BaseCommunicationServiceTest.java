/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.service;

import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import java.net.URI;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public abstract class BaseCommunicationServiceTest<T> {
  protected static final String TOKEN = UUID.randomUUID().toString();

  @Mock
  protected RestTemplate restTemplate;

  @Mock
  private AuthService authService;

  @Captor
  protected ArgumentCaptor<URI> uriCaptor;

  @Captor
  protected ArgumentCaptor<HttpEntity> entityCaptor;

  @Rule
  public final ExpectedException expectedException = ExpectedException.none();

  @Before
  public void setUp() {
    mockAuth();
  }

  @After
  public void tearDown() {
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
    when(restTemplate.exchange(
        any(URI.class), eq(HttpMethod.GET), any(HttpEntity.class),
            eq(service.getResultClass())
    )).thenReturn(response);

    T found = service.findOne(id);

    // then
    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), eq(service.getResultClass())
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + id;

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, is(instance));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

  @Test
  public void shouldReturnNullIfEntityCannotBeFoundById() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.exchange(
        any(URI.class), eq(HttpMethod.GET),
        any(HttpEntity.class), eq(service.getResultClass())
    )).thenThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND));

    T found = service.findOne(id);

    // then
    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), eq(service.getResultClass())
    );

    URI uri = uriCaptor.getValue();
    String url = service.getServiceUrl() + service.getUrl() + id;

    assertThat(uri.toString(), is(equalTo(url)));
    assertThat(found, is(nullValue()));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

  @Test(expected = DataRetrievalException.class)
  public void shouldThrowExceptionIfThereIsOtherProblemWithFindingById() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.exchange(
        any(URI.class), eq(HttpMethod.GET), any(HttpEntity.class),
            eq(service.getResultClass())
    )).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

    service.findOne(id);
  }

  @Test
  public void shouldRetryObtainingAccessToken() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    HttpStatusCodeException exception = mock(HttpStatusCodeException.class);
    when(exception.getStatusCode()).thenReturn(HttpStatus.UNAUTHORIZED);
    when(exception.getResponseBodyAsString()).thenReturn(
        "{\"error\":\"invalid_token\",\"error_description\":\"" + UUID.randomUUID() + "}");
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.exchange(
        any(URI.class), eq(HttpMethod.GET), any(HttpEntity.class),
        eq(service.getResultClass())
    )).thenThrow(exception);

    expectedException.expect(DataRetrievalException.class);
    service.findOne(id);

    verify(authService, times(1)).clearTokenCache();
    verify(authService, times(2)).obtainAccessToken();
  }

  @Test
  public void shouldRetryObtainingAccessTokenIfResponseBodyIsEmpty() throws Exception {
    // given
    BaseCommunicationService<T> service = prepareService();
    HttpStatusCodeException exception = mock(HttpStatusCodeException.class);
    when(exception.getStatusCode()).thenReturn(HttpStatus.UNAUTHORIZED);
    when(exception.getResponseBodyAsString()).thenReturn("");
    UUID id = UUID.randomUUID();

    // when
    when(restTemplate.exchange(
        any(URI.class), eq(HttpMethod.GET), any(HttpEntity.class),
        eq(service.getResultClass())
    )).thenThrow(exception);

    expectedException.expect(DataRetrievalException.class);
    service.findOne(id);

    verify(authService, times(1)).clearTokenCache();
    verify(authService, times(2)).obtainAccessToken();
  }

  protected abstract T generateInstance();

  protected abstract BaseCommunicationService<T> getService();

  protected BaseCommunicationService prepareService() {
    BaseCommunicationService service = getService();
    service.setRestTemplate(restTemplate);
    service.setAuthService(authService);

    return service;
  }

  protected void assertAuthHeader(HttpEntity entity) {
    assertThat(entity.getHeaders().get(HttpHeaders.AUTHORIZATION),
            is(singletonList("Bearer " + TOKEN)));
  }

  private void mockAuth() {
    when(authService.obtainAccessToken()).thenReturn(TOKEN);
  }

  private void checkAuth() {
    verify(authService, atLeastOnce()).obtainAccessToken();
  }

}
