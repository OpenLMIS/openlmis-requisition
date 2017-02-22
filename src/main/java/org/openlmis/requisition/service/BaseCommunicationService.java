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

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;
import static org.openlmis.utils.RequestHelper.createUri;

import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.utils.DynamicParameterizedTypeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestOperations;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("PMD.TooManyMethods")
public abstract class BaseCommunicationService<T> {
  protected final Logger logger = LoggerFactory.getLogger(getClass());

  protected RestOperations restTemplate = new RestTemplate();

  protected AuthService authService;

  protected abstract String getServiceUrl();

  protected abstract String getUrl();

  protected abstract Class<T> getResultClass();

  protected abstract Class<T[]> getArrayResultClass();

  /**
   * Return one object from service.
   *
   * @param id UUID of requesting object.
   * @return Requesting reference data object.
   */
  public T findOne(UUID id) {
    return findOne(id.toString(), RequestParameters.init());
  }

  /**
   * Return one object from service.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @return one reference data T objects.
   */
  public T findOne(String resourceUrl, RequestParameters parameters) {
    return findOne(resourceUrl, parameters, getResultClass());
  }

  /**
   * Return one object from service.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @param type        set to what type a response should be converted.
   * @return one reference data T objects.
   */
  public <P> P findOne(String resourceUrl, RequestParameters parameters, Class<P> type) {
    String url = getServiceUrl() + getUrl() + resourceUrl;

    RequestParameters params = RequestParameters
        .init()
        .setAll(parameters)
        .set(ACCESS_TOKEN, authService.obtainAccessToken());

    try {
      return restTemplate
          .getForEntity(createUri(url, params), type)
          .getBody();
    } catch (HttpStatusCodeException ex) {
      // rest template will handle 404 as an exception, instead of returning null
      if (HttpStatus.NOT_FOUND == ex.getStatusCode()) {
        logger.warn(
            "{} matching params does not exist. Params: {}",
            getResultClass().getSimpleName(), parameters
        );

        return null;
      }

      throw buildDataRetrievalException(ex);
    }
  }

  public List<T> findAll() {
    return findAll("");
  }

  public List<T> findAll(String resourceUrl) {
    return findAll(resourceUrl, getArrayResultClass());
  }

  public <P> List<P> findAll(String resourceUrl, Class<P[]> type) {
    return findAll(resourceUrl, RequestParameters.init(), type);
  }

  /**
   * Return all reference data T objects.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @return all reference data T objects.
   */
  public List<T> findAll(String resourceUrl, RequestParameters parameters) {
    return findAll(resourceUrl, parameters, getArrayResultClass());
  }

  public <P> List<P> findAll(String resourceUrl, RequestParameters parameters, Class<P[]> type) {
    return findAll(resourceUrl, parameters, null, HttpMethod.GET, type);
  }

  /**
   * Return all reference data T objects that need to be retrieved with POST request.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @param payload     body to include with the outgoing request.
   * @return all reference data T objects.
   */
  protected List<T> findAll(String resourceUrl, RequestParameters parameters,
                            Object payload) {
    return findAll(resourceUrl, parameters, payload, HttpMethod.POST, getArrayResultClass());
  }

  protected <P> List<P> findAll(String resourceUrl, RequestParameters parameters,
                          Object payload, HttpMethod method, Class<P[]> type) {
    String url = getServiceUrl() + getUrl() + resourceUrl;

    RequestParameters params = RequestParameters
        .init()
        .setAll(parameters)
        .set(ACCESS_TOKEN, authService.obtainAccessToken());

    try {
      ResponseEntity<P[]> response;

      if (HttpMethod.GET == method) {
        response = restTemplate
            .getForEntity(createUri(url, params), type);
      } else {
        response = restTemplate
            .postForEntity(createUri(url, params), payload, type);
      }

      return Stream.of(response.getBody()).collect(Collectors.toList());
    } catch (HttpStatusCodeException ex) {
      throw buildDataRetrievalException(ex);
    }
  }

  protected <P> ResultDto<P> getResult(String resourceUrl, RequestParameters parameters,
                                       Class<P> type) {
    String url = getServiceUrl() + getUrl() + resourceUrl;
    RequestParameters params = RequestParameters
        .init()
        .setAll(parameters)
        .set(ACCESS_TOKEN, authService.obtainAccessToken());

    ResponseEntity<ResultDto<P>> response = restTemplate.exchange(
        createUri(url, params),
        HttpMethod.GET,
        null,
        new DynamicParameterizedTypeReference<>(type)
    );

    return response.getBody();
  }

  private DataRetrievalException buildDataRetrievalException(HttpStatusCodeException ex) {
    return new DataRetrievalException(getResultClass().getSimpleName(),
        ex.getStatusCode(),
        ex.getResponseBodyAsString());
  }

  @Autowired
  public void setAuthService(AuthService authService) {
    this.authService = authService;
  }

  void setRestTemplate(RestOperations template) {
    this.restTemplate = template;
  }

}
