package org.openlmis.requisition.service;

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;

import org.openlmis.requisition.dto.ResultDto;
import org.openlmis.utils.DynamicParameterizedTypeReference;
import org.openlmis.utils.RequestHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestOperations;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public abstract class BaseCommunicationService<T> {
  protected final Logger logger = LoggerFactory.getLogger(getClass());

  protected RestOperations restTemplate = new RestTemplate();

  private AuthService authService;

  protected abstract String getServiceUrl();

  protected abstract String getUrl();

  protected abstract Class<T> getResultClass();

  protected abstract Class<T[]> getArrayResultClass();

  /**
   * Return one object from Reference data service.
   *
   * @param id UUID of requesting object.
   * @return Requesting reference data object.
   */
  public T findOne(UUID id) {
    return findOne(id.toString(), RequestParameters.init());
  }

  /**
   * Return one object from Reference data service.
   *
   * @param resourceUrl Endpoint url.
   * @param parameters  Map of query parameters.
   * @return one reference data T objects.
   */
  public T findOne(String resourceUrl, RequestParameters parameters) {
    String url = getServiceUrl() + getUrl() + resourceUrl;

    RequestParameters params = RequestParameters
        .init()
        .setAll(parameters)
        .set(ACCESS_TOKEN, obtainAccessToken());

    try {
      return restTemplate
          .getForEntity(createUri(url, params), getResultClass())
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
        .set(ACCESS_TOKEN, obtainAccessToken());

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
        .set(ACCESS_TOKEN, obtainAccessToken());

    ResponseEntity<ResultDto<P>> response = restTemplate.exchange(
        createUri(url, params),
        HttpMethod.GET,
        null,
        new DynamicParameterizedTypeReference<>(type)
    );

    return response.getBody();
  }

  protected String obtainAccessToken() {
    return authService.obtainAccessToken();
  }

  protected URI createUri(String url, RequestParameters parameters) {
    return RequestHelper.createUri(url, parameters);
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
