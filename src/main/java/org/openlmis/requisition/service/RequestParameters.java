package org.openlmis.requisition.service;

import com.google.common.collect.Maps;

import java.util.Map;
import java.util.function.Consumer;

public final class RequestParameters {
  private Map<String, Object> params = Maps.newHashMap();

  private RequestParameters() {
  }

  public static RequestParameters init() {
    return new RequestParameters();
  }

  /**
   * Set parameter (key argument) with the value only if the value is not null.
   */
  public RequestParameters set(String key, Object value) {
    if (null != value) {
      params.put(key, value);
    }

    return this;
  }

  public RequestParameters setAll(RequestParameters parameters) {
    parameters.forEach(entry -> set(entry.getKey(), entry.getValue()));
    return this;
  }

  public void forEach(Consumer<Map.Entry<String, Object>> action) {
    params.entrySet().forEach(action);
  }

}
