package org.openlmis.requisition.service;

import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import com.google.common.collect.Maps;

import org.junit.Test;

import java.util.Map;

public class RequestParametersTest {

  @Test
  public void shouldSetParameter() throws Exception {
    RequestParameters params = RequestParameters.init().set("a", "b");
    assertThat(toMap(params), hasEntry("a", "b"));
  }

  @Test
  public void shouldNotSetParametersValueIsNull() throws Exception {
    RequestParameters params = RequestParameters.init().set("a", null);
    assertThat(toMap(params), not(hasKey("a")));
  }

  @Test
  public void shouldSetAllParametersFromOtherInstance() throws Exception {
    RequestParameters parent = RequestParameters.init().set("a", "b");
    RequestParameters params = RequestParameters.init().setAll(parent);

    assertThat(toMap(params), hasEntry("a", "b"));
  }

  private Map<String, Object> toMap(RequestParameters parameters) {
    Map<String, Object> map = Maps.newHashMap();
    parameters.forEach(e -> map.put(e.getKey(), e.getValue()));

    return map;
  }
}
