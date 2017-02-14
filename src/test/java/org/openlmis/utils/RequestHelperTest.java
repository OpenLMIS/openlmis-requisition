package org.openlmis.utils;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.openlmis.requisition.service.RequestParameters;

import java.net.URI;

public class RequestHelperTest {

  @Test
  public void shouldCreateUriWithoutParameters() throws Exception {
    URI uri = RequestHelper.createUri("http://localhost", RequestParameters.init());
    assertThat(uri.getQuery(), is(nullValue()));
  }

  @Test
  public void shouldCreateUriWithParameters() throws Exception {
    URI uri = RequestHelper.createUri("http://localhost", RequestParameters.init().set("a", "b"));
    assertThat(uri.getQuery(), is("a=b"));
  }

  @Test
  public void shouldCreateUriWithIncorrectParameters() throws Exception {
    URI uri = RequestHelper.createUri("http://localhost", RequestParameters.init().set("a", "b c"));
    assertThat(uri.getQuery(), is("a=b c"));
  }
}
