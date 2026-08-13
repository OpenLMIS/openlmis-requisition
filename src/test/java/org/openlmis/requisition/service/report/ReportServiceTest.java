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

package org.openlmis.requisition.service.report;

import static org.junit.Assert.assertArrayEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.openlmis.requisition.service.AuthService;
import org.openlmis.requisition.service.DataRetrievalException;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestOperations;

@RunWith(MockitoJUnitRunner.class)
public class ReportServiceTest {

  private static final String SERVICE_URL = "http://localhost";
  private static final String REPORT_NAME = "requisition";

  @Mock
  private RestOperations restTemplate;

  @Mock
  private AuthService authService;

  @InjectMocks
  private ReportService reportService;

  private byte[] template = new byte[]{1, 2, 3};
  private byte[] generated = new byte[]{4, 5, 6};
  private Map<String, Object> params = new HashMap<>();

  @Before
  public void setUp() {
    ReflectionTestUtils.setField(reportService, "restTemplate", restTemplate);
    ReflectionTestUtils.setField(reportService, "authService", authService);
    ReflectionTestUtils.setField(reportService, "reportUrl", SERVICE_URL);
    when(authService.obtainAccessToken()).thenReturn("token");
    params.put("lang", "fr");
  }

  @Test
  public void shouldReturnGeneratedReport() {
    when(restTemplate.exchange(any(URI.class), eq(HttpMethod.POST), any(HttpEntity.class),
        eq(byte[].class))).thenReturn(new ResponseEntity<>(generated, HttpStatus.OK));

    assertArrayEquals(generated, reportService.generate(REPORT_NAME, template, params));
  }

  @Test
  public void shouldSendNameTemplateAndParamsToReportService() {
    when(restTemplate.exchange(any(URI.class), eq(HttpMethod.POST), any(HttpEntity.class),
        eq(byte[].class))).thenReturn(new ResponseEntity<>(generated, HttpStatus.OK));

    reportService.generate(REPORT_NAME, template, params);

    ArgumentCaptor<HttpEntity> entity = ArgumentCaptor.forClass(HttpEntity.class);
    verify(restTemplate).exchange(any(URI.class), eq(HttpMethod.POST), entity.capture(),
        eq(byte[].class));

    GenerateReportDto sent = (GenerateReportDto) entity.getValue().getBody();
    assertArrayEquals(template, sent.getTemplate());
    org.junit.Assert.assertEquals(REPORT_NAME, sent.getName());
    org.junit.Assert.assertEquals("fr", sent.getParams().get("lang"));
  }

  @Test(expected = DataRetrievalException.class)
  public void shouldThrowDataRetrievalExceptionWhenReportServiceFails() {
    doThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR))
        .when(restTemplate).exchange(any(URI.class), eq(HttpMethod.POST), any(HttpEntity.class),
            eq(byte[].class));

    reportService.generate(REPORT_NAME, template, params);
  }
}
