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

package org.openlmis.requisition.service.referencedata;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;

import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Matchers;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.service.BaseCommunicationServiceTest;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;
import java.net.URI;
import java.time.LocalDate;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class PeriodReferenceDataServiceTest
    extends BaseCommunicationServiceTest<ProcessingPeriodDto> {

  @Override
  protected ProcessingPeriodDto generateInstance() {
    return new ProcessingPeriodDto();
  }

  @Override
  protected BaseCommunicationService<ProcessingPeriodDto> getService() {
    return new PeriodReferenceDataService();
  }

  @Test
  public void shouldSearchProcessingPeriodsByScheduleIdAndEndDate() {
    UUID scheduleId = UUID.randomUUID();
    LocalDate date = LocalDate.now();

    PeriodReferenceDataService service = (PeriodReferenceDataService) prepareService();
    ProcessingPeriodDto period = mockPageResponseEntityAndGetDto();

    // when
    Collection<ProcessingPeriodDto> result = service.search(scheduleId, date);

    // then
    assertThat(result, hasSize(1));
    assertTrue(result.contains(period));

    verify(restTemplate, atLeastOnce()).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET), entityCaptor.capture(),
        Matchers.any(DynamicPageTypeReference.class)
    );

    URI uri = uriCaptor.getValue();
    List<NameValuePair> parse = URLEncodedUtils.parse(uri, "UTF-8");

    assertQueryParameter(parse, "processingScheduleId", scheduleId);
    assertQueryParameter(parse, "endDate", date);
    assertQueryParameter(parse, "size", 2000);

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

  @Test
  public void shouldSearchProcessingPeriodsByFacilityAndProgram() {
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();

    PeriodReferenceDataService service = (PeriodReferenceDataService) prepareService();
    ProcessingPeriodDto period = mockPageResponseEntityAndGetDto();

    // when
    Collection<ProcessingPeriodDto> result = service
        .searchByProgramAndFacility(programId, facilityId);

    // then
    assertThat(result, hasSize(1));
    assertTrue(result.contains(period));

    verify(restTemplate, atLeastOnce()).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET), entityCaptor.capture(),
        Matchers.any(DynamicPageTypeReference.class)
    );

    URI uri = uriCaptor.getValue();
    List<NameValuePair> parse = URLEncodedUtils.parse(uri, "UTF-8");

    assertQueryParameter(parse, "facilityId", facilityId);
    assertQueryParameter(parse, "programId", programId);
    assertQueryParameter(parse, "size", 2000);

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }
}
