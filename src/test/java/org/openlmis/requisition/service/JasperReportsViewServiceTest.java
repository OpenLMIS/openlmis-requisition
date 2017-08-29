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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.dto.TimelinessReportFacilityDto.DISTRICT_LEVEL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.GeographicLevelDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.GeographicZoneReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.data.domain.Page;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

@RunWith(MockitoJUnitRunner.class)
public class JasperReportsViewServiceTest {

  @InjectMocks
  private JasperReportsViewService service;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private GeographicZoneReferenceDataService geographicZoneReferenceDataService;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private ProgramDto program;

  @Mock
  private ProcessingPeriodDto period;

  @Mock
  private GeographicZoneDto district;

  private UUID programId = UUID.randomUUID();
  private UUID periodId = UUID.randomUUID();

  private Map<String, Object> reportParams = new HashMap<>();

  @Before
  public void setUp() {
    when(program.getId()).thenReturn(programId);
    when(period.getId()).thenReturn(periodId);

    when(programReferenceDataService.findOne(programId)).thenReturn(program);
    when(periodReferenceDataService.findOne(periodId)).thenReturn(period);

    reportParams.put("program", programId.toString());
    reportParams.put("period", periodId.toString());
  }

  @Test
  public void shouldSetViewParamsForTimelinessReport() {
    //given
    UUID districtId = UUID.randomUUID();
    reportParams.put("district", districtId.toString());
    when(geographicZoneReferenceDataService.findOne(districtId)).thenReturn(district);

    // when
    ModelAndView view = service.getTimelinessJasperReportView(
        new JasperReportsMultiFormatView(), reportParams);
    Map<String, Object> outputParams = view.getModel();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    Assert.assertEquals(program, outputParams.get("program"));
    Assert.assertEquals(period, outputParams.get("period"));
    Assert.assertEquals(district, outputParams.get("district"));
    Assert.assertEquals(facilities, Collections.emptyList());
  }

  @Test
  public void shouldGetTimelinessReportViewWithActiveFacilitiesMissingRnR() {
    // given
    List<FacilityDto> facilitiesToReturn = new ArrayList<>();

    // active facilities missing RnR
    FacilityDto facility = mockFacility(true, true, UUID.randomUUID(), "Test", "Test");
    FacilityDto anotherFacility = mockFacility(true, true,
        UUID.randomUUID(), "zone", "facility");
    facilitiesToReturn.add(facility);
    facilitiesToReturn.add(anotherFacility);

    // "on time" active facility
    facilitiesToReturn.add(mockFacility(true, false));

    // inactive facilitieskurwa
    facilitiesToReturn.add(mockFacility(false, false));
    facilitiesToReturn.add(mockFacility(false, true));

    when(facilityReferenceDataService.findAll()).thenReturn(facilitiesToReturn);

    // when
    ModelAndView view = service.getTimelinessJasperReportView(
        new JasperReportsMultiFormatView(), reportParams);
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(view.getModel());

    // then
    Assert.assertEquals(2, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId).collect(Collectors.toList());
    Assert.assertTrue(facilityIds.contains(facility.getId()));
    Assert.assertTrue(facilityIds.contains(anotherFacility.getId()));
  }

  @Test
  public void shouldGetTimelinessReportViewWithFacilitiesFromSpecifiedDistrict() {
    //given
    UUID districtId = UUID.randomUUID();
    reportParams.put("district", districtId.toString());

    // active facilities missing RnR
    MinimalFacilityDto facility = mockBasicFacility(true, true, districtId, "parent-zone", "f1");
    MinimalFacilityDto childFacility =
        mockBasicFacility(true, true, UUID.randomUUID(), "child-zone", "f2");

    GeographicLevelDto childLevel = mock(GeographicLevelDto.class);
    when(childLevel.getLevelNumber()).thenReturn(DISTRICT_LEVEL + 1);

    GeographicZoneDto childZone = childFacility.getGeographicZone();
    childZone.setParent(facility.getGeographicZone());
    childZone.setLevel(childLevel);

    // facility missing RnR from another district
    mockFacility(true, true);

    when(facilityReferenceDataService.search(any(), any(), eq(districtId), eq(true)))
        .thenReturn(Arrays.asList(facility, childFacility));

    // when
    ModelAndView view = service.getTimelinessJasperReportView(
        new JasperReportsMultiFormatView(), reportParams);
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(view.getModel());

    // then
    Assert.assertEquals(2, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId).collect(Collectors.toList());
    Assert.assertTrue(facilityIds.contains(facility.getId()));
    Assert.assertTrue(facilityIds.contains(childFacility.getId()));
  }

  @Test
  public void shouldGetTimelinessReportViewWithFacilitiesFromAllZonesIfDistrictNotSpecified() {
    //given
    UUID zone1Id = UUID.randomUUID();
    UUID zone2Id = UUID.randomUUID();

    List<FacilityDto> facilitiesToReturn = Arrays.asList(
        // active facilities missing RnR from different zones
        mockFacility(true, true, zone1Id, "district A", "f1"),
        mockFacility(true, true, zone1Id, "district A", "f2"),
        mockFacility(true, true, zone2Id, "district B", "f3"),
        mockFacility(true, true, zone2Id, "district B", "f4")
    );

    when(facilityReferenceDataService.findAll()).thenReturn(facilitiesToReturn);

    // when
    ModelAndView view = service.getTimelinessJasperReportView(
        new JasperReportsMultiFormatView(), reportParams);
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(view.getModel());

    // then
    Assert.assertEquals(4, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId).collect(Collectors.toList());
    for (MinimalFacilityDto facility : facilitiesToReturn) {
      Assert.assertTrue(facilityIds.contains(facility.getId()));
    }
  }

  @Test
  public void shouldSortFacilitiesReturnedWithTimelinessReportView() {
    //given
    UUID zone1Id = UUID.randomUUID();
    UUID zone2Id = UUID.randomUUID();

    FacilityDto facility1A = mockFacility(true, true, zone1Id, "zone1", "facilityA");
    FacilityDto facility1B = mockFacility(true, true, zone1Id, "zone1", "facilityB");
    FacilityDto facility2A = mockFacility(true, true, zone2Id, "zone2", "facilityA");
    FacilityDto facility2B = mockFacility(true, true, zone2Id, "zone2", "facilityB");

    when(facilityReferenceDataService.findAll()).thenReturn(Arrays.asList(
        facility2B, facility2A, facility1A, facility1B));

    // when
    ModelAndView view = service.getTimelinessJasperReportView(
        new JasperReportsMultiFormatView(), reportParams);
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(view.getModel());

    // then
    Assert.assertEquals(4, facilities.size());
    Assert.assertEquals(facility1A.getId(), facilities.get(0).getId());
    Assert.assertEquals(facility1B.getId(), facilities.get(1).getId());
    Assert.assertEquals(facility2A.getId(), facilities.get(2).getId());
    Assert.assertEquals(facility2B.getId(), facilities.get(3).getId());
  }

  private List<FacilityDto> extractFacilitiesFromOutputParams(Map<String, Object> outputParams) {
    JRBeanCollectionDataSource datasource =
        (JRBeanCollectionDataSource) outputParams.get("datasource");
    return (List<FacilityDto>) datasource.getData();
  }

  private FacilityDto mockFacility(boolean isActive, boolean isMissingRnR) {
    return mockFacility(isActive, isMissingRnR, UUID.randomUUID(), "test", "test");
  }

  private FacilityDto mockFacility(boolean isActive, boolean isMissingRnR, UUID districtId,
                                   String districtName, String facilityName) {
    FacilityDto facility = new FacilityDto();

    UUID facilityId = UUID.randomUUID();
    facility.setId(facilityId);
    facility.setActive(isActive);
    facility.setName(facilityName);

    facility.setGeographicZone(
        mockGeographicZone(districtId, districtName, facilityId, isMissingRnR));

    return facility;
  }

  private MinimalFacilityDto mockBasicFacility(boolean isActive, boolean isMissingRnR,
                                               UUID districtId, String districtName,
                                               String facilityName) {
    MinimalFacilityDto facility = new MinimalFacilityDto();

    UUID facilityId = UUID.randomUUID();
    facility.setId(facilityId);
    facility.setActive(isActive);
    facility.setName(facilityName);

    facility.setGeographicZone(
        mockGeographicZone(districtId, districtName, facilityId, isMissingRnR));

    return facility;
  }

  private GeographicZoneDto mockGeographicZone(UUID districtId, String districtName,
                                               UUID facilityId, boolean isMissingRnR) {
    GeographicZoneDto geographicZoneDto = mock(GeographicZoneDto.class);
    when(geographicZoneDto.getId()).thenReturn(districtId);
    when(geographicZoneDto.getName()).thenReturn(districtName);

    GeographicLevelDto geographicLevelDto = mock(GeographicLevelDto.class);
    when(geographicLevelDto.getLevelNumber()).thenReturn(DISTRICT_LEVEL);
    when(geographicZoneDto.getLevel()).thenReturn(geographicLevelDto);
    when(geographicZoneReferenceDataService.findOne(districtId)).thenReturn(geographicZoneDto);

    Requisition mockRequisition = mock(Requisition.class);
    Page<Requisition> requisitionPage = mock(Page.class);
    when(requisitionPage.getContent()).thenReturn(
        (isMissingRnR) ? Collections.emptyList() : Collections.singletonList(mockRequisition));
    
    when(requisitionService.searchRequisitions(eq(facilityId), eq(programId), any(), any(),
        eq(periodId), any(), any(), any(), any()))
        .thenReturn(requisitionPage);

    return geographicZoneDto;
  }
}
