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
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED_WITHOUT_ORDER;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.sql.DataSource;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.GeographicLevelDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReportingRateReportDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.repository.custom.DefaultRequisitionSearchParams;
import org.openlmis.requisition.repository.custom.RequisitionSearchParams;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.GeographicZoneReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.report.ReportService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.web.ReportingRateReportDtoBuilder;
import org.openlmis.requisition.web.RequisitionReportDtoBuilder;
import org.springframework.data.domain.Page;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings({"PMD.TooManyMethods"})
@RunWith(MockitoJUnitRunner.class)
public class JasperReportsViewServiceTest {

  private static final String DATE_FORMAT = "dd/MM/yyyy";
  private static final String GROUPING_SEPARATOR = ",";
  private static final String GROUPING_SIZE = "3";
  private static final String PERIOD = "period";
  private static final String PROGRAM = "program";
  private static final String DISTRICT = "district";
  private static final String DEFAULT_LOCALE = "en";
  private static final String CURRENCY_LOCALE = "US";
  private static final String PARAM_KEY_FORMAT = "format";

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private GeographicZoneReferenceDataService geographicZoneReferenceDataService;

  @Mock
  private ReportingRateReportDtoBuilder reportingRateReportDtoBuilder; //NOPMD

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private JasperTemplate jasperTemplate;

  @Mock
  private JasperPrint jasperPrint;

  @Mock
  private RequisitionReportDtoBuilder requisitionReportDtoBuilder;

  @Mock
  private RequisitionLineItem lineItem1;

  @Mock
  private RequisitionLineItem lineItem2;

  @Mock
  private ReportService reportService;

  @Mock
  private DataSource replicationDataSource; //NOPMD

  private JasperReportsViewService service;

  @Spy
  private RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
      .withAllColumns().build();

  private Requisition requisition;
  private ProgramDto program = DtoGenerator.of(ProgramDto.class);
  private ProcessingPeriodDto period = DtoGenerator.of(ProcessingPeriodDto.class);
  private GeographicZoneDto district = DtoGenerator.of(GeographicZoneDto.class);
  private FacilityDto facility = DtoGenerator.of(FacilityDto.class);
  private SupervisoryNodeDto supervisoryNode = DtoGenerator.of(SupervisoryNodeDto.class);
  private Locale locale = new Locale(DEFAULT_LOCALE, CURRENCY_LOCALE);

  private Map<String, Object> reportParams = new HashMap<>();

  private byte[] expectedReportData;

  @Before
  public void setUp() throws Exception {
    service = org.mockito.Mockito.spy(new JasperReportsViewService(
        replicationDataSource,
        requisitionReportDtoBuilder,
        facilityReferenceDataService,
        programReferenceDataService,
        periodReferenceDataService,
        geographicZoneReferenceDataService,
        requisitionService,
        reportService,
        reportingRateReportDtoBuilder
    ));

    generateRequisition();

    ReflectionTestUtils.setField(service, "dateFormat", DATE_FORMAT);
    ReflectionTestUtils.setField(service, "groupingSeparator", GROUPING_SEPARATOR);
    ReflectionTestUtils.setField(service, "groupingSize", GROUPING_SIZE);
    ReflectionTestUtils.setField(service, "defaultLocale", DEFAULT_LOCALE);
    ReflectionTestUtils.setField(service, "currencyLocale", CURRENCY_LOCALE);

    expectedReportData = new byte[1];

    doReturn(jasperPrint).when(service)
        .fillJasperReport(any(JasperReport.class), anyMap(), any(JRDataSource.class));
    doReturn(expectedReportData).when(service).exportJasperReportToPdf(any(JasperPrint.class));

    when(reportService.generateReport(any(), anyMap())).thenReturn(expectedReportData);
  }

  @Test
  public void generateReportShouldReturnPdfReportAsDefault() {
    //given

    //when
    byte[] reportData = service.generateReport(jasperTemplate, reportParams);
    
    //then
    assertEquals(expectedReportData, reportData);
    verify(reportService).generateReport(jasperTemplate, reportParams);
  }

  @Test
  public void generateReportShouldReturnCsvReport() {
    //given
    reportParams.put(PARAM_KEY_FORMAT, "csv");

    //when
    byte[] reportData = service.generateReport(jasperTemplate, reportParams);

    //then
    assertEquals(expectedReportData, reportData);
    verify(reportService).generateReport(jasperTemplate, reportParams);
  }

  @Test
  public void generateReportShouldReturnXlsReport() {
    //given
    reportParams.put(PARAM_KEY_FORMAT, "xls");

    //when
    byte[] reportData = service.generateReport(jasperTemplate, reportParams);

    //then
    assertEquals(expectedReportData, reportData);
    verify(reportService).generateReport(jasperTemplate, reportParams);
  }

  @Test
  public void generateReportShouldReturnHtmlReport() {
    //given
    reportParams.put(PARAM_KEY_FORMAT, "html");

    //when
    byte[] reportData = service.generateReport(jasperTemplate, reportParams);

    //then
    assertEquals(expectedReportData, reportData);
    verify(reportService).generateReport(jasperTemplate, reportParams);
  }

  @Test
  public void generateTimelinessReportShouldSetViewParams() throws Exception {
    //given
    UUID districtId = UUID.randomUUID();
    reportParams.put(PROGRAM, program.getId().toString());
    reportParams.put(PERIOD, period.getId().toString());
    reportParams.put(DISTRICT, districtId.toString());

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);
    when(geographicZoneReferenceDataService.findOne(districtId)).thenReturn(district);

    // when
    byte[] reportData = service.generateTimelinessReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    assertEquals(expectedReportData, reportData);
    Assert.assertEquals(Collections.emptyList(), facilities);
    Assert.assertEquals(program, outputParams.get(PROGRAM));
    Assert.assertEquals(period, outputParams.get(PERIOD));
    Assert.assertEquals(district, outputParams.get(DISTRICT));
  }

  @Test
  public void generateTimelinessReportShouldGetReportWithActiveFacilitiesMissingRnR()
      throws JasperReportViewException {
    // given
    reportParams.put(PROGRAM, program.getId().toString());
    reportParams.put(PERIOD, period.getId().toString());

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);

    List<FacilityDto> facilitiesToReturn = new ArrayList<>();

    // active facilities missing RnR
    FacilityDto facility = mockFacility(true, true, UUID.randomUUID(), "Test", "Test");
    FacilityDto anotherFacility = mockFacility(
        true, true, UUID.randomUUID(), "zone", "facility");

    facilitiesToReturn.add(facility);
    facilitiesToReturn.add(anotherFacility);

    // "on time" active facility
    facilitiesToReturn.add(mockFacility(true, false));

    // inactive facilities
    facilitiesToReturn.add(mockFacility(false, false));
    facilitiesToReturn.add(mockFacility(false, true));

    when(facilityReferenceDataService.findAll()).thenReturn(facilitiesToReturn);

    // when
    byte[] reportData = service.generateTimelinessReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    assertEquals(expectedReportData, reportData);
    assertEquals(2, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId)
        .collect(Collectors.toList());

    Assert.assertTrue(facilityIds.contains(facility.getId()));
    Assert.assertTrue(facilityIds.contains(anotherFacility.getId()));
  }

  @Test
  public void generateTimelinessReportShouldGetReportWithFacilitiesFromSpecifiedDistrict()
      throws JasperReportViewException {
    //given
    UUID districtId = UUID.randomUUID();
    reportParams.put(DISTRICT, districtId.toString());
    reportParams.put(PROGRAM, program.getId().toString());
    reportParams.put(PERIOD, period.getId().toString());

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);

    // active facilities missing RnR
    MinimalFacilityDto mockedFacility = mockBasicFacility(
        true, true, districtId, "parent-zone", "f1");
    MinimalFacilityDto childFacility = mockBasicFacility(
        true, true, UUID.randomUUID(), "child-zone", "f2");

    GeographicLevelDto childLevel = mock(GeographicLevelDto.class);

    GeographicZoneDto childZone = childFacility.getGeographicZone();
    childZone.setParent(mockedFacility.getGeographicZone());
    childZone.setLevel(childLevel);

    // facility missing RnR from another district
    mockFacility(true, true);

    when(facilityReferenceDataService.search(any(), any(), eq(districtId), eq(true)))
        .thenReturn(Arrays.asList(mockedFacility, childFacility));

    // when
    byte[] reportData = service.generateTimelinessReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    assertEquals(expectedReportData, reportData);
    assertEquals(2, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId)
        .collect(Collectors.toList());

    Assert.assertTrue(facilityIds.contains(mockedFacility.getId()));
    Assert.assertTrue(facilityIds.contains(childFacility.getId()));
  }

  @Test
  public void generateTimelinessReportShouldGetReportWithFacilitiesAllZonesIfDistrictNotSpecified()
      throws JasperReportViewException {
    //given
    reportParams.put(PROGRAM, program.getId().toString());
    reportParams.put(PERIOD, period.getId().toString());

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);

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
    byte[] reportData = service.generateTimelinessReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    assertEquals(expectedReportData, reportData);
    assertEquals(4, facilities.size());
    List<UUID> facilityIds = facilities.stream()
        .map(FacilityDto::getId)
        .collect(Collectors.toList());

    for (MinimalFacilityDto f : facilitiesToReturn) {
      Assert.assertTrue(facilityIds.contains(f.getId()));
    }
  }

  @Test
  public void generateTimelinessReportShouldGetReportWithSortedFacilities()
      throws JasperReportViewException {
    //given
    reportParams.put(PROGRAM, program.getId().toString());
    reportParams.put(PERIOD, period.getId().toString());

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);

    UUID zone1Id = UUID.randomUUID();
    UUID zone2Id = UUID.randomUUID();

    FacilityDto fac1A = mockFacility(true, true, zone1Id, "zone1", "facilityA");
    FacilityDto fac1B = mockFacility(true, true, zone1Id, "zone1", "facilityB");
    FacilityDto fac2A = mockFacility(true, true, zone2Id, "zone2", "facilityA");
    FacilityDto fac2B = mockFacility(true, true, zone2Id, "zone2", "facilityB");

    when(facilityReferenceDataService.findAll()).thenReturn(Arrays.asList(
        fac2B, fac2A, fac1A, fac1B));

    // when
    byte[] reportData = service.generateTimelinessReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();
    List<FacilityDto> facilities = extractFacilitiesFromOutputParams(outputParams);

    // then
    assertEquals(expectedReportData, reportData);
    assertEquals(4, facilities.size());
    assertEquals(fac1A.getId(), facilities.get(0).getId());
    assertEquals(fac1B.getId(), facilities.get(1).getId());
    assertEquals(fac2A.getId(), facilities.get(2).getId());
    assertEquals(fac2B.getId(), facilities.get(3).getId());
  }

  @Test
  public void generateReportingRateReportShouldSetParams() throws Exception {
    UUID districtId = UUID.randomUUID();
    reportParams.put("Program", program.getId().toString());
    reportParams.put("Period", period.getId().toString());
    reportParams.put("GeographicZone", districtId.toString());
    reportParams.put("DueDays", "10");

    when(programReferenceDataService.findOne(program.getId())).thenReturn(program);
    when(periodReferenceDataService.findOne(period.getId())).thenReturn(period);
    when(geographicZoneReferenceDataService.findOne(districtId)).thenReturn(district);

    // FIX: Mock the builder so the service doesn't throw a NullPointerException
    ReportingRateReportDto mockRateDto = mock(ReportingRateReportDto.class);
    when(reportingRateReportDtoBuilder.build(any(), any(), any(), any())).thenReturn(mockRateDto);

    service.generateReportingRateReport(jasperTemplate, reportParams);

    ArgumentCaptor<Map<String, Object>> paramArg = ArgumentCaptor.forClass(Map.class);

    // FIX: Verify against the external reportService
    verify(reportService).generateReport(any(), paramArg.capture());

    Map<String, Object> outputParams = paramArg.getValue();

    assertEquals(DATE_FORMAT, outputParams.get("dateFormat"));
    assertEquals(createDecimalFormat(), outputParams.get("decimalFormat"));
  }

  @Test
  public void generateRequisitionReportShouldSetParams() throws Exception {
    doReturn(locale).when(service).getLocaleFromService();
    reportParams.put("Requisition", requisition.getId());

    RequisitionReportDto requisitionReportDto = DtoGenerator.of(RequisitionReportDto.class);
    when(requisitionReportDtoBuilder.build(requisition)).thenReturn(requisitionReportDto);

    Boolean showInDoses = true;
    byte[] reportData = service.generateRequisitionReport(requisition, showInDoses);
    ArgumentCaptor<Map<String,Object>> paramArg = ArgumentCaptor.forClass(Map.class);
    verify(service).fillAndExportReport(any(JasperReport.class), paramArg.capture());
    Map<String, Object> outputParams = paramArg.getValue();

    assertEquals(expectedReportData, reportData);
    assertEquals(DATE_FORMAT, outputParams.get("dateFormat"));
    assertEquals(createDecimalFormat(), outputParams.get("decimalFormat"));
    assertEquals(NumberFormat.getCurrencyInstance(locale),
        outputParams.get("currencyDecimalFormat"));
  }

  @SuppressWarnings("unchecked")
  private List<FacilityDto> extractFacilitiesFromOutputParams(Map<String, Object> outputParams) {
    return (List<FacilityDto>) outputParams.get("datasource");
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

    when(geographicZoneReferenceDataService.findOne(districtId)).thenReturn(geographicZoneDto);

    Requisition mockRequisition = mock(Requisition.class);
    Page<Requisition> requisitionPage = mock(Page.class);
    when(requisitionPage.getContent()).thenReturn(
        (isMissingRnR) ? Collections.emptyList() : singletonList(mockRequisition));

    RequisitionSearchParams params = new DefaultRequisitionSearchParams(
        facilityId, program.getId(), period.getId(), null, null, null, null,
        null, null, EnumSet.of(APPROVED, RELEASED, RELEASED_WITHOUT_ORDER));

    when(requisitionService.searchRequisitions(eq(params), any())).thenReturn(requisitionPage);

    return geographicZoneDto;
  }

  private DecimalFormat createDecimalFormat() {
    DecimalFormatSymbols decimalFormatSymbols = new DecimalFormatSymbols();
    decimalFormatSymbols.setGroupingSeparator(GROUPING_SEPARATOR.charAt(0));
    DecimalFormat decimalFormat = new DecimalFormat("", decimalFormatSymbols);
    decimalFormat.setGroupingSize(Integer.valueOf(GROUPING_SIZE));
    return decimalFormat;
  }

  private Requisition generateRequisition() {
    requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(),
        INITIATED, false);
    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setSupplyingFacilityId(facility.getId());
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(lineItem1);
    requisitionLineItems.add(lineItem2);
    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setTemplate(requisitionTemplate);
    requisition.setFacilityId(facility.getId());
    requisition.setProgramId(program.getId());
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setStatus(AUTHORIZED);
    return requisition;
  }

  // We use in the service writeObject method which has the final modifier
  // and we can't mock the method.
  private ObjectOutputStream createObjectOutputStream() throws IOException {
    return new ObjectOutputStream() {

      @Override
      protected final void writeObjectOverride(Object obj) {
        // nothing to do here
      }

      @Override
      public void flush() {
        // nothing to do here
      }

      @Override
      public void close() {
        // nothing to do here
      }
    };
  }
}
