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

import static org.openlmis.requisition.dto.TimelinessReportFacilityDto.DISTRICT_LEVEL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CLASS_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IO;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_JASPER_FILE_FORMAT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_TEMPLATE_PARAMETER_INVALID;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.sql.DataSource;
import net.sf.jasperreports.engine.JRBand;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperExportManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.design.JasperDesign;
import net.sf.jasperreports.engine.xml.JRXmlLoader;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReportingRateReportDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.TimelinessReportFacilityDto;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.custom.DefaultRequisitionSearchParams;
import org.openlmis.requisition.repository.custom.RequisitionSearchParams;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.GeographicZoneReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.ReportUtils;
import org.openlmis.requisition.web.ReportingRateReportDtoBuilder;
import org.openlmis.requisition.web.RequisitionReportDtoBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

@Service
@SuppressWarnings({"PMD.TooManyMethods"})
public class JasperReportsViewService {
  private static final String DATASOURCE = "datasource";
  private static final String REQUISITION_REPORT_DIR = "/jasperTemplates/requisition.jrxml";
  private static final String REQUISITION_LINE_REPORT_DIR =
      "/jasperTemplates/requisitionLines.jrxml";

  @Autowired
  private DataSource replicationDataSource;

  @Autowired
  private RequisitionReportDtoBuilder requisitionReportDtoBuilder;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private GeographicZoneReferenceDataService geographicZoneReferenceDataService;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private ReportingRateReportDtoBuilder reportingRateReportDtoBuilder;

  @Value("${dateFormat}")
  private String dateFormat;

  @Value("${groupingSeparator}")
  private String groupingSeparator;

  @Value("${groupingSize}")
  private String groupingSize;

  @Value("${defaultLocale}")
  private String defaultLocale;

  @Value("${currencyLocale}")
  private String currencyLocale;

  /**
   * Generate Jasper Report.
   *
   * @param jasperTemplate template that will be used to generate the report
   * @param params report parameters
   * @return generated report.
   * @throws JasperReportViewException if there will be any problem with generating the report.
   */
  public byte[] generateReport(JasperTemplate jasperTemplate, Map<String, Object> params)
      throws JasperReportViewException {
    return fillAndExportReport(getReportFromTemplateData(jasperTemplate), params);
  }

  /**
   * Generate Jasper Reporting Rate Report.
   *
   * @param jasperTemplate template that will be used to generate the report
   * @param params report parameters
   * @return generated report.
   * @throws JasperReportViewException if there will be any problem with generating the report.
   */
  public byte[] generateReportingRateReport(JasperTemplate jasperTemplate,
      Map<String, Object> params)
      throws JasperReportViewException {

    UUID programId = (UUID) processParameter(params, "Program", true, UUID.class);
    ProgramDto program = programReferenceDataService.findOne(programId);

    UUID periodId = (UUID) processParameter(params, "Period", true, UUID.class);
    ProcessingPeriodDto period = periodReferenceDataService.findOne(periodId);

    UUID zoneId = (UUID) processParameter(params, "GeographicZone", false, UUID.class);
    GeographicZoneDto zone = null;
    if (zoneId != null) {
      zone = geographicZoneReferenceDataService.findOne(zoneId);
    }

    Integer dueDays = (Integer) processParameter(params, "DueDays", false, Integer.class);
    if (dueDays != null && dueDays < 0) {
      throw new ValidationMessageException(
          new Message(ERROR_REPORTING_TEMPLATE_PARAMETER_INVALID, "DueDays"));
    }

    ReportingRateReportDto reportDto = reportingRateReportDtoBuilder.build(program, period, zone,
        dueDays);
    params.put(DATASOURCE, Collections.singletonList(reportDto));
    params.put("dateFormat", dateFormat);
    params.put("decimalFormat", createDecimalFormat());

    return fillAndExportReport(getReportFromTemplateData(jasperTemplate), params);
  }

  /**
   * Generate Jasper Report for printing a requisition.
   *
   * @param requisition requisition for printing the report.
   * @return generated report.
   * @throws JasperReportViewException if there will be any problem with generating the report.
   */
  public byte[] generateRequisitionReport(Requisition requisition)
      throws JasperReportViewException {
    RequisitionReportDto reportDto = requisitionReportDtoBuilder.build(requisition);
    RequisitionTemplate template = requisition.getTemplate();

    Map<String, Object> params = ReportUtils.createParametersMap();
    params.put("subreport", createCustomizedRequisitionLineSubreport(template,
        requisition.getStatus()));
    params.put(DATASOURCE, Collections.singletonList(reportDto));
    params.put("template", template);
    params.put("dateFormat", dateFormat);
    params.put("decimalFormat", createDecimalFormat());
    params.put("currencyDecimalFormat",
        NumberFormat.getCurrencyInstance(getLocaleFromService()));

    return fillAndExportReport(compileReportFromTemplateUrl(REQUISITION_REPORT_DIR), params);
  }

  /**
   * Generate Jasper Reporting Rate Report.
   *
   * @param jasperTemplate template that will be used to generate the report
   * @param parameters report parameters
   * @return generated report.
   * @throws JasperReportViewException if there will be any problem with generating the report.
   */
  public byte[] generateTimelinessReport(JasperTemplate jasperTemplate,
      Map<String, Object> parameters)
      throws JasperReportViewException {
    ProgramDto program = programReferenceDataService.findOne(
        UUID.fromString(parameters.get("program").toString())
    );
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        UUID.fromString(parameters.get("period").toString())
    );
    GeographicZoneDto district = null;
    Object districtId = parameters.get("district");
    if (districtId != null && !districtId.toString().isEmpty()) {
      district = geographicZoneReferenceDataService.findOne(
          UUID.fromString(districtId.toString()));
    }
    List<FacilityDto> facilities = getFacilitiesForTimelinessReport(program, period, district);

    parameters.put(DATASOURCE, facilities);
    parameters.put("program", program);
    parameters.put("period", period);
    parameters.put("district", district);

    return fillAndExportReport(getReportFromTemplateData(jasperTemplate), parameters);
  }

  private JasperDesign createCustomizedRequisitionLineSubreport(RequisitionTemplate template,
                                                                RequisitionStatus requisitionStatus)
      throws JasperReportViewException {
    try (InputStream inputStream = getClass().getResourceAsStream(REQUISITION_LINE_REPORT_DIR)) {
      JasperDesign design = JRXmlLoader.load(inputStream);
      JRBand detail = design.getDetailSection().getBands()[0];
      JRBand header = design.getColumnHeader();

      Map<String, RequisitionTemplateColumn> columns =
          ReportUtils.getSortedTemplateColumnsForPrint(template.viewColumns(), requisitionStatus);

      ReportUtils.customizeBandWithTemplateFields(detail, columns, design.getPageWidth(), 9);
      ReportUtils.customizeBandWithTemplateFields(header, columns, design.getPageWidth(), 9);

      return design;
    } catch (IOException err) {
      throw new JasperReportViewException(err, ERROR_IO, err.getMessage());
    } catch (JRException err) {
      throw new JasperReportViewException(err, ERROR_JASPER_FILE_FORMAT, err.getMessage());
    }
  }

  /**
   * Fill in and export a compiled report.
   */
  byte[] fillAndExportReport(JasperReport compiledReport, Map<String, Object> params)
      throws JasperReportViewException {

    byte[] bytes;

    try {
      JasperPrint jasperPrint;
      if (params.containsKey(DATASOURCE)) {
        jasperPrint = JasperFillManager.fillReport(compiledReport, params,
            new JRBeanCollectionDataSource((List) params.get(DATASOURCE)));
      } else {
        jasperPrint = JasperFillManager.fillReport(compiledReport, params,
            replicationDataSource.getConnection());
      }

      JasperExporter exporter;
      String format = (String) params.get("format");
      if ("csv".equals(format)) {
        exporter = new JasperCsvExporter(jasperPrint);
        bytes = exporter.exportReport();
      } else if ("xls".equals(format)) {
        exporter = new JasperXlsExporter(jasperPrint);
        bytes = exporter.exportReport();
      } else if ("html".equals(format)) {
        exporter = new JasperHtmlExporter(jasperPrint);
        bytes = exporter.exportReport();
      } else {
        bytes = JasperExportManager.exportReportToPdf(jasperPrint);
      }
    } catch (Exception e) {
      throw new JasperReportViewException(e, ERROR_JASPER_FILE_FORMAT, e.getMessage());
    }

    return bytes;
  }

  /**
   * Return a compiled report from a Jasper template URL string.
   */
  private JasperReport compileReportFromTemplateUrl(String templateUrl)
      throws JasperReportViewException {
    try (InputStream inputStream = getClass().getResourceAsStream(templateUrl)) {

      return JasperCompileManager.compileReport(inputStream);
    } catch (IOException ex) {
      throw new JasperReportViewException(ex, ERROR_IO, ex.getMessage());
    } catch (JRException ex) {
      throw new JasperReportViewException(ex, ERROR_JASPER_FILE_FORMAT, ex.getMessage());
    }
  }

  /**
   * Get (compiled) Jasper report from Jasper template.
   */
  private JasperReport getReportFromTemplateData(JasperTemplate jasperTemplate)
      throws JasperReportViewException {

    try (ObjectInputStream inputStream = createObjectInputStream(jasperTemplate)) {

      return readReportData(inputStream);
    } catch (IOException ex) {
      throw new JasperReportViewException(ex, ERROR_IO, ex.getMessage());
    } catch (ClassNotFoundException ex) {
      throw new JasperReportViewException(ex, ERROR_CLASS_NOT_FOUND, JasperReport.class.getName());
    }
  }

  private List<FacilityDto> getFacilitiesForTimelinessReport(
      ProgramDto program, ProcessingPeriodDto processingPeriod, GeographicZoneDto district) {
    Set<RequisitionStatus> validStatuses = Arrays.stream(RequisitionStatus.values())
        .filter(RequisitionStatus::isApproved)
        .collect(Collectors.toSet());

    List<MinimalFacilityDto> facilities = new ArrayList<>();
    if (district != null) {
      facilities.addAll(facilityReferenceDataService.search(null, null, district.getId(), true));
    } else {
      facilities.addAll(facilityReferenceDataService.findAll());
    }

    List<TimelinessReportFacilityDto> facilitiesMissingRnR = new ArrayList<>();
    // find active facilities that are missing R&R
    for (MinimalFacilityDto facility : facilities) {
      if (facility.getActive()) {
        RequisitionSearchParams params = new DefaultRequisitionSearchParams(
            facility.getId(), program.getId(), processingPeriod.getId(),
            null, null, null, null, null, null, validStatuses);

        PageRequest pageRequest = PageRequest.of(Pagination.DEFAULT_PAGE_NUMBER,
            Pagination.NO_PAGINATION);

        List<Requisition> requisitions = requisitionService
            .searchRequisitions(params, pageRequest)
            .getContent();

        if (requisitions.isEmpty()) {
          TimelinessReportFacilityDto timelinessFacility = new TimelinessReportFacilityDto();
          facility.export(timelinessFacility);
          facilitiesMissingRnR.add(timelinessFacility);
        }
      }
    }

    // sort alphabetically by district and then facility name
    Comparator<MinimalFacilityDto> comparator = Comparator.comparing(
        facility -> facility.getZoneByLevelNumber(DISTRICT_LEVEL).getName());
    comparator = comparator.thenComparing(Comparator.comparing(MinimalFacilityDto::getName));

    return facilitiesMissingRnR.stream()
        .sorted(comparator).collect(Collectors.toList());
  }

  private Object processParameter(Map<String, Object> params, String key, boolean required,
                                  Class paramType) {
    Message errorMessage = new Message(ERROR_REPORTING_TEMPLATE_PARAMETER_INVALID, key);

    try {
      if (!params.containsKey(key)) {
        if (required) {
          throw new ValidationMessageException(errorMessage);
        } else {
          return null;
        }
      }
      String paramValue = (String) params.get(key);

      if (UUID.class.equals(paramType)) {
        return UUID.fromString(paramValue);
      } else if (Integer.class.equals(paramType)) {
        return Integer.valueOf(paramValue);
      }
      return paramValue;
    } catch (ClassCastException | IllegalArgumentException err) {
      throw new ValidationMessageException(errorMessage, err);
    }
  }

  private DecimalFormat createDecimalFormat() {
    DecimalFormatSymbols decimalFormatSymbols = new DecimalFormatSymbols();
    decimalFormatSymbols.setGroupingSeparator(groupingSeparator.charAt(0));
    DecimalFormat decimalFormat = new DecimalFormat("", decimalFormatSymbols);
    decimalFormat.setGroupingSize(Integer.valueOf(groupingSize));
    return decimalFormat;
  }

  // These methods are here so that the tests are easier to write.
  
  protected ObjectInputStream createObjectInputStream(JasperTemplate jasperTemplate)
      throws IOException {
    return new ObjectInputStream(new ByteArrayInputStream(jasperTemplate.getData()));
  }

  protected JasperReport readReportData(ObjectInputStream objectInputStream)
      throws ClassNotFoundException, IOException {
    return (JasperReport) objectInputStream.readObject();
  }

  protected Locale getLocaleFromService() {
    return new Locale(defaultLocale, currencyLocale);
  }
}
