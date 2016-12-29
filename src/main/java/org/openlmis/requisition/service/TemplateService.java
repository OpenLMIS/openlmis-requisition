package org.openlmis.requisition.service;

import static java.io.File.createTempFile;
import static org.apache.commons.io.FileUtils.writeByteArrayToFile;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IO;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_CREATION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_EXTRA_PROPERTIES;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_EMPTY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_INCORRECT_TYPE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_INVALID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_MISSING;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_PARAMETER_INCORRECT_TYPE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_PARAMETER_MISSING;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_TEMPLATE_EXIST;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRParameter;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;

import org.apache.log4j.Logger;
import org.openlmis.requisition.domain.Template;
import org.openlmis.requisition.domain.TemplateParameter;
import org.openlmis.requisition.exception.ReportingException;
import org.openlmis.requisition.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

@Service
public class TemplateService {

  private static final Logger LOGGER = Logger.getLogger(TemplateService.class);

  @Autowired
  private TemplateRepository templateRepository;

  public Template getByName(String name) {
    return templateRepository.findByName(name);
  }

  /**
   * Validate ".jrmxl" file and insert this template to database.
   */
  public void validateFileAndInsertTemplate(Template template, MultipartFile file)
      throws ReportingException {
    throwIfTemplateWithSameNameAlreadyExists(template.getName());
    validateFile(template, file);
    saveWithParameters(template);
  }

  /**
   * Validate ".jrmxl" file and insert if template not exist.
   * If this name of template already exist, remove older template and insert new.
   */
  public void validateFileAndSaveTemplate(Template template, MultipartFile file)
      throws ReportingException {
    Template templateTmp = templateRepository.findByName(template.getName());
    if (templateTmp != null) {
      templateRepository.delete(templateTmp.getId());
    }
    validateFile(template, file);
    saveWithParameters(template);
  }

  /**
   * Insert template and template parameters to database.
   */
  public void saveWithParameters(Template template) {
    templateRepository.save(template);
  }

  /**
   * Convert template from ".jasper" format in database to ".jrxml"(extension) format.
   */
  public File convertJasperToXml(Template template) throws ReportingException {
    try (InputStream inputStream = new ByteArrayInputStream(template.getData());
         ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      JasperCompileManager.writeReportToXmlStream(inputStream, outputStream);
      File xmlReport = createTempFile(template.getName(), ".jrxml");
      writeByteArrayToFile(xmlReport, outputStream.toByteArray());
      return xmlReport;
    } catch (JRException | IOException ex) {
      throw new ReportingException(ex, ERROR_REPORTING_CREATION);
    }
  }

  /**
   * Validate ".jrxml" report file with JasperCompileManager.
   * If report is valid create additional report parameters.
   * Save additional report parameters as TemplateParameter list.
   * Save report file as ".jasper" in byte array in Template class.
   * If report is not valid throw exception.
   */
  private void validateFile(Template template, MultipartFile file) throws ReportingException {
    throwIfFileIsNull(file);
    throwIfIncorrectFileType(file);
    throwIfFileIsEmpty(file);

    try {
      JasperReport report = JasperCompileManager.compileReport(file.getInputStream());
      JRParameter[] jrParameters = report.getParameters();

      if (jrParameters != null && jrParameters.length > 0) {
        setTemplateParameters(template, jrParameters);
      }

      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);
      out.writeObject(report);
      template.setData(bos.toByteArray());
    } catch (JRException ex) {
      throw new ReportingException(ex, ERROR_REPORTING_FILE_INVALID);
    } catch (IOException ex) {
      throw new ReportingException(ex, ERROR_IO, ex.getMessage());
    }
  }

  private void setTemplateParameters(Template template, JRParameter[] jrParameters)
      throws ReportingException {
    ArrayList<TemplateParameter> parameters = new ArrayList<>();

    for (JRParameter jrParameter : jrParameters) {
      if (!jrParameter.isSystemDefined()) {
        parameters.add(createParameter(jrParameter));
      }
    }

    template.setTemplateParameters(parameters);
  }

  /**
   * Create new report parameter of report which is not defined in Jasper system.
   */
  private TemplateParameter createParameter(JRParameter jrParameter) throws ReportingException {
    String[] propertyNames = jrParameter.getPropertiesMap().getPropertyNames();
    //Check # of properties and that required ones are given.
    if (propertyNames.length > 2) {
      throw new ReportingException(ERROR_REPORTING_EXTRA_PROPERTIES);
    }
    String displayName = jrParameter.getPropertiesMap().getProperty("displayName");
    if (isBlank(displayName)) {
      throw new ReportingException(
          ERROR_REPORTING_PARAMETER_MISSING, "displayName");
    }
    //Look for sql for select and that data type is supported string.
    String dataType = jrParameter.getValueClassName();
    String selectSql = jrParameter.getPropertiesMap().getProperty("selectSql");
    //Sql selects need String data type.
    if (isNotBlank(selectSql) && !"java.lang.String".equals(dataType)) {
      throw new ReportingException(
          ERROR_REPORTING_PARAMETER_INCORRECT_TYPE, "sql", "string");
    }
    //Set parameters.
    TemplateParameter templateParameter = new TemplateParameter();
    templateParameter.setName(jrParameter.getName());
    templateParameter.setDisplayName(displayName);
    templateParameter.setDescription(jrParameter.getDescription());
    templateParameter.setDataType(dataType);
    if (isNotBlank(selectSql)) {
      LOGGER.debug("SQL from report parameter: " + selectSql);
      templateParameter.setSelectSql(selectSql);
    }
    if (jrParameter.getDefaultValueExpression() != null) {
      templateParameter.setDefaultValue(jrParameter.getDefaultValueExpression()
          .getText().replace("\"", "").replace("\'", ""));
    }
    return templateParameter;
  }

  private void throwIfTemplateWithSameNameAlreadyExists(String name) throws ReportingException {
    if (templateRepository.findByName(name) != null) {
      throw new ReportingException(ERROR_REPORTING_TEMPLATE_EXIST);
    }
  }

  private void throwIfFileIsEmpty(MultipartFile file) throws ReportingException {
    if (file.isEmpty()) {
      throw new ReportingException(ERROR_REPORTING_FILE_EMPTY);
    }
  }

  private void throwIfIncorrectFileType(MultipartFile file) throws ReportingException {
    if (!file.getOriginalFilename().endsWith(".jrxml")) {
      throw new ReportingException(ERROR_REPORTING_FILE_INCORRECT_TYPE);
    }
  }

  private void throwIfFileIsNull(MultipartFile file) throws ReportingException {
    if (file == null) {
      throw new ReportingException(ERROR_REPORTING_FILE_MISSING);
    }
  }
}
