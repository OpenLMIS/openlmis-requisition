package org.openlmis.reporting.service;

import static java.io.File.createTempFile;
import static org.apache.commons.io.FileUtils.writeByteArrayToFile;
import static org.apache.commons.lang3.StringUtils.isBlank;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRParameter;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;
import org.apache.log4j.Logger;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.model.TemplateParameter;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
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

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

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
    for (TemplateParameter parameter : template.getTemplateParameters()) {
      parameter.setTemplate(template);
      parameter = templateParameterRepository.save(parameter);
    }
  }

  /**
   * Convert template from ".jasper" format in database to ".jrxml"(extension) format.
   */
  public File convertJasperToXml(Template template) throws IOException, JRException {
    InputStream inputStream = new ByteArrayInputStream(template.getData());
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    JasperCompileManager.writeReportToXmlStream(inputStream, outputStream);
    File xmlReport =  createTempFile(template.getName(), ".jrxml");
    writeByteArrayToFile(xmlReport, outputStream.toByteArray());
    return xmlReport;
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

        ArrayList<TemplateParameter> parameters = new ArrayList<>();
        for (JRParameter jrParameter : jrParameters) {
          if (!jrParameter.isSystemDefined()) {
            parameters.add(createParameter(jrParameter));
          }
        }
        template.setTemplateParameters(parameters);
      }
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);
      out.writeObject(report);
      template.setData(bos.toByteArray());
    } catch (JRException ex) {
      throw new ReportingException("Report template error: file invalid.", ex);
    } catch (IOException ex) {
      throw new ReportingException("Report template error: reading file.", ex);
    }
  }

  /**
   * Create new report parameter of report which is not defined in Jasper system.
   */
  private TemplateParameter createParameter(JRParameter jrParameter) throws ReportingException {
    String[] propertyNames = jrParameter.getPropertiesMap().getPropertyNames();
    //Check # of properties and that required ones are given.
    if (propertyNames.length > 2) {
      throw new ReportingException("Report template error: extra properties.");
    }
    String displayName = jrParameter.getPropertiesMap().getProperty("displayName");
    if (isBlank(displayName)) {
      throw new ReportingException("Report template error: parameter displayName missing.");
    }
    //Look for sql for select and that data type is supported string.
    String dataType = jrParameter.getValueClassName();
    String selectSql = jrParameter.getPropertiesMap().getProperty("selectSql");
    //Sql selects need String data type.
    if (isBlank(selectSql) == false && dataType.equals("java.lang.String") == false) {
      throw new ReportingException("Report template error: parameter sql data type not string.");
    }
    //Set parameters.
    TemplateParameter templateParameter = new TemplateParameter();
    templateParameter.setName(jrParameter.getName());
    templateParameter.setDisplayName(displayName);
    templateParameter.setDescription(jrParameter.getDescription());
    templateParameter.setDataType(dataType);
    if (isBlank(selectSql) == false) {
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
      throw new ReportingException("Report template error: name of template already exists.");
    }
  }

  private void throwIfFileIsEmpty(MultipartFile file) throws ReportingException {
    if (file.isEmpty()) {
      throw new ReportingException("Report template error: empty file.");
    }
  }

  private void throwIfIncorrectFileType(MultipartFile file) throws ReportingException {
    if (!file.getOriginalFilename().endsWith(".jrxml")) {
      throw new ReportingException("Report template error: type of file.");
    }
  }

  private void throwIfFileIsNull(MultipartFile file) throws ReportingException {
    if (file == null) {
      throw new ReportingException("Report template error: missing file.");
    }
  }
}
