package org.openlmis.requisition.service;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.arrayContaining;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_EXTRA_PROPERTIES;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_EMPTY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_INCORRECT_TYPE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_INVALID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_FILE_MISSING;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_PARAMETER_MISSING;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REPORTING_TEMPLATE_EXIST;
import static org.powermock.api.mockito.PowerMockito.doNothing;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.whenNew;

import net.sf.jasperreports.engine.JRExpression;
import net.sf.jasperreports.engine.JRParameter;
import net.sf.jasperreports.engine.JRPropertiesMap;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.exception.ReportingException;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.ObjectOutputStream;

@RunWith(PowerMockRunner.class)
@PowerMockRunnerDelegate(BlockJUnit4ClassRunner.class)
@PrepareForTest({JasperTemplateService.class, JasperCompileManager.class})
public class JasperTemplateServiceTest {

  @Mock
  private JasperTemplateRepository jasperTemplateRepository;

  @InjectMocks
  private JasperTemplateService jasperTemplateService;

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  private static final String NAME_OF_FILE = "report.jrxml";
  private static final String DISPLAY_NAME = "displayName";
  private static final String PARAM_DISPLAY_NAME = "Param Display Name";

  @Test
  public void shouldThrowErrorIfFileNotOfTypeJasperXml() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_FILE_INCORRECT_TYPE);

    MockMultipartFile file = new MockMultipartFile("report.pdf", new byte[1]);
    jasperTemplateService.validateFileAndInsertTemplate(new JasperTemplate(), file);
  }

  @Test
  public void shouldThrowErrorIfFileEmpty() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_FILE_EMPTY);
    MockMultipartFile file = new MockMultipartFile(
        NAME_OF_FILE, NAME_OF_FILE, "", new byte[0]);

    jasperTemplateService.validateFileAndInsertTemplate(new JasperTemplate(), file);
  }

  @Test
  public void shouldThrowErrorIfFileNotPresent() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_FILE_MISSING);

    jasperTemplateService.validateFileAndInsertTemplate(new JasperTemplate(), null);
  }

  @Test
  public void shouldThrowErrorIfFileIsInvalid() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_FILE_INVALID);

    jasperTemplateService.validateFileAndInsertTemplate(new JasperTemplate(),
        new MockMultipartFile(NAME_OF_FILE, NAME_OF_FILE, "", new byte[1]));
  }

  @Test
  public void shouldThrowErrorIfTemplateNameAlreadyExists() throws Exception {
    JasperTemplate jasperTemplate = new JasperTemplate();
    jasperTemplate.setName("Name");
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_TEMPLATE_EXIST);
    when(jasperTemplateRepository.findByName(Matchers.anyObject())).thenReturn(jasperTemplate);

    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, null);
  }

  @Test
  public void shouldThrowErrorIfDisplayNameOfParameterIsMissing() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expect(hasProperty("params", arrayContaining("displayName")));
    expectedException.expectMessage(ERROR_REPORTING_PARAMETER_MISSING);

    MultipartFile file = mock(MultipartFile.class);
    when(file.getOriginalFilename()).thenReturn(NAME_OF_FILE);

    mockStatic(JasperCompileManager.class);
    JasperReport report = mock(JasperReport.class);
    InputStream inputStream = mock(InputStream.class);
    when(file.getInputStream()).thenReturn(inputStream);

    JRParameter param1 = mock(JRParameter.class);
    JRParameter param2 = mock(JRParameter.class);
    JRPropertiesMap propertiesMap = mock(JRPropertiesMap.class);

    when(report.getParameters()).thenReturn(new JRParameter[]{param1, param2});
    when(JasperCompileManager.compileReport(inputStream)).thenReturn(report);
    when(param1.getPropertiesMap()).thenReturn(propertiesMap);
    String[] propertyNames = {"name1"};
    when(propertiesMap.getPropertyNames()).thenReturn(propertyNames);
    when(propertiesMap.getProperty(DISPLAY_NAME)).thenReturn(null);
    JasperTemplate jasperTemplate = new JasperTemplate();

    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, file);

    verify(jasperTemplateService, never()).saveWithParameters(jasperTemplate);
  }

  @Test
  public void shouldThrowErrorIfThereAreExtraParameterProperties() throws Exception {
    expectedException.expect(ReportingException.class);
    expectedException.expectMessage(ERROR_REPORTING_EXTRA_PROPERTIES);
    MultipartFile file = mock(MultipartFile.class);
    when(file.getOriginalFilename()).thenReturn(NAME_OF_FILE);

    mockStatic(JasperCompileManager.class);
    JasperReport report = mock(JasperReport.class);
    InputStream inputStream = mock(InputStream.class);
    when(file.getInputStream()).thenReturn(inputStream);

    JRParameter param1 = mock(JRParameter.class);
    JRPropertiesMap propertiesMap = mock(JRPropertiesMap.class);

    when(report.getParameters()).thenReturn(new JRParameter[]{param1});
    when(JasperCompileManager.compileReport(inputStream)).thenReturn(report);
    when(param1.getPropertiesMap()).thenReturn(propertiesMap);
    String[] propertyNames = {"name1", "name2", "name3"};
    when(propertiesMap.getPropertyNames()).thenReturn(propertyNames);
    JasperTemplate jasperTemplate = new JasperTemplate();

    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, file);

    verify(jasperTemplateService, never()).saveWithParameters(jasperTemplate);
  }

  @Test
  public void shouldValidateFileAndSetData() throws Exception {
    MultipartFile file = mock(MultipartFile.class);
    when(file.getOriginalFilename()).thenReturn(NAME_OF_FILE);

    mockStatic(JasperCompileManager.class);
    JasperReport report = mock(JasperReport.class);
    InputStream inputStream = mock(InputStream.class);
    when(file.getInputStream()).thenReturn(inputStream);

    JRParameter param1 = mock(JRParameter.class);
    JRParameter param2 = mock(JRParameter.class);
    JRPropertiesMap propertiesMap = mock(JRPropertiesMap.class);
    JRExpression jrExpression = mock(JRExpression.class);

    String[] propertyNames = {DISPLAY_NAME};
    when(report.getParameters()).thenReturn(new JRParameter[]{param1, param2});
    when(JasperCompileManager.compileReport(inputStream)).thenReturn(report);
    when(propertiesMap.getPropertyNames()).thenReturn(propertyNames);
    when(propertiesMap.getProperty(DISPLAY_NAME)).thenReturn(PARAM_DISPLAY_NAME);

    when(param1.getPropertiesMap()).thenReturn(propertiesMap);
    when(param1.getValueClassName()).thenReturn("String");
    when(param1.getName()).thenReturn("name");
    when(param1.getDescription()).thenReturn("desc");
    when(param1.getDefaultValueExpression()).thenReturn(jrExpression);
    when(jrExpression.getText()).thenReturn("text");

    when(param2.getPropertiesMap()).thenReturn(propertiesMap);
    when(param2.getValueClassName()).thenReturn("Integer");
    when(param2.getName()).thenReturn("name");
    when(param2.getDescription()).thenReturn("desc");
    when(param2.getDefaultValueExpression()).thenReturn(jrExpression);

    ByteArrayOutputStream byteOutputStream = mock(ByteArrayOutputStream.class);
    whenNew(ByteArrayOutputStream.class).withAnyArguments().thenReturn(byteOutputStream);
    ObjectOutputStream objectOutputStream = spy(new ObjectOutputStream(byteOutputStream));
    whenNew(ObjectOutputStream.class).withArguments(byteOutputStream)
        .thenReturn(objectOutputStream);
    doNothing().when(objectOutputStream).writeObject(report);
    byte[] byteData = new byte[1];
    when(byteOutputStream.toByteArray()).thenReturn(byteData);
    JasperTemplate jasperTemplate = new JasperTemplate();

    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, file);

    verify(jasperTemplateRepository).save(jasperTemplate);

    assertThat(jasperTemplate.getTemplateParameters().get(0).getDisplayName(),
        is(PARAM_DISPLAY_NAME));
    assertThat(jasperTemplate.getTemplateParameters().get(0).getDescription(), is("desc"));
    assertThat(jasperTemplate.getTemplateParameters().get(0).getName(), is("name"));
  }

  @Test
  public void shouldValidateFileAndSetDataIfDefaultValueExpressionIsNull() throws Exception {
    MultipartFile file = mock(MultipartFile.class);
    when(file.getOriginalFilename()).thenReturn(NAME_OF_FILE);

    mockStatic(JasperCompileManager.class);
    JasperReport report = mock(JasperReport.class);
    InputStream inputStream = mock(InputStream.class);
    when(file.getInputStream()).thenReturn(inputStream);

    JRParameter param1 = mock(JRParameter.class);
    JRParameter param2 = mock(JRParameter.class);
    JRPropertiesMap propertiesMap = mock(JRPropertiesMap.class);
    JRExpression jrExpression = mock(JRExpression.class);
    String[] propertyNames = {DISPLAY_NAME};

    when(report.getParameters()).thenReturn(new JRParameter[]{param1, param2});
    when(JasperCompileManager.compileReport(inputStream)).thenReturn(report);
    when(propertiesMap.getPropertyNames()).thenReturn(propertyNames);
    when(propertiesMap.getProperty(DISPLAY_NAME)).thenReturn(PARAM_DISPLAY_NAME);

    when(param1.getPropertiesMap()).thenReturn(propertiesMap);
    when(param1.getValueClassName()).thenReturn("String");
    when(param1.getDefaultValueExpression()).thenReturn(jrExpression);
    when(jrExpression.getText()).thenReturn("text");

    when(param2.getPropertiesMap()).thenReturn(propertiesMap);
    when(param2.getValueClassName()).thenReturn("Integer");
    when(param2.getDefaultValueExpression()).thenReturn(null);

    ByteArrayOutputStream byteOutputStream = mock(ByteArrayOutputStream.class);
    whenNew(ByteArrayOutputStream.class).withAnyArguments().thenReturn(byteOutputStream);
    ObjectOutputStream objectOutputStream = spy(new ObjectOutputStream(byteOutputStream));
    whenNew(ObjectOutputStream.class).withArguments(byteOutputStream)
        .thenReturn(objectOutputStream);
    doNothing().when(objectOutputStream).writeObject(report);
    byte[] byteData = new byte[1];
    when(byteOutputStream.toByteArray()).thenReturn(byteData);
    JasperTemplate jasperTemplate = new JasperTemplate();

    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, file);

    verify(jasperTemplateRepository).save(jasperTemplate);
    assertThat(jasperTemplate.getTemplateParameters().get(0).getDisplayName(),
        is(PARAM_DISPLAY_NAME));
  }
}
