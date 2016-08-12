package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.model.TemplateParameter;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class TemplateParameterRepositoryIntegrationTest {

  @Autowired
  private TemplateRepository templateRepository;

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

  private Template template = new Template();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    template.setName("TemplateRepositoryIntegrationTest");
    templateRepository.save(template);
  }

  @Test
  public void testCreate() {
    TemplateParameter templateParameter = new TemplateParameter();
    templateParameter.setTemplate(template);
    Assert.assertNull(templateParameter.getId());
    templateParameter = templateParameterRepository.save(templateParameter);
    Assert.assertNotNull(templateParameter.getId());
  }
}
