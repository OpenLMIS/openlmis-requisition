package org.openlmis.requisition.repository;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.openlmis.requisition.domain.Template;
import org.springframework.beans.factory.annotation.Autowired;

public class TemplateRepositoryIntegrationTest extends
    BaseCrudRepositoryIntegrationTest<Template> {

  private static final String NAME = "TemplateRepositoryIntegrationTest";

  @Autowired
  private TemplateRepository templateRepository;

  @Override
  TemplateRepository getRepository() {
    return this.templateRepository;
  }

  @Override
  protected Template generateInstance() {
    Template template = new Template();
    template.setName(NAME);
    return template;
  }

  @Test
  public void shouldFindTemplateByName() {
    templateRepository.save(generateInstance());

    Template found = templateRepository.findByName(NAME);

    assertThat(found.getName(), is(NAME));
  }
}
