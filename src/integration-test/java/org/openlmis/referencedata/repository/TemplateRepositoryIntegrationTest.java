package org.openlmis.referencedata.repository;

import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;

public class TemplateRepositoryIntegrationTest extends
    BaseCrudRepositoryIntegrationTest<Template> {

  @Autowired
  private TemplateRepository templateRepository;

  TemplateRepository getRepository() {
    return this.templateRepository;
  }

  @Override
  Template generateInstance() {
    Template template = new Template();
    template.setName("TemplateRepositoryIntegrationTest");
    return template;
  }
}
