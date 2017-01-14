package org.openlmis.utils;

import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.repository.TemplateRepository;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class CleanRepositoryHelper {

  public static final UUID INITIAL_USER_ID =
      UUID.fromString("35316636-6264-6331-2d34-3933322d3462");

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private TemplateRepository templateRepository;

  /**
   * Delete all entities from most of repositories.
   */
  @Transactional
  public void cleanAll() {
    configurationSettingRepository.deleteAll();
    requisitionRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    templateRepository.deleteAll();
  }
}
