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

package org.openlmis.utils;

import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.repository.JasperTemplateRepository;
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
  private JasperTemplateRepository jasperTemplateRepository;

  /**
   * Delete all entities from most of repositories.
   */
  @Transactional
  public void cleanAll() {
    configurationSettingRepository.deleteAll();
    requisitionRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    jasperTemplateRepository.deleteAll();
  }
}
