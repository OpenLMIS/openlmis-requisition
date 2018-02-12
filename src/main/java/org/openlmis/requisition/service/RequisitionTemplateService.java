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

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class RequisitionTemplateService {

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  /**
   * Method returns Requisition templates with matched parameters.
   *
   * @param program Program of searched requisition template.
   * @return RequisitionTemplate with matched parameters.
   */
  public RequisitionTemplate getTemplateForProgram(UUID program) {
    return requisitionTemplateRepository.getTemplateForProgram(program);
  }

  /**
   * Method saves given requisition template. When an update occurs,
   * and the template is already referred by a requisition, a new template is created.
   *
   * @param template Template to be saved.
   * @return Saved template.
   */
  public RequisitionTemplate save(final RequisitionTemplate template) {
    List<Requisition> requisitions = requisitionRepository.findByTemplateId(template.getId());
    RequisitionTemplate newTemplate = template;

    if (!requisitions.isEmpty()) {
      template.archive();
      requisitionTemplateRepository.saveAndFlush(template);

      newTemplate = new RequisitionTemplate();
      newTemplate.updateFrom(template);
    }

    return requisitionTemplateRepository.save(newTemplate);
  }

  /**
   * Safe delete of a requisition template. If the requisition template is used by any
   * requisition, a {@link ValidationMessageException} signals that it cannot be removed.
   *
   * @param template the template to remove
   */
  public void delete(RequisitionTemplate template) {
    if (!requisitionRepository.findByTemplateId(template.getId()).isEmpty()) {
      throw new ValidationMessageException(new Message(
          MessageKeys.ERROR_REQUISITION_TEMPLATE_IN_USE,
          template.getId()));
    } else {
      requisitionTemplateRepository.delete(template);
    }
  }
}
