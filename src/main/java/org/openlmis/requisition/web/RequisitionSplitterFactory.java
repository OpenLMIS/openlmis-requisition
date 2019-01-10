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

package org.openlmis.requisition.web;

import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyPartnerReferenceDataService;
import org.openlmis.requisition.service.referencedata.TogglzReferenceDataService;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class RequisitionSplitterFactory implements FactoryBean<RequisitionSplitter> {

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Autowired
  private SupplyPartnerReferenceDataService supplyPartnerReferenceDataService;

  @Autowired
  private TogglzReferenceDataService togglzReferenceDataService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private MessageService messageService;

  @Override
  public RequisitionSplitter getObject() {
    return new RequisitionSplitter(supervisoryNodeReferenceDataService,
        supplyPartnerReferenceDataService, togglzReferenceDataService,
        requisitionRepository, messageService);
  }

  @Override
  public Class<?> getObjectType() {
    return RequisitionSplitter.class;
  }

  @Override
  public boolean isSingleton() {
    return false;
  }
}
