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

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.exception.BindingResultException;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.openlmis.requisition.validate.RequisitionTemplateValidator;
import org.openlmis.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import java.util.UUID;

@Controller
@Transactional
public class RequisitionTemplateController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionTemplateController.class);

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionTemplateValidator validator;

  @Autowired
  private PermissionService permissionService;

  /**
   * Allows creating a new Requisition Template.
   * If the id is specified, it will be ignored.
   *
   * @param requisitionTemplateDto A requisitionTemplateDto bound to the request body.
   * @param bindingResult          Object used for validation.
   * @return created requisitionTemplate.
   */
  @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.CREATED)
  @ResponseBody
  public RequisitionTemplate createRequisitionTemplate(
      @RequestBody RequisitionTemplateDto requisitionTemplateDto, BindingResult bindingResult) {
    permissionService.canManageRequisitionTemplate();

    RequisitionTemplate requisitionTemplate =
        RequisitionTemplate.newInstance(requisitionTemplateDto);

    validator.validate(requisitionTemplate, bindingResult);
    if (bindingResult.hasErrors()) {
      throw new BindingResultException(getErrors(bindingResult));
    }

    LOGGER.debug("Creating new requisitionTemplate");
    requisitionTemplate.setId(null);
    RequisitionTemplate newRequisitionTemplate =
        requisitionTemplateRepository.save(requisitionTemplate);
    LOGGER.debug("Created new requisitionTemplate with id: " + requisitionTemplate.getId());
    return newRequisitionTemplate;
  }

  /**
   * Get all requisitionTemplates.
   *
   * @return RequisitionTemplates.
   */
  @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Iterable<RequisitionTemplateDto> getAllRequisitionTemplates() {
    permissionService.canManageRequisitionTemplate();

    return RequisitionTemplateDto.newInstance(requisitionTemplateRepository.findAll());
  }

  /**
   * Allows updating requisitionTemplates.
   *
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to update
   * @param requisitionTemplateDto   A requisitionTemplateDto bound to the request body
   * @param bindingResult         Object used for validation.
   * @return updated requisitionTemplate.
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionTemplate updateRequisitionTemplate(
      @PathVariable("id") UUID requisitionTemplateId,
      @RequestBody RequisitionTemplateDto requisitionTemplateDto,
      BindingResult bindingResult) {
    permissionService.canManageRequisitionTemplate();

    RequisitionTemplate requisitionTemplate =
        RequisitionTemplate.newInstance(requisitionTemplateDto);

    validator.validate(requisitionTemplate, bindingResult);
    if (bindingResult.hasErrors()) {
      throw new BindingResultException(getErrors(bindingResult));
    }

    RequisitionTemplate requisitionTemplateToUpdate =
        requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplateToUpdate == null) {
      requisitionTemplateToUpdate = new RequisitionTemplate();
      requisitionTemplateToUpdate.updateFrom(requisitionTemplate);
      LOGGER.info("Creating new requisitionTemplate");
    } else {
      requisitionTemplate.setId(requisitionTemplateToUpdate.getId());
      requisitionTemplateToUpdate = requisitionTemplate;
      LOGGER.debug("Updating requisitionTemplate with id: " + requisitionTemplateId);
    }

    requisitionTemplateToUpdate = requisitionTemplateService.save(requisitionTemplateToUpdate);

    LOGGER.debug("Saved requisitionTemplate with id: " + requisitionTemplateToUpdate.getId());
    return requisitionTemplateToUpdate;
  }

  /**
   * Get chosen requisitionTemplate.
   *
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to get
   * @return RequisitionTemplate.
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionTemplateDto getRequisitionTemplate(
      @PathVariable("id") UUID requisitionTemplateId) {
    permissionService.canManageRequisitionTemplate();

    RequisitionTemplate requisitionTemplate =
        requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_FOUND_FOR_ID, requisitionTemplateId));
    } else {
      return RequisitionTemplateDto.newInstance(requisitionTemplate);
    }
  }

  /**
   * Allows deleting requisitionTemplate.
   *
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to delete
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisitionTemplate(@PathVariable("id") UUID requisitionTemplateId) {
    permissionService.canManageRequisitionTemplate();
    RequisitionTemplate requisitionTemplate =
        requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_FOUND_FOR_ID, requisitionTemplateId));
    } else {
      requisitionTemplateService.delete(requisitionTemplate);
    }
  }

  /**
   * Returns requisition template for the given program.
   *
   * @param program program of searched requisition templates.
   * @return RequisitionTemplate matching provided parameters.
   */
  @RequestMapping(value = "/requisitionTemplates/search", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionTemplateDto getTemplateByProgram(
      @RequestParam(value = "program", required = false) UUID program) {
    permissionService.canManageRequisitionTemplate();
    return RequisitionTemplateDto.newInstance(
        requisitionTemplateService.getTemplateForProgram(program));
  }
}
