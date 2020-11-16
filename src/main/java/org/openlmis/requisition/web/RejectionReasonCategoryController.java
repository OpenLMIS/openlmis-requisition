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

import com.google.common.collect.Sets;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import lombok.NoArgsConstructor;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.dto.RejectionReasonCategoryDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RejectionReasonCategoryRepository;
import org.openlmis.requisition.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;


@NoArgsConstructor
@Controller
@Transactional
public class RejectionReasonCategoryController extends BaseController {

  private static final Logger LOGGER = LoggerFactory
          .getLogger(RejectionReasonCategoryController.class);

  @Autowired
  private RejectionReasonCategoryRepository rejectionReasonCategoryRepository;

  public RejectionReasonCategoryController(RejectionReasonCategoryRepository repository) {
    this.rejectionReasonCategoryRepository = Objects.requireNonNull(repository);
  }

  /**
   * Get all rejection reason categories in the system.
   *
   * @return all rejection reason categories in the system.
   */
  @RequestMapping(value = "/rejectionReasonCategories", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<RejectionReasonCategoryDto> getAllRejectionReasonCategories(Pageable pageable) {

    LOGGER.debug("Getting all rejection reason categories");
    Set<RejectionReasonCategory> rejectionReasonCategories = Sets
            .newHashSet(rejectionReasonCategoryRepository.findAll());
    Set<RejectionReasonCategoryDto> rejectionReasonCategoryDtos =
            rejectionReasonCategories.stream().map(this::exportToDto).collect(Collectors.toSet());

    return toPage(rejectionReasonCategoryDtos, pageable);
  }

  /**
   * Get chosen rejection reason category.
   *
   * @param id id of the rejection reason to get.
   * @return the rejection reason category.
   */
  @RequestMapping(value = "/rejectionReasonCategories/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RejectionReasonCategoryDto getRejectionReasonCategory(@PathVariable("id")
                                                                       UUID id) {
    RejectionReasonCategory rejectionReasonCategory = rejectionReasonCategoryRepository
            .findById(id)
            .orElseThrow(() -> new ContentNotFoundMessageException(
                    new Message(MessageKeys.ERROR_REJECTION_REASON_CATEGORY_NOT_FOUND, id)));

    return exportToDto(rejectionReasonCategory);
  }

  /**
   * Save a rejection reason category using the provided rejection
   * reason category DTO. If the category does not exist, will create one. If it
   * does exist, will update it.
   *
   * @param rejectionReasonCategoryDto provided rejection reason category DTO.
   * @return the saved reason.
   */
  @RequestMapping(value = "/rejectionReasonCategories", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.CREATED)
  @ResponseBody
  public RejectionReasonCategoryDto saveRejectionReasonCategory(
          @RequestBody RejectionReasonCategoryDto rejectionReasonCategoryDto) {

    RejectionReasonCategory rejectionReasonCategoryToSave = RejectionReasonCategory
            .newRejectionReasonCategory(rejectionReasonCategoryDto);

    LOGGER.debug("Saving rejection reason category");
    rejectionReasonCategoryToSave = rejectionReasonCategoryRepository
            .save(rejectionReasonCategoryToSave);

    LOGGER.debug("Saved rejection reason "
            + "category with id: " + rejectionReasonCategoryToSave.getId());

    return exportToDto(rejectionReasonCategoryToSave);
  }

  /**
   * Allows updating Rejection Reason Category.
   *
   * @param rejectionReasonCategoryDto a rejectionReasonCategoryDto bound to the request body.
   * @param rejectionReasonCategoryId  the UUID of Rejection Reason Category which we want to
   *                                   update.
   * @return the updated RejectionReasonCategoryDto.
   */
  @RequestMapping(value = "/rejectionReasonCategories/{id}", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RejectionReasonCategoryDto updateRejectionReasonCategory(
          @RequestBody RejectionReasonCategoryDto rejectionReasonCategoryDto,
          @PathVariable("id") UUID rejectionReasonCategoryId) {

    RejectionReasonCategory template =
            RejectionReasonCategory.newRejectionReasonCategory(rejectionReasonCategoryDto);
    RejectionReasonCategory rejectionReasonCategoryToUpdate = rejectionReasonCategoryRepository
            .findById(rejectionReasonCategoryId)
            .orElse(null);
    RejectionReasonCategory rejectionReasonCategoryToSave;
    if (rejectionReasonCategoryToUpdate == null) {
      rejectionReasonCategoryToSave = template;
      rejectionReasonCategoryToSave.setId(rejectionReasonCategoryId);
    } else {
      rejectionReasonCategoryToSave = rejectionReasonCategoryToUpdate;
      rejectionReasonCategoryToSave.updateFrom(template);
    }

    LOGGER.debug("Updating Rejection Reason Category");
    rejectionReasonCategoryToSave = rejectionReasonCategoryRepository
            .save(rejectionReasonCategoryToSave);

    LOGGER.debug("Saved rejectionReasonCategory with id: " + rejectionReasonCategoryToSave.getId());
    return exportToDto(rejectionReasonCategoryToSave);
  }

  /**
   * Finds rejection reason categories matching all of the provided parameters.
   */
  @RequestMapping(value = "/rejectionReasonCategories/search", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Set<RejectionReasonCategoryDto> searchRejectionReasonCategory(
          @RequestParam(value = "name", required = false) String name,
          @RequestParam(value = "code", required = false) String code,
          @RequestParam(value = "active", required = false) Boolean active) {
    Set<RejectionReasonCategory> foundRejectionReasonCategory;
    if (active != null) {
      foundRejectionReasonCategory =
              rejectionReasonCategoryRepository.findByActive(active);
    } else {
      if (name == null && code == null) {
        throw new ValidationMessageException(MessageKeys.ERROR_MISSING_MANDATORY_ITEMS);
      }
      foundRejectionReasonCategory = rejectionReasonCategoryRepository
              .searchRejectionReasonCategory(name, code);
    }
    return exportToDtos(foundRejectionReasonCategory);

  }

  private RejectionReasonCategoryDto exportToDto(RejectionReasonCategory rejectionReasonCategory) {
    RejectionReasonCategoryDto rejectionReasonCategoryDto = new RejectionReasonCategoryDto();
    rejectionReasonCategory.export(rejectionReasonCategoryDto);
    return rejectionReasonCategoryDto;
  }

  private Set<RejectionReasonCategoryDto> exportToDtos(
          Set<RejectionReasonCategory> rejectionReasonCategories) {
    Set<RejectionReasonCategoryDto> rejectionReasonCategoryDtos = new HashSet<>();
    for (RejectionReasonCategory rejectionReasonCategory : rejectionReasonCategories) {
      RejectionReasonCategoryDto rejectionReasonCategoryDto = new RejectionReasonCategoryDto();
      rejectionReasonCategory.export(rejectionReasonCategoryDto);
      rejectionReasonCategoryDtos.add(rejectionReasonCategoryDto);
    }
    return rejectionReasonCategoryDtos;
  }
}
