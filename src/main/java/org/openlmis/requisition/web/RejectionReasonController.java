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

import static java.util.stream.Collectors.toSet;

import com.google.common.collect.Sets;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import lombok.NoArgsConstructor;
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.dto.RejectionReasonDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RejectionReasonRepository;
import org.openlmis.requisition.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
public class RejectionReasonController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RejectionReasonController.class);

  @Autowired
  private RejectionReasonRepository rejectionReasonsRepository;

  public RejectionReasonController(RejectionReasonRepository repository) {
    this.rejectionReasonsRepository = Objects.requireNonNull(repository);
  }

  /**
   * Get all rejection reasons in the system.
   *
   * @return all rejection reasons in the system.
   */
  @RequestMapping(value = "/rejectionReasons", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Set<RejectionReasonDto> getAllRejectionReasons() {

    LOGGER.debug("Getting all rejection reasons");
    Set<RejectionReason> rejectionReasons = Sets.newHashSet(rejectionReasonsRepository.findAll());
    return rejectionReasons.stream().map(this::exportToDto).collect(toSet());
  }

  /**
   * Get chosen rejection reason.
   *
   * @param rejectionReasonId id of the rejection reason to get.
   * @return the rejection reason.
   */
  @RequestMapping(value = "/rejectionReasons/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RejectionReasonDto getRejectionReason(@PathVariable("id") UUID rejectionReasonId) {

    RejectionReason rejectionReasons =
            rejectionReasonsRepository.findById(rejectionReasonId)
                    .orElseThrow(() -> new ContentNotFoundMessageException(
            new Message(MessageKeys.ERROR_REJECTION_REASON_NOT_FOUND, rejectionReasonId)));
    return exportToDto(rejectionReasons);
  }

  /**
   * Get chosen rejection reason(s).
   *
   * @param rejectionReasonCategoryId id of the rejection reason to get.
   * @return the rejection reason.
   */
  @RequestMapping(value = "/rejectionReasonByCategory/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Set<RejectionReasonDto> getRejectionReasonByCategory(
          @PathVariable("id") UUID rejectionReasonCategoryId) {

    List<RejectionReason> rejectionReasons =
            rejectionReasonsRepository.findRejectionReasonsByCategory(rejectionReasonCategoryId);

    if (rejectionReasons.size() == 0) {
      throw new ContentNotFoundMessageException(MessageKeys.ERROR_REJECTION_REASON_NOT_FOUND);
    } else {
      return rejectionReasons.stream().map(this::exportToDto).collect(toSet());
    }
  }

  /**
   * Save a rejection reason using the provided rejection
   * reason DTO. If the reason does not exist, will create one. If it
   * does exist, will update it.
   *
   * @param rejectionReasonDto provided rejection reason DTO.
   * @return the saved rejection reasons.
   */
  @RequestMapping(value = "/rejectionReasons", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RejectionReasonDto saveRejectionReasons(
          @RequestBody RejectionReasonDto rejectionReasonDto) {

    RejectionReason rejectionReasonsToSave = RejectionReason.newRejectionReason(rejectionReasonDto);

    RejectionReason storedRejectionReason =
            rejectionReasonsRepository.findFirstByName(rejectionReasonsToSave.getName());
    if (storedRejectionReason != null) {
      LOGGER.debug("Rejection reason  found in the system, assign id");
      rejectionReasonsToSave.setId(storedRejectionReason.getId());
    }

    LOGGER.debug("Saving rejection reason");
    rejectionReasonsToSave = rejectionReasonsRepository.save(rejectionReasonsToSave);

    LOGGER.debug("Saved rejection reasons with id: " + rejectionReasonsToSave.getId());

    return exportToDto(rejectionReasonsToSave);
  }

  /**
   * Delete an existing rejection reason.
   *
   * @param id id of the rejection reasons to delete.
   */
  @RequestMapping(value = "/rejectionReasons/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRejectionReason(@PathVariable("id") UUID id) {

    rejectionReasonsRepository.findById(id)
                    .orElseThrow(() -> new ContentNotFoundMessageException(
                            new Message(MessageKeys.ERROR_REJECTION_REASON_NOT_FOUND, id)));

    LOGGER.debug("Deleting rejection reason");
    rejectionReasonsRepository.deleteById(id);
  }

  /**
   * Finds rejection reasons matching all of the provided parameters.
   */
  @RequestMapping(value = "/rejectionReasons/search", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Set<RejectionReasonDto> searchRejectionReason(
          @RequestParam(value = "name", required = false) String name,
          @RequestParam(value = "code", required = false) String code) {

    Set<RejectionReason> foundRejectionReasons =
            rejectionReasonsRepository.searchRejectionReason(name, code);

    return exportToDtos(foundRejectionReasons);

  }

  private RejectionReasonDto exportToDto(RejectionReason rejectionReasons) {
    RejectionReasonDto rejectionReasonsDto = new RejectionReasonDto();
    rejectionReasons.export(rejectionReasonsDto);
    return rejectionReasonsDto;
  }

  private Set<RejectionReasonDto> exportToDtos(Set<RejectionReason> rejectionReasons) {
    Set<RejectionReasonDto> rejectionReasonDtos = new HashSet<>();
    for (RejectionReason rejectionReason : rejectionReasons) {
      RejectionReasonDto rejectionReasonDto = new RejectionReasonDto();
      rejectionReason.export(rejectionReasonDto);
      rejectionReasonDtos.add(rejectionReasonDto);
    }
    return rejectionReasonDtos;
  }
}
