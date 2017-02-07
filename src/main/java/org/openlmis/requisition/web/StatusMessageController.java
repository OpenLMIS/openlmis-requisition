package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.StatusMessageDto;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Controller
@Transactional
public class StatusMessageController extends BaseController {

  @Autowired
  StatusMessageRepository statusMessageRepository;

  /**
   * Get all status messages for the specified requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/statusMessages", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public List<StatusMessageDto> getAllRequisitionStatusMessages(@PathVariable("id") UUID id) {
    List<StatusMessage> statusMessages = statusMessageRepository.findByRequisitionId(id);
    return exportToDtos(statusMessages);
  }

  private List<StatusMessageDto> exportToDtos(List<StatusMessage> statusMessages) {
    return statusMessages.stream()
        .map(this::exportToDto)
        .collect(Collectors.toList());
  }
  
  private StatusMessageDto exportToDto(StatusMessage statusMessage) {
    StatusMessageDto statusMessageDto = new StatusMessageDto();
    statusMessage.export(statusMessageDto);
    return statusMessageDto;
  }
}
