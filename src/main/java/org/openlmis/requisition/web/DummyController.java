package org.openlmis.requisition.web;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * DummyController just for showcase.
 */
@Controller
public class DummyController extends BaseController {

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  /**
   * Test endpoint.
   *
   * @return retrieved users
   */
  @RequestMapping(value = "/refdatatest", method = RequestMethod.GET)
  public ResponseEntity<?> getAllUsers() {
    List<UserDto> users = userReferenceDataService.findAll();
    List<ProgramDto> programs = programReferenceDataService.findAll();
    List<FacilityDto> facilities = facilityReferenceDataService.findAll();
    List<ProcessingPeriodDto> periods = periodReferenceDataService.findAll();

    Map<String, List<?>> response = new HashMap<>();
    response.put("Users", users);
    response.put("Programs", programs);
    response.put("Facilities", facilities);
    response.put("Periods", periods);

    return new ResponseEntity<>(response, HttpStatus.OK);
  }
}