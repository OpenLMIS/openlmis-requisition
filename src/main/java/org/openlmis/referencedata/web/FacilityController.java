package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@RepositoryRestController
public class FacilityController  {
  private Logger logger = LoggerFactory.getLogger(FacilityController.class);

  @Autowired
  private FacilityRepository facilityRepository;

  @RequestMapping(value = "/facilities", method = RequestMethod.POST)
  public ResponseEntity<?> createFacility(@RequestBody Facility facility) {
    if (facility == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Creating new facility");
      // ignore provided id, create new facility instead
      facility.setId(null);
      Facility newFacility = facilityRepository.save(facility);
      return new ResponseEntity<Facility>(newFacility, HttpStatus.CREATED);
    }
  }
}
