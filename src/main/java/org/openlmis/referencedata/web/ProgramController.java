package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.dto.ProgramDto;
import org.openlmis.referencedata.repository.ProgramRepository;
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
public class ProgramController {

  private Logger logger = LoggerFactory.getLogger(ProgramController.class);

  @Autowired
  private ProgramRepository programRepository;

  /**
   * Updating Program code and name.
   */
  @RequestMapping(value = "/programs/update", method = RequestMethod.POST)
  public ResponseEntity<?> updateProgram(@RequestBody ProgramDto programDto) {
    if (programDto == null || programDto.getId() == null) {
      logger.debug("Update failed - program id not specified");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    Program program = programRepository.findOne(programDto.getId());
    if (program == null) {
      logger.debug("Update failed - program with id: {} not found", programDto.getId());
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    program.setCode(programDto.getCode());
    program.setName(programDto.getName());

    program = programRepository.save(program);

    return new ResponseEntity<>(program, HttpStatus.OK);
  }
}
