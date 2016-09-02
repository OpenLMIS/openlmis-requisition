package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@Controller
public class ProgramController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProgramController.class);

  @Autowired
  private ProgramRepository programRepository;

  /**
   * Allows creating new programs.
   * If the id is specified, it will be ignored.
   *
   * @param program A program bound to the request body
   * @return ResponseEntity containing the created program
   */
  @RequestMapping(value = "/programs", method = RequestMethod.POST)
  public ResponseEntity<?> createProgram(@RequestBody Program program) {
    try {
      LOGGER.debug("Creating new program");
      program.setId(null);
      Program newProgram = programRepository.save(program);
      LOGGER.debug("Created new program with id: " + program.getId());
      return new ResponseEntity<Program>(newProgram, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating program", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all programs.
   *
   * @return Programs.
   */
  @RequestMapping(value = "/programs", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllPrograms() {
    Iterable<Program> programs = programRepository.findAll();
    return new ResponseEntity<>(programs, HttpStatus.OK);
  }

  /**
   * Get chosen program.
   *
   * @param programId UUID of program which we want to get
   * @return Program.
   */
  @RequestMapping(value = "/programs/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProgram(@PathVariable("id") UUID programId) {
    Program program = programRepository.findOne(programId);
    if (program == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(program, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting program.
   *
   * @param programId UUID of program which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/programs/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProgram(@PathVariable("id") UUID programId) {
    Program program = programRepository.findOne(programId);
    if (program == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        programRepository.delete(program);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting program with id: "
                    + programId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Program>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Allows updating programs.
   *
   * @param program A program bound to the request body
   * @param programId UUID of program which we want to update
   * @return ResponseEntity containing the updated program
   */
  @RequestMapping(value = "/programs/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProgram(@RequestBody Program program,
                                                    @PathVariable("id") UUID programId) {
    try {
      LOGGER.debug("Updating program with id: " + programId);

      Program programToUpdate = programRepository.findOne(programId);

      if (programToUpdate == null) {
        programToUpdate = new Program();
      }

      programToUpdate.updateFrom(program);
      programToUpdate = programRepository.save(programToUpdate);

      LOGGER.debug("Updated program with id: " + programId);
      return new ResponseEntity<Program>(programToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating program with id: "
                  + programId, ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }
}
