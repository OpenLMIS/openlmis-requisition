package org.openlmis.referencedata.service;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ProgramProductService {

  @Autowired
  private ProgramProductRepository programProductRepository;

  /**
   * Finds ProgramProducts matching all of provided parameters.
   * @param program program of searched ProgramProducts.
   * @param fullSupply are the looking programProducts fullSupply.
   * @return list of all ProgramProducts matching all of provided parameters.
   */
  public List<ProgramProduct> searchProgramProducts(Program program, Boolean fullSupply) {
    return programProductRepository.searchProgramProducts(program,fullSupply);
  }
}
