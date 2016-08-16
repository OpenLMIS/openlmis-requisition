package org.openlmis.referencedata.repository.custom;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;

import java.util.List;

public interface ProgramProductRepositoryCustom  {

  List<ProgramProduct> searchProgramProducts(Program program, Boolean fullSupply);
}
