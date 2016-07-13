package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface ProgramProductRepository extends PagingAndSortingRepository<ProgramProduct, UUID> {

  String FULL_SUPPLY = "fullSupply";
  String PROGRAM = "program";

  Iterable<ProgramProduct> findByProgram(
          @Param(PROGRAM) Program program);

  Iterable<ProgramProduct> findByProgramAndFullSupply(
          @Param(PROGRAM) Program program,
          @Param(FULL_SUPPLY) boolean fullSupply);
}
