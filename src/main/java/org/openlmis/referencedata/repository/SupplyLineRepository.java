package org.openlmis.referencedata.repository;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface SupplyLineRepository extends PagingAndSortingRepository<SupplyLine, UUID> {

  String PROGRAM = "program";
  String SUPERVISORY_NODE = "supervisoryNode";

  SupplyLine findByProgramAndSupervisoryNode(
      @Param(PROGRAM) Program program,
      @Param(SUPERVISORY_NODE) SupervisoryNode supervisoryNode);
}
