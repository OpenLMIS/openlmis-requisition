package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.Program;
import org.springframework.data.repository.CrudRepository;

public interface ProgramRepository extends CrudRepository<Program, Integer> {
}
