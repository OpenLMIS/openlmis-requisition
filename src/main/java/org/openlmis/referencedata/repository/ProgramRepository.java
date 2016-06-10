package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public interface ProgramRepository extends CrudRepository<Program, UUID> {
}
