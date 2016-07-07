package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface ProgramRepository extends ReferenceDataRepository<Program, UUID> {
    //Add custom Program related members here. See UserRepository.java for examples.

  @Override
  @RestResource
  <S extends Program> S save (S entity);

  @Override
  @RestResource
  <S extends Program> Iterable<S> save (Iterable<S> entities);
}
