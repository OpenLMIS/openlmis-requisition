package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.Repository;

public interface ProgramRepository extends Repository<Program, Integer>
{
    Iterable<Program> findAll(Sort sort);
    Page<Program> findAll(Pageable pageable);

    //See UserRepository for examples of how we might implement additional endpoints after we know what they should be.
}
