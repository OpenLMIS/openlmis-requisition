package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

@RepositoryRestResource(exported = true)
public interface UserRepository extends ReferenceDataRepository<User, UUID> {
  //Accessible via http://127.0.0.1:8080/api/users/search/findByUsername?username={username}
  Iterable<User> findByUsername(@Param("username") String username);


  //Accessible via http://127.0.0.1:8080/api/users/search/findByLastName?lastName={lastName}
  Iterable<User> findByLastName(@Param("lastName") String lastName);

  Iterable<User> findByFirstName(@Param("firstName") String firstName);

  //Accessible via http://127.0.0.1:8080/api/users/search/findByFirstNameAndLastName?firstName={firstName}&lastName={lastName}
  Iterable<User> findByFirstNameAndLastName(
      @Param("firstName") String firstName, @Param("lastName") String lastName);

  //Accessible via http://127.0.0.1:8080/api/users/search/findByHomeFacility?homeFacility=http://127.0.0.1:8080/api/facilities/{facilityId}
  Iterable<User> findByHomeFacility(@Param("homeFacility") Facility homeFacilityId);

  //Accessible via http://127.0.0.1:8080/api/users/search/findByActive?active={active}
  Iterable<User> findByActive(@Param("active") boolean active);

  //Accessible via http://127.0.0.1:8080/api/users/search/findByVerified?verified={verified}
  Iterable<User> findByVerified(@Param("verified") boolean verified);


  @Override
  @RestResource
  <S extends User> S save(S entity);

  @Override
  @RestResource
  <S extends User> Iterable<S> save(Iterable<S> entities);
}
