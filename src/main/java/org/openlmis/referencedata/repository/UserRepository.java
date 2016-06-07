package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RestResource;


@RestResource(exported=true)
public interface UserRepository extends Repository<User, Integer>
{
    /*
        Generate and expose endpoints such as:

        http://127.0.0.1:8080/api/users/
        http://127.0.0.1:8080/api/users?page={number}&size={number}&sort={number}
    */
    Iterable<User> findAll(Sort sort);
    Page<User> findAll(Pageable pageable);


    //Accessible via http://127.0.0.1:8080/api/users/{id}
    User findOne(Integer id);

    //Accessible via http://127.0.0.1:8080/api/users/search/findByUsername?username={username}
    Iterable<User> findByUsername(@Param("username") String username);


    //Accessible via http://127.0.0.1:8080/api/users/search/findByLastName?lastName={lastName}
    Iterable<User> findByLastName(@Param("lastName") String lastName);
    Iterable<User> findByFirstName(@Param("firstName") String firstName);

    //Accessible via http://127.0.0.1:8080/api/users/search/findByFirstNameAndLastName?firstName={firstName}&lastName={lastName}
    Iterable<User> findByFirstNameAndLastName(@Param("firstName") String firstName, @Param("lastName") String lastName);

    //Accessible via http://127.0.0.1:8080/api/users/search/findByHomeFacility?homeFacility=http://127.0.0.1:8080/api/facilities/{facilityId}
    Iterable<User> findByHomeFacility(@Param("homeFacility") Facility homeFacilityId);

    //Accessible via http://127.0.0.1:8080/api/users/search/findByActive?active={active}
    Iterable<User> findByActive(@Param("active") boolean active);

    //Accessible via http://127.0.0.1:8080/api/users/search/findByVerified?verified={verified}
    Iterable<User> findByVerified(@Param("verified") boolean verified);

    //Generate JPA code to save a user, but don't expose an endpoint for it
    @RestResource(exported=false)
    User save(User entity);
}
