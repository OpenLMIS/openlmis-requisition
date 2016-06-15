package org.openlmis.referencedata.repository;

import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.io.Serializable;

/*
    The ReferenceDataRepository exists as a convenient way to expose all read-only members of
    PagingAndSortingRepository and CrudRepository.
 */
@NoRepositoryBean
public interface ReferenceDataRepository<T, IDT extends Serializable>
    extends PagingAndSortingRepository<T, IDT> {
  @Override
  @RestResource(exported = false)
  void delete(T entity);

  @Override
  @RestResource(exported = false)
  void delete(IDT id);

  @Override
  @RestResource(exported = false)
  void delete(Iterable<? extends T> entities);

  @Override
  @RestResource(exported = false)
  void deleteAll();

  @Override
  @RestResource(exported = false)
  <S extends T> S save(S entity);

  @Override
  @RestResource(exported = false)
  <S extends T> Iterable<S> save(Iterable<S> entities);
}
