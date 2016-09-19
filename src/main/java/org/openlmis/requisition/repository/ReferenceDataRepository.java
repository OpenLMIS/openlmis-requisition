package org.openlmis.requisition.repository;

import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.io.Serializable;

/*
    The ReferenceDataRepository exists as a convenient way to expose all read-only members of
    PagingAndSortingRepository and CrudRepository.
 */
@NoRepositoryBean
public interface ReferenceDataRepository<T, IDT extends Serializable>
    extends PagingAndSortingRepository<T, IDT> {
  @Override
  void delete(T entity);

  @Override
  void delete(IDT id);

  @Override
  void delete(Iterable<? extends T> entities);

  @Override
  void deleteAll();

  @Override
  <S extends T> S save(S entity);

  @Override
  <S extends T> Iterable<S> save(Iterable<S> entities);
}
