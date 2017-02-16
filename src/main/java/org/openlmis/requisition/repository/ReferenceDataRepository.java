/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

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
