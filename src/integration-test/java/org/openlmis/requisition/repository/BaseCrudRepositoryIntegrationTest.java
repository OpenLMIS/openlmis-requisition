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

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.domain.BaseEntity;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public abstract class BaseCrudRepositoryIntegrationTest<T extends BaseEntity> {

  abstract CrudRepository<T, UUID> getRepository();

  /*
   * Generate a unique instance of given type.
   * @return generated instance
   */
  abstract T generateInstance();

  private AtomicInteger instanceNumber = new AtomicInteger(0);

  int getNextInstanceNumber() {
    return this.instanceNumber.incrementAndGet();
  }

  protected void assertInstance(T instance) {
    Assert.assertNotNull(instance.getId());
  }

  @Test
  public void testCreate() {
    CrudRepository<T, UUID> repository = this.getRepository();

    T instance = this.generateInstance();
    Assert.assertNull(instance.getId());

    instance = repository.save(instance);
    assertInstance(instance);

    Assert.assertTrue(repository.existsById(instance.getId()));
  }

  @Test
  public void testFindOne() {
    CrudRepository<T, UUID> repository = this.getRepository();

    T instance = this.generateInstance();

    instance = repository.save(instance);
    assertInstance(instance);

    UUID id = instance.getId();

    instance = repository.findById(id).orElse(null);
    assertInstance(instance);
    Assert.assertEquals(id, instance.getId());
  }

  @Test
  public void testDelete() {
    CrudRepository<T, UUID> repository = this.getRepository();

    T instance = this.generateInstance();
    Assert.assertNotNull(instance);

    instance = repository.save(instance);
    assertInstance(instance);

    UUID id = instance.getId();

    repository.deleteById(id);
    Assert.assertFalse(repository.existsById(id));
  }

  protected Pageable createPageable(int pageSize, int pageNumber) {
    return PageRequest.of(pageNumber, pageSize);
  }
}
