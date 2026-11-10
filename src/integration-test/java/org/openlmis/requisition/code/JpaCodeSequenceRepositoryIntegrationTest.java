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

package org.openlmis.requisition.code;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.UUID;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.IllegalTransactionStateException;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * Runs against the real database without a test-managed transaction, because the behaviour under
 * test depends on transactions being committed or rolled back. Every test uses its own keys,
 * which are removed in {@link #cleanUp()}.
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@ActiveProfiles("test")
public class JpaCodeSequenceRepositoryIntegrationTest {

  @Autowired
  private CodeSequenceRepository sequences;

  @Autowired
  private PlatformTransactionManager transactionManager;

  @Autowired
  private JdbcTemplate jdbcTemplate;

  private TransactionTemplate transaction;
  private String keyPrefix;
  private String key;

  @Before
  public void setUp() {
    transaction = new TransactionTemplate(transactionManager);
    keyPrefix = "it-" + UUID.randomUUID() + "|";
    key = keyPrefix + "facility=A";
  }

  @After
  public void cleanUp() {
    jdbcTemplate.update("DELETE FROM requisition.code_sequences WHERE scopekey LIKE ?",
        keyPrefix + "%");
  }

  @Test
  public void shouldStartFromOneAndIncrementOnEveryCall() {
    assertThat(nextInTransaction(key)).isEqualTo(1);
    assertThat(nextInTransaction(key)).isEqualTo(2);
    assertThat(nextInTransaction(key)).isEqualTo(3);
  }

  @Test
  public void shouldKeepSeparateCountersForDifferentKeys() {
    String second = keyPrefix + "facility=B";

    assertThat(nextInTransaction(key)).isEqualTo(1);
    assertThat(nextInTransaction(key)).isEqualTo(2);
    assertThat(nextInTransaction(second)).isEqualTo(1);
  }

  @Test
  public void shouldNotConsumeValueClaimedInRolledBackTransaction() {
    assertThat(nextInTransaction(key)).isEqualTo(1);

    Integer rolledBack = transaction.execute(status -> {
      status.setRollbackOnly();
      return sequences.next(key);
    });

    assertThat(rolledBack).isEqualTo(2);
    assertThat(nextInTransaction(key)).isEqualTo(2);
  }

  @Test
  public void shouldNotKeepCounterCreatedInRolledBackTransaction() {
    transaction.execute(status -> {
      status.setRollbackOnly();
      return sequences.next(key);
    });

    assertThat(nextInTransaction(key)).isEqualTo(1);
  }

  @Test
  public void shouldRequireSurroundingTransaction() {
    assertThatThrownBy(() -> sequences.next(key))
        .isInstanceOf(IllegalTransactionStateException.class);
  }

  private int nextInTransaction(String key) {
    return transaction.execute(status -> sequences.next(key));
  }
}
