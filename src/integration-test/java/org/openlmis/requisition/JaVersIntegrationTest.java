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

package org.openlmis.requisition;

import static java.time.temporal.ChronoUnit.SECONDS;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.TimeZone;
import org.javers.core.Javers;
import org.javers.core.metamodel.object.CdoSnapshot;
import org.javers.repository.jql.QueryBuilder;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.dto.CodeDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@ActiveProfiles("test")
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class JaVersIntegrationTest {

  @Autowired
  private Javers javers;

  private static TimeZone defaultZone;
  private static final String COMMIT_AUTHOR = "author";

  @BeforeClass
  public static void beforeClass() {
    defaultZone = TimeZone.getDefault();
  }

  @After
  public void after() {
    TimeZone.setDefault(defaultZone);
  }

  @Test
  public void shouldAlwaysCommitWithUtcTimeZone() throws IOException {

    //given
    CodeDto code = new CodeDto();
    code.setCode("code_1");

    //when
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    javers.commit(COMMIT_AUTHOR, code);

    TimeZone.setDefault(TimeZone.getTimeZone("Africa/Johannesburg"));
    code.setCode("code_2");
    javers.commit(COMMIT_AUTHOR, code);

    //then
    List<CdoSnapshot> snapshots = javers.findSnapshots(QueryBuilder.byClass(CodeDto.class).build());

    assertEquals(2, snapshots.size());

    LocalDateTime commitTime1 = snapshots.get(0).getCommitMetadata().getCommitDate();
    LocalDateTime commitTime2 = snapshots.get(1).getCommitMetadata().getCommitDate();

    long delta = SECONDS.between(commitTime1, commitTime2);
    assertThat(delta, lessThan(1L));
  }
}
