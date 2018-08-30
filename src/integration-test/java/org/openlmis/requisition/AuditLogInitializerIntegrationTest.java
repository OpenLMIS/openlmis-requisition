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

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import org.apache.commons.lang.StringUtils;
import org.assertj.core.util.Maps;
import org.javers.core.Javers;
import org.javers.core.metamodel.object.CdoSnapshot;
import org.javers.core.metamodel.object.GlobalId;
import org.javers.core.metamodel.object.InstanceId;
import org.javers.repository.jql.QueryBuilder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@ActiveProfiles("test")
@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class AuditLogInitializerIntegrationTest {

  private static final String[] REQUISITION_FIELDS = {
      "id", "createddate", "modifieddate", "draftstatusmessage", "emergency", "facilityid",
      "numberofmonthsinperiod", "processingperiodid", "programid", "status", "supervisorynodeid",
      "supplyingfacilityid", "templateid", "datephysicalstockcountcompleted", "version",
      "reportonly"
  };

  private static final String INSERT_SQL = String.format(
      "INSERT INTO requisition.requisitions (%s) VALUES (%s);",
      StringUtils.join(REQUISITION_FIELDS, ", "),
      StringUtils.repeat("?", ", ", REQUISITION_FIELDS.length)
  );

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Autowired
  private Javers javers;

  @Autowired
  private ApplicationContext applicationContext;

  @PersistenceContext
  private EntityManager entityManager;

  @Test
  public void shouldCreateSnapshots() {
    // given
    UUID requisitionId = UUID.randomUUID();

    AvailableRequisitionColumn availableRequisitionColumn = addAvailableRequisitionColumn();
    RequisitionTemplate requisitionTemplate = addRequisitionTemplate(availableRequisitionColumn);
    addRequisition(requisitionId, requisitionTemplate.getId());

    // when
    QueryBuilder jqlQuery = QueryBuilder.byInstanceId(requisitionId, Requisition.class);
    List<CdoSnapshot> snapshots = javers.findSnapshots(jqlQuery.build());

    assertThat(snapshots, hasSize(0));

    AuditLogInitializer auditLogInitializer = new AuditLogInitializer(applicationContext, javers);
    auditLogInitializer.run();

    snapshots = javers.findSnapshots(jqlQuery.build());

    // then
    assertThat(snapshots, hasSize(1));

    CdoSnapshot snapshot = snapshots.get(0);
    GlobalId globalId = snapshot.getGlobalId();

    assertThat(globalId, is(notNullValue()));
    assertThat(globalId, instanceOf(InstanceId.class));

    InstanceId instanceId = (InstanceId) globalId;
    assertThat(instanceId.getCdoId(), is(requisitionId));
    assertThat(instanceId.getTypeName(), is("Requisition"));
  }

  private AvailableRequisitionColumn addAvailableRequisitionColumn () {
    AvailableRequisitionColumn availableRequisitionColumn =
        new AvailableRequisitionColumnDataBuilder()
            .withoutId()
            .withoutOptions()
            .build();

    return availableRequisitionColumnRepository.save(availableRequisitionColumn);
  }

  private RequisitionTemplate addRequisitionTemplate(AvailableRequisitionColumn columnDefinition) {
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName("column")
        .withColumnDefinition(columnDefinition)
        .withoutOption()
        .build();

    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(Maps.newHashMap(column.getName(), column))
        .build();

    return requisitionTemplateRepository.save(requisitionTemplate);
  }

  private void addRequisition(UUID requisitionId, UUID requisitionTemplateId) {
    entityManager.flush();
    entityManager
        .createNativeQuery(INSERT_SQL)
        .setParameter(1, requisitionId) // id
        .setParameter(2, ZonedDateTime.now()) // createdDate
        .setParameter(3, ZonedDateTime.now()) // modifiedDate
        .setParameter(4, "") // draftStatusMessage
        .setParameter(5, false) // emergency
        .setParameter(6, UUID.randomUUID()) // facilityId
        .setParameter(7, 1) // numberOfMonthsInPeriod
        .setParameter(8, UUID.randomUUID()) // processingPeriodId
        .setParameter(9, UUID.randomUUID()) // programId
        .setParameter(10, RequisitionStatus.RELEASED.name()) // status
        .setParameter(11, UUID.randomUUID()) // supervisoryNodeId
        .setParameter(12, UUID.randomUUID()) // supplyingFacilityId
        .setParameter(13, requisitionTemplateId) // templateId
        .setParameter(14, LocalDate.now()) // datePhysicalStockCountCompleted
        .setParameter(15, 10) // version
        .setParameter(16, false) // reportOnly
        .executeUpdate();
  }
}
