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

import static java.util.UUID.fromString;
import static org.junit.Assert.assertNotEquals;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import org.javers.core.Javers;
import org.javers.core.metamodel.object.CdoSnapshot;
import org.javers.repository.jql.QueryBuilder;
import org.junit.Before;
import org.junit.Test;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.ColumnType;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.dto.CodeDto;
import org.openlmis.requisition.repository.AvailableRequisitionColumnOptionRepository;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AuditLogInitializerIntegrationTest extends BaseAuditLogInitializerIntegrationTest {

  private List<CdoSnapshot> snapshots;
  private Set<AvailableRequisitionColumnOption> options  = new HashSet<>();
  private HashMap<String, RequisitionTemplateColumn> columns;
  private String name = "Default Name";

  private static final String templateId = "'460d7666-aac2-11e8-98d0-529269fb1459'";
  private final String requisitionTemplateId = "460d7666-aac2-11e8-98d0-529269fb1459";

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Autowired
  private AvailableRequisitionColumnOptionRepository availableRequisitionColumnOptionRepository;

  private static final String INSERT_REQ_SQL = "INSERT INTO "
      + "requisition.requisitions "
      + "(id, createddate, modifieddate, draftstatusmessage, "
      + "emergency, facilityid, numberofmonthsinperiod, "
      + "processingperiodid, programid, status, supervisorynodeid, "
      + "supplyingfacilityid, templateid, datephysicalstockcountcompleted, "
      + "version, reportonly) \n"
      + " VALUES \n"
      + "('460d6554-aac2-11e8-98d0-529269fb1459',"
      + "'2018-08-28 14:00:00', '2018-08-28 14:06:00',"
      + "'draft-status', false, "
      + "'460d68f6-aac2-11e8-98d0-529269fb1459',"
      + "1, '460d6dd8-aac2-11e8-98d0-529269fb1459',"
      + "'460d7008-aac2-11e8-98d0-529269fb1459', "
      + "'status', '460d726a-aac2-11e8-98d0-529269fb1459',"
      + "'460d7472-aac2-11e8-98d0-529269fb1459', "
      + templateId
      + ", '2018-08-23', 7784847621901938732, false)";

  @PersistenceContext
  private EntityManager entityManager;

  @Autowired
  private Javers javers;

  @Before
  public void createQuery() {
    addRequisitionTemplate();
    StringBuilder builder = new StringBuilder(INSERT_REQ_SQL);

    entityManager.createNativeQuery(builder.toString()).executeUpdate();
  }

  @Transactional
  private RequisitionTemplate addRequisitionTemplate() {

    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder().withColumnDefinition(getAvailableRequisitionColumn()).build();
    columns.put(name, column);

    RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
        .withoutId()
        .withColumns(columns)
        .build();

    requisitionTemplate.setId(fromString(requisitionTemplateId));
    requisitionTemplateRepository.save(requisitionTemplate);
    entityManager.flush();
    return requisitionTemplateRepository.findOne(fromString(requisitionTemplateId));
  }

  @Transactional
  private AvailableRequisitionColumn getAvailableRequisitionColumn () {
    AvailableRequisitionColumn availableRequisitionColumn = new AvailableRequisitionColumnDataBuilder()
        .build();

    AvailableRequisitionColumnOption option = new AvailableRequisitionColumnOption(availableRequisitionColumn, "default", "Default");
    options.add(option);

    availableRequisitionColumn.setOptions(options);
    availableRequisitionColumn.setColumnType(ColumnType.TEXT);
    availableRequisitionColumnRepository.save(availableRequisitionColumn);
    entityManager.flush();

    return availableRequisitionColumnRepository.findOne(availableRequisitionColumn.getId());
  }

  @Test
  public void shouldCreateSnapshots() throws IOException {
    snapshots = javers.findSnapshots(QueryBuilder.byClass(CodeDto.class).build());
    assertNotEquals(0, snapshots.size());
  }
}
