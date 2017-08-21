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

import java.io.IOException;
import java.nio.charset.Charset;
import java.sql.ResultSet;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.dto.RequisitionPermissionDto;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.core.annotation.Order;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;

/**
 * RightAssignmentInitializer runs after its associated Spring application has loaded. It 
 * automatically re-generates right assignments into the database, after dropping the existing 
 * right assignments. This component only runs when the "refresh-db" Spring profile is set.
 */
@Component
@Profile("refresh-db")
@Order(10)
public class RequisitionPermissionStringInitializer implements CommandLineRunner {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(
      RequisitionPermissionStringInitializer.class);

  private static final String REQ_PERMS_PATH = "classpath:db/refresh/";

  static final String DELETE_SQL = "DELETE FROM requisition.requisition_permission_strings;";

  @Value(value = REQ_PERMS_PATH + "get_requisition_permissions.sql")
  private Resource requisitionPermissionsResource;

  @Autowired
  JdbcTemplate template;

  /**
   * Re-generates right assignments.
   * @param args command line arguments
   */
  public void run(String... args) throws IOException {
    XLOGGER.entry();

    // Drop existing rows; we are regenerating from scratch
    XLOGGER.debug("Drop existing requisition permission strings");
    template.update(DELETE_SQL);

    // Get a right assignment matrix from database
    XLOGGER.debug("Get requisition permissions from db");
    List<RequisitionPermissionDto> requisitionPermissions = getRequisitionPermissionsFromDbResource(
        requisitionPermissionsResource);

    // Convert set of right assignments to insert to a set of SQL inserts
    XLOGGER.debug("Convert requisition permissions to SQL inserts");
    List<String> requisitionPermissionSqlInserts = requisitionPermissions.stream()
        .map(this::convertRequisitionPermissionToSqlInsertString)
        .collect(Collectors.toList());

    XLOGGER.debug("Perform SQL inserts");
    int[] updateCounts = template.batchUpdate(requisitionPermissionSqlInserts.toArray(
        new String[requisitionPermissionSqlInserts.size()]));

    XLOGGER.exit("Total db updates: " + Arrays.stream(updateCounts).sum());
  }

  List<RequisitionPermissionDto> getRequisitionPermissionsFromDbResource(Resource resource)
      throws IOException {
    return template.query(
        resourceToString(resource),
        (ResultSet rs, int rowNum) -> {

          RequisitionPermissionDto requisitionPermissionMap = new RequisitionPermissionDto();
          requisitionPermissionMap.setRequisitionId(UUID.fromString(
              rs.getString("requisitionid")));
          requisitionPermissionMap.setPermissionString(rs.getString("permissionstring"));
          return requisitionPermissionMap;
        }
    );
  }
  
  String convertRequisitionPermissionToSqlInsertString(RequisitionPermissionDto 
      requisitionPermissionDto) {
    String insertValues = String.join(",",
        surroundWithSingleQuotes(UUID.randomUUID().toString()),
        surroundWithSingleQuotes(requisitionPermissionDto.getRequisitionId().toString()),
        surroundWithSingleQuotes(requisitionPermissionDto.getPermissionString())
    );

    return "INSERT INTO requisition.requisition_permission_strings "
        + "(id, requisitionid, permissionstring) VALUES ("
        + insertValues
        + ");";
  }

  private String surroundWithSingleQuotes(String str) {
    return "'" + str + "'";
  }

  private String resourceToString(final Resource resource) throws IOException {
    XLOGGER.entry(resource.getDescription());
    String str = StreamUtils.copyToString(resource.getInputStream(), Charset.defaultCharset());
    XLOGGER.exit();
    return str;
  }
}