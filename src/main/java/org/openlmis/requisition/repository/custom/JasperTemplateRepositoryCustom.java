package org.openlmis.requisition.repository.custom;

import java.util.List;

public interface JasperTemplateRepositoryCustom {

  List<String> runArbitrarySql(String arbitrarySql);
}
