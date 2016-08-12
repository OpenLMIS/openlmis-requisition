package org.openlmis.referencedata.repository.custom;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;

import java.util.List;

public interface SupplyLineRepositoryCustom {

  List<SupplyLine> searchSupplyLines(Program program, SupervisoryNode supervisoryNode);
}
