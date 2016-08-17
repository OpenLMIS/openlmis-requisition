package org.openlmis.hierarchyandsupervision.repository.custom;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;

import java.util.List;

public interface SupplyLineRepositoryCustom {

  List<SupplyLine> searchSupplyLines(Program program, SupervisoryNode supervisoryNode);
}
