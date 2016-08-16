package org.openlmis.referencedata.service;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SupplyLineService {

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  /**
   * Method returns all Supply Lines with matched parameters.
   * @param program program of searched Supply Lines.
   * @param supervisoryNode supervisoryNode of searched Supply Lines.
   * @return list of Supply Lines with matched parameters.
   */
  public List<SupplyLine> searchSupplyLines(Program program, SupervisoryNode supervisoryNode) {
    return supplyLineRepository.searchSupplyLines(program, supervisoryNode);
  }
}
