package org.openlmis.utils;

import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class CleanRepositoryHelper {

  public static final UUID INITIAL_USER_ID =
      UUID.fromString("35316636-6264-6331-2d34-3933322d3462");

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

  @Autowired
  private TemplateRepository templateRepository;

  @Autowired
  private UserReferenceDataService referenceDataService;


  /**
   * Delete all entities from most of repositories.
   */
  @Transactional
  public void cleanAll() {
    templateParameterRepository.deleteAll();
    templateRepository.deleteAll();
    proofOfDeliveryLineRepository.deleteAll();
    proofOfDeliveryRepository.deleteAll();
    configurationSettingRepository.deleteAll();
    commentRepository.deleteAll();
    orderLineRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    requisitionRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    orderRepository.deleteAll();
  }
}
