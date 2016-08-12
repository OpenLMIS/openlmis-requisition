package org.openlmis.referencedata.utils;

import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupRepository;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.repository.CommentRepository;
import org.openlmis.referencedata.repository.FacilityOperatorRepository;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeApprovedProductRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramProductRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.referencedata.repository.StockRepository;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;

@Component
public class CleanRepositoryHelper {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private ProgramProductRepository programProductRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityOperatorRepository facilityOperatorRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityTypeApprovedProductRepository facilityTypeApprovedProductRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

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
  private RequisitionGroupRepository requisitionGroupRepository;

  @Autowired
  private RequisitionGroupProgramScheduleRepository requisitionGroupProgramScheduleRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

  @Autowired
  private TemplateRepository templateRepository;

  /**
   * Delete all entities from most of repositories.
   */
  @Transactional
  public void cleanAll() {
    templateParameterRepository.deleteAll();
    templateRepository.deleteAll();
    stockRepository.deleteAll();
    requisitionGroupRepository.deleteAll();
    requisitionGroupProgramScheduleRepository.deleteAll();
    supplyLineRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    proofOfDeliveryLineRepository.deleteAll();
    proofOfDeliveryRepository.deleteAll();
    orderLineRepository.deleteAll();
    orderRepository.deleteAll();
    commentRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    facilityTypeApprovedProductRepository.deleteAll();
    programProductRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityOperatorRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();
    configurationSettingRepository.deleteAll();
    for (User user : userRepository.findAll()) {
      if (!user.getUsername().equals("admin")) {
        userRepository.delete(user);
      }
    }
  }
}
