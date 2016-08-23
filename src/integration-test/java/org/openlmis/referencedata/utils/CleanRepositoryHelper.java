package org.openlmis.referencedata.utils;

import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupRepository;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.requisition.repository.CommentRepository;
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
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
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

  @Autowired
  private RoleRepository roleRepository;

  @Autowired
  private RightRepository rightRepository;

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
    facilityTypeApprovedProductRepository.deleteAll();
    commentRepository.deleteAll();
    orderLineRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    stockRepository.deleteAll();
    programProductRepository.deleteAll();
    requisitionRepository.deleteAll();
    requisitionGroupProgramScheduleRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    supplyLineRepository.deleteAll();
    orderRepository.deleteAll();
    productRepository.deleteAll();
    periodRepository.deleteAll();
    programRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    deleteAllUsersExceptAdmin();
    productCategoryRepository.deleteAll();
    scheduleRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    requisitionGroupRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    facilityOperatorRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    roleRepository.deleteAll();
    rightRepository.deleteAll();
  }

  private void deleteAllUsersExceptAdmin() {
    User initialUser = userRepository.findOne(INITIAL_USER_ID);
    initialUser.setHomeFacility(null);
    initialUser.setSupervisedNode(null);
    userRepository.save(initialUser);
    for (User user : userRepository.findAll()) {
      if (!user.getId().equals(INITIAL_USER_ID)) {
        userRepository.delete(user);
      }
    }
  }
}
