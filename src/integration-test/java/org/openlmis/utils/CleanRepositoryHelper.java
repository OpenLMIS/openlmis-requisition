package org.openlmis.utils;

import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupRepository;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.FacilityOperatorRepository;
import org.openlmis.requisition.repository.FacilityRepository;
import org.openlmis.requisition.repository.FacilityTypeApprovedProductRepository;
import org.openlmis.requisition.repository.FacilityTypeRepository;
import org.openlmis.requisition.repository.GeographicLevelRepository;
import org.openlmis.requisition.repository.GeographicZoneRepository;
import org.openlmis.requisition.repository.ProcessingPeriodRepository;
import org.openlmis.requisition.repository.ProgramProductRepository;
import org.openlmis.requisition.repository.ProgramRepository;
import org.openlmis.requisition.repository.ProcessingScheduleRepository;
import org.openlmis.requisition.repository.StockRepository;
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
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

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
  private StockRepository stockRepository;

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

  @Autowired
  private TemplateRepository templateRepository;

  @Autowired
  private RoleRepository roleRepository;

  @Autowired
  private RightRepository rightRepository;

  @Autowired
  private ReferenceDataService referenceDataService;

  /**
   * Delete all entities from most of repositories.
   */
  @Transactional
  public void cleanAll() {
    requisitionGroupRepository.deleteAll();
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
    geographicZoneRepository.deleteAll();
    facilityOperatorRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    roleRepository.deleteAll();
    rightRepository.deleteAll();
  }

  private void deleteAllUsersExceptAdmin() {
    UserDto initialUser = referenceDataService.findOneUser(INITIAL_USER_ID);
    initialUser.setHomeFacility(null);
    initialUser.setSupervisedNode(null);
    referenceDataService.saveUser(initialUser);
    for (UserDto user : referenceDataService.findAllUsers()) {
      if (!user.getId().equals(INITIAL_USER_ID)) {
        referenceDataService.deleteUser(user.getId());
      }
    }
  }
}
