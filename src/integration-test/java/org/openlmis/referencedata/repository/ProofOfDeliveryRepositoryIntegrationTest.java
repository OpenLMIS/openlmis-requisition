package org.openlmis.referencedata.repository;


import org.junit.Assert;
import org.junit.Before;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

public class ProofOfDeliveryRepositoryIntegrationTest extends
    BaseCrudRepositoryIntegrationTest<ProofOfDelivery> {

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ReferenceDataService referenceDataService;

  private static final String CODE = "ProofOfDeliveryRepositoryIntegrationTest";

  private Order order = new Order();

  ProofOfDeliveryRepository getRepository() {
    return this.proofOfDeliveryRepository;
  }

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(CODE);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(CODE);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(CODE);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(CODE);
    facility.setName(CODE);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Program program = new Program();
    program.setCode(CODE);
    programRepository.save(program);

    UserDto[] allUsers = referenceDataService.findAllUsers();
    Assert.assertEquals(1, allUsers.length);
    UserDto user = allUsers[0];

    order.setOrderCode(CODE);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedById(user.getId());
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    orderRepository.save(order);
  }

  ProofOfDelivery generateInstance() {
    ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
    proofOfDelivery.setOrder(order);
    return proofOfDelivery;
  }
}
