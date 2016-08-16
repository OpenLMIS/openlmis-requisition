package org.openlmis.referencedata.repository;


import org.junit.Assert;
import org.junit.Before;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
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
  private UserRepository userRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  private String orderProofOfDeliveryString = "OrderProofOfDeliveryRepositoryIntegrationTest";

  private Order order = new Order();

  ProofOfDeliveryRepository getRepository() {
    return this.proofOfDeliveryRepository;
  }

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(orderProofOfDeliveryString);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(orderProofOfDeliveryString);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(orderProofOfDeliveryString);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(orderProofOfDeliveryString);
    facility.setName(orderProofOfDeliveryString);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Program program = new Program();
    program.setCode(orderProofOfDeliveryString);
    programRepository.save(program);

    Assert.assertEquals(1, userRepository.count());
    User user = userRepository.findAll().iterator().next();

    order.setOrderCode(orderProofOfDeliveryString);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
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
