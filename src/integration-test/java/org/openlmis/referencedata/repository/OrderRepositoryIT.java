package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Order;
import org.openlmis.referencedata.domain.OrderStatus;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.math.BigDecimal;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class OrderRepositoryIT extends BaseCrudRepositoryIT<Order> {

    @Autowired
    OrderRepository repository;

    @Autowired
    ProgramRepository programRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    FacilityRepository facilityRepository;

    OrderRepository getRepository() {
        return this.repository;
    }

    private Facility facility = new Facility();
    private Program program = new Program();
    private User user = new User();

    @Before
    public void setUp() {
        facilityRepository.deleteAll();
        FacilityType facilityType = new FacilityType();
        facilityType.setCode("OrderRepositoryIT");

        GeographicLevel level = new GeographicLevel();
        level.setCode("OrderRepositoryIT");
        level.setLevelNumber(1);

        GeographicZone geographicZone = new GeographicZone();
        geographicZone.setCode("OrderRepositoryIT");
        geographicZone.setLevel(level);

        facility.setType(facilityType);
        facility.setGeographicZone(geographicZone);
        facility.setCode("OrderRepositoryIT");
        facility.setName("OrderRepositoryIT");
        facility.setDescription("Test facility");
        facility.setActive(true);
        facility.setEnabled(true);
        facilityRepository.save(facility);

        programRepository.deleteAll();
        program.setCode("OrderRepositoryIT");
        programRepository.save(program);

        userRepository.deleteAll();
        user.setUsername("OrderRepositoryIT");
        user.setPassword("OrderRepositoryIT");
        user.setFirstName("Test");
        user.setLastName("User");
        userRepository.save(user);
    }

    Order generateInstance() {
        int instanceNumber = this.getNextInstanceNumber();
        Order order = new Order();
        order.setOrderCode("O" + instanceNumber);
        order.setQuotedCost(new BigDecimal(1.29));
        order.setStatus(OrderStatus.PICKING);
        order.setProgram(program);
        order.setCreatedBy(user);
        order.setRequestingFacility(facility);
        order.setReceivingFacility(facility);
        order.setSupplyingFacility(facility);
        return order;
    }
}
