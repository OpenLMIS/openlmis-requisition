package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class FacilityRepositoryIT extends BaseCrudRepositoryIT<Facility> {

    @Autowired
    FacilityRepository repository;

    FacilityRepository getRepository() {
        return this.repository;
    }

    private FacilityType facilityType = new FacilityType();
    private GeographicZone geographicZone = new GeographicZone();

    @Before
    public void setUp() {
        this.facilityType.setCode("FacilityRepositoryIT");
        GeographicLevel level = new GeographicLevel();
        level.setCode("FacilityRepositoryIT");
        level.setLevelNumber(1);
        this.geographicZone.setCode("FacilityRepositoryIT");
        this.geographicZone.setLevel(level);
    }

    Facility generateInstance() {
        int instanceNumber = this.getNextInstanceNumber();
        Facility facility = new Facility();
        facility.setType(this.facilityType);
        facility.setGeographicZone(this.geographicZone);
        facility.setCode("F" + instanceNumber);
        facility.setName("Facility #" + instanceNumber);
        facility.setDescription("Test facility");
        facility.setActive(true);
        facility.setEnabled(true);
        return facility;
    }
}
