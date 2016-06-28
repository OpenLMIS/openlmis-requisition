package org.openlmis.referencedata.domain.projection;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.User;
import org.springframework.data.rest.core.config.Projection;

@Projection(name="UserWithHomeFacility" , types = User.class)
public interface UserWithHomeFacilityProjection
{
    String getUsername();
    String getFirstName();
    String getLastName();
    Facility getHomeFacility();
    Boolean getVerified();
    Boolean getActive();
}
