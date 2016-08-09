package org.openlmis.hierarchyandsupervision.repository.custom;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.Facility;

import java.util.List;

public interface UserRepositoryCustom {

  List<User> searchUsers(
          String username, String firstName, String lastName,
          Facility homeFacility, Boolean active, Boolean verified);
}
