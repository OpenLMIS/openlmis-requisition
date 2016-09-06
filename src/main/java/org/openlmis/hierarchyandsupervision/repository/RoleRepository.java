package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.referencedata.repository.ReferenceDataRepository;

import java.util.UUID;

public interface RoleRepository extends ReferenceDataRepository<Role, UUID> {
}
